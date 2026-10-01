"""Integration tests against EMQX 6.3.1, using real MQTT 5 packets and HTTPS."""

import json
import queue
import ssl
import unittest
import urllib.request
import uuid

import paho.mqtt.client as mqtt

TENANTS = {
    "north": ("north/device17", "north-device-password", "north-demo-service-key"),
    "south": ("south/device42", "south-device-password", "south-demo-service-key"),
}
TLS = ssl.create_default_context(cafile="/ca/auth-ca.pem")
TIMEOUT = 8


def http(tenant, path, body=None, key=None):
    headers = {"Content-Type": "application/json"}
    if key is not None:
        headers["X-API-Key"] = key
    request = urllib.request.Request(
        f"https://{tenant}.auth.example.com{path}",
        data=None if body is None else json.dumps(body).encode(),
        headers=headers,
    )
    with urllib.request.urlopen(request, context=TLS, timeout=TIMEOUT) as response:
        return json.load(response)


def admin(tenant, path, body=None):
    return http(tenant, "/__test__/" + path, body, TENANTS[tenant][2])


class Device:
    def __init__(self, username, password):
        self.id = "routing-test-" + uuid.uuid4().hex
        self.connacks = queue.Queue()
        self.subacks = queue.Queue()
        self.pubacks = queue.Queue()
        self.messages = queue.Queue()
        self.client = mqtt.Client(
            callback_api_version=mqtt.CallbackAPIVersion.VERSION2,
            client_id=self.id,
            protocol=mqtt.MQTTv5,
            reconnect_on_failure=False,
        )
        if username is not None:
            self.client.username_pw_set(username, password)
        self.client.on_connect = lambda c, u, f, r, p: self.connacks.put(r.value)
        self.client.on_subscribe = lambda c, u, m, r, p: self.subacks.put((m, [x.value for x in r]))
        self.client.on_publish = lambda c, u, m, r, p: self.pubacks.put((m, r.value))
        self.client.on_message = lambda c, u, m: self.messages.put((m.topic, m.payload))

    def connect(self):
        self.client.connect("emqx", 1883, keepalive=20)
        self.client.loop_start()
        return self.connacks.get(timeout=TIMEOUT)

    def subscribe(self, topic):
        rc, mid = self.client.subscribe(topic, qos=1)
        if rc != mqtt.MQTT_ERR_SUCCESS:
            raise AssertionError(f"SUBSCRIBE could not be sent: {rc}")
        ack_mid, codes = self.subacks.get(timeout=TIMEOUT)
        if ack_mid != mid:
            raise AssertionError("unexpected SUBACK packet identifier")
        return codes

    def publish(self, topic, payload):
        info = self.client.publish(topic, payload, qos=1)
        if info.rc != mqtt.MQTT_ERR_SUCCESS:
            raise AssertionError(f"PUBLISH could not be sent: {info.rc}")
        ack_mid, code = self.pubacks.get(timeout=TIMEOUT)
        if ack_mid != info.mid:
            raise AssertionError("unexpected PUBACK packet identifier")
        return code

    def close(self):
        self.client.disconnect()
        self.client.loop_stop()


class RoutingTests(unittest.TestCase):
    def setUp(self):
        for tenant in TENANTS:
            admin(tenant, "faults", {"unavailable": False, "reject_key": False})
        self.addCleanup(self.reset_faults)

    def reset_faults(self):
        for tenant in TENANTS:
            admin(tenant, "faults", {"unavailable": False, "reject_key": False})

    def device(self, tenant, *, username=None, password=None, allowed=True):
        user, secret, _ = TENANTS[tenant]
        return self.login(
            user if username is None else username,
            secret if password is None else password,
            allowed=allowed,
        )

    def login(self, username, password, *, allowed):
        device = Device(username, password)
        self.addCleanup(device.close)
        code = device.connect()
        if allowed:
            self.assertEqual(code, 0, f"{username}: unexpected CONNACK {code:#x}")
        else:
            self.assertGreaterEqual(code, 0x80, f"{username}: connection was accepted")
        return device

    def events(self, tenant, device, endpoint=None):
        return [
            event for event in admin(tenant, "events")
            if event["clientid"] == device.id
            and (endpoint is None or event["endpoint"] == endpoint)
        ]

    def test_both_tenants_route_with_their_own_service_key(self):
        for tenant, other in (("north", "south"), ("south", "north")):
            with self.subTest(tenant=tenant):
                device = self.device(tenant)
                events = self.events(tenant, device, "/authn")
                self.assertEqual(len(events), 1)
                self.assertTrue(events[0]["api_key_valid"])
                self.assertEqual(events[0]["result"], "allow")
                self.assertEqual(self.events(other, device), [])

    def test_bad_passwords_and_cross_tenant_credentials_are_rejected(self):
        for tenant, other in (("north", "south"), ("south", "north")):
            for password in ("wrong-password", TENANTS[other][1]):
                with self.subTest(tenant=tenant, case="password"):
                    device = self.device(tenant, password=password, allowed=False)
                    self.assertEqual(self.events(tenant, device)[0]["result"], "deny")
            foreign_device = TENANTS[other][0].split("/", 1)[1]
            device = self.device(
                tenant, username=f"{tenant}/{foreign_device}",
                password=TENANTS[other][1], allowed=False,
            )
            self.assertEqual(self.events(tenant, device)[0]["result"], "deny")

    def test_missing_or_malformed_identity_is_rejected(self):
        for username in (None, "", "device17", "north/unknown"):
            with self.subTest(username=username):
                self.login(username, "north-device-password", allowed=False)

    def test_unknown_tenant_is_blocked_before_http(self):
        # Prove DNS and TLS work for west, so either cannot explain the denial.
        self.assertEqual(http("west", "/health")["tenant"], "north")
        device = self.login("west/device17", "north-device-password", allowed=False)
        for tenant in TENANTS:
            self.assertEqual(self.events(tenant, device), [])

    def test_each_tenant_can_publish_and_receive_its_own_messages(self):
        for tenant in TENANTS:
            with self.subTest(tenant=tenant):
                subscriber = self.device(tenant)
                publisher = self.device(tenant)
                self.assertEqual(subscriber.subscribe(f"{tenant}/#"), [1])
                topic = f"{tenant}/telemetry/{uuid.uuid4().hex}"
                payload = f"hello from {tenant}".encode()
                self.assertEqual(publisher.publish(topic, payload), 0)
                self.assertEqual(subscriber.messages.get(timeout=TIMEOUT), (topic, payload))
                for device, action in ((subscriber, "subscribe"), (publisher, "publish")):
                    events = self.events(tenant, device, "/authz")
                    self.assertEqual(len(events), 1)
                    self.assertEqual(events[0]["action"], action)
                    self.assertTrue(events[0]["api_key_valid"])

    def test_cross_tenant_publish_is_denied_and_not_delivered(self):
        for tenant, other in (("north", "south"), ("south", "north")):
            with self.subTest(tenant=tenant):
                subscriber = self.device(other)
                publisher = self.device(tenant)
                self.assertEqual(subscriber.subscribe(f"{other}/#"), [1])
                self.assertEqual(publisher.publish(f"{other}/telemetry", "forbidden"), 0x87)
                with self.assertRaises(queue.Empty):
                    subscriber.messages.get(timeout=0.3)
                events = self.events(tenant, publisher, "/authz")
                self.assertEqual(events[0]["result"], "deny")
                self.assertEqual(self.events(other, publisher), [])

    def test_cross_tenant_and_broad_subscriptions_are_denied(self):
        for tenant, other in (("north", "south"), ("south", "north")):
            device = self.device(tenant)
            for topic in (f"{other}/#", "#", "+/#", f"{tenant}-other/#", "$SYS/#"):
                with self.subTest(tenant=tenant, topic=topic):
                    self.assertEqual(device.subscribe(topic), [0x87])
            events = self.events(tenant, device, "/authz")
            self.assertEqual(len(events), 5)
            self.assertTrue(all(event["result"] == "deny" for event in events))

    def test_http_services_require_their_own_api_key(self):
        for tenant, other in (("north", "south"), ("south", "north")):
            user, password, _ = TENANTS[tenant]
            body = {
                "username": user, "password": password, "clientid": "http-contract-test",
                "action": "publish", "topic": f"{tenant}/telemetry",
            }
            for key in (None, TENANTS[other][2]):
                for endpoint in ("/authn", "/authz"):
                    with self.subTest(tenant=tenant, endpoint=endpoint, missing=key is None):
                        self.assertEqual(http(tenant, endpoint, body, key)["result"], "deny")

    def assert_fault_is_contained(self, fault):
        for tenant, other in (("north", "south"), ("south", "north")):
            with self.subTest(tenant=tenant):
                connected = self.device(tenant)
                admin(tenant, "faults", {fault: True})
                try:
                    rejected = self.device(tenant, allowed=False)
                    self.assertEqual(connected.publish(f"{tenant}/during-failure", "blocked"), 0x87)
                    self.assertEqual(connected.subscribe(f"{tenant}/during-failure/#"), [0x87])
                    self.assertEqual(self.events(tenant, rejected)[0]["result"], "deny")
                    healthy = self.device(other)
                    self.assertEqual(healthy.subscribe(f"{other}/#"), [1])
                    topic = f"{other}/healthy"
                    self.assertEqual(healthy.publish(topic, "still working"), 0)
                    self.assertEqual(healthy.messages.get(timeout=TIMEOUT), (topic, b"still working"))
                finally:
                    admin(tenant, "faults", {fault: False})
                self.device(tenant)  # Recovery is verified too.

    def test_broker_service_key_mismatch_denies_connect_and_topic_operations(self):
        self.assert_fault_is_contained("reject_key")

    def test_http_503_denies_connect_and_topic_operations(self):
        self.assert_fault_is_contained("unavailable")


if __name__ == "__main__":
    unittest.main(verbosity=2)
