# Dynamic Multi-Tenant Auth Routing

Run EMQX 6.3.1 with two independent HTTPS authentication and authorization services. A username prefix selects the tenant service; the service validates the device and decides which topics it may use. EMQX supplies a different API key to each service without requiring devices to know those keys.

| MQTT username | Device password | HTTP service | Allowed topics |
|---|---|---|---|
| `north/device17` | `north-device-password` | `north.auth.example.com` | `north/#` |
| `south/device42` | `south-device-password` | `south.auth.example.com` | `south/#` |

The domains resolve inside the Compose network. Nothing needs to be added to your host's DNS or hosts file. Each service has its own device record and TLS certificate, signed by a generated demo CA.

## Run

Requires Docker with Docker Compose v2 or later. The first run downloads EMQX and builds the small Python service/test image. No local Python installation or MQTT client is required for the tests.

From this directory:

```sh
docker compose up -d --build --wait emqx
docker compose run --rm --no-deps tests
```

The test command exits nonzero on failure. It exercises real MQTT 5 connections and HTTPS requests. To see the routed requests:

```sh
docker compose logs north-auth south-auth
```

Verified on 2026-10-01 with `emqx/emqx-enterprise:6.3.1` on Linux/arm64: all 10 integration tests passed.

Each log entry identifies the tenant, endpoint, client, action, topic and decision. `api_key_valid` shows whether the correct service key arrived; the key and device password are omitted.

For manual MQTT testing, connect your client to `localhost:1883` with MQTT 5 and either login from the table. Subscribe to `north/#` and publish to `north/telemetry` as North; then try `south/telemetry`. Use QoS 1 to observe a rejected publish's PUBACK reason code `0x87` (Not authorized). A rejected subscription returns the same reason code in SUBACK.

If port 1883 is busy, prefix the start command with `MQTT_PORT=1884`. This changes only the host port; the container tests still use port 1883.

Stop the example and remove its generated certificates:

```sh
docker compose down -v
```

This removes this Compose project's containers, network and certificate volumes. Certificates expire after 30 days; the same cleanup command lets the next start generate fresh ones.

## Configuration

[`base.hocon`](base.hocon) contains the example's listener, client-attribute, authentication and authorization settings. Compose mounts it at `/opt/emqx/etc/base.hocon` and keeps the image's default `emqx.conf`. The demo node name and cookie are supplied through environment variables.

The HTTP authenticator and authorization source both use:

```hocon
url = "https://${client_attrs.tenant}.auth.example.com/authn"
hostname_resolution = dynamic
allowed_hosts = ["north.auth.example.com", "south.auth.example.com"]
headers {
  "Content-Type" = "application/json"
  "X-API-Key" = "${client_attrs.auth_token}"
}
```

The authorization URL ends in `/authz`. The explicit allow list restricts requests to the two provisioned tenant services. An unlisted hostname such as `west.auth.example.com` is rejected before HTTP, even when its DNS and TLS work. The tests verify this boundary.

TLS peer verification is enabled and uses the generated CA. EMQX extracts `tenant` from the username and evaluates `getenv()` to populate `auth_token` from `EMQXVAR_north_auth_token` or `EMQXVAR_south_auth_token`. Both expressions extract the username prefix independently: one initializer cannot read an attribute created by another.

[`auth_server.py`](auth_server.py) checks the broker's API key on both endpoints. `/authn` also checks the full username and device password, returning `is_superuser: false` on success. `/authz` checks the authenticated username, action and tenant topic prefix. Explicit decisions use HTTP 200 with a JSON `result` of `allow` or `deny`.

**EMQX 6.3.0 and 6.3.1 compatibility:** send the response header as lowercase `content-type: application/json`. In these versions, the dynamic HTTP path preserves response-header casing but the authentication parser looks up `content-type` in lowercase. The service uses lowercase explicitly.

The custom `tenant` attribute routes requests. It does not assign an EMQX namespace (`tns`) or add a topic mountpoint. The backend's topic policy provides the isolation demonstrated here.

## What the tests verify

[`test_routing.py`](test_routing.py) checks:

- Both tenants connect through their own service, with the correct broker API key.
- Wrong passwords, credentials from the other tenant, and missing or malformed identities are rejected.
- An unknown `west` tenant is blocked before HTTP. Its hostname deliberately has working DNS and TLS, so the test exercises `allowed_hosts`.
- Both tenants can publish, subscribe and receive messages within their own topic prefix.
- Cross-tenant publishes return a denied PUBACK and deliver no message to the other tenant.
- Cross-tenant, global wildcard, misleading-prefix and system-topic subscriptions are denied.
- Both HTTP endpoints reject a missing key or the other tenant's key.
- Rejected broker API keys and HTTP 503 responses deny new connections and new topic operations on existing connections. The other tenant keeps working; access recovers when the failure is cleared.

Fault injection and redacted request history use `/__test__/faults` and `/__test__/events`. These helper endpoints require the service key and are enabled only by `ENABLE_TEST_API=true`. They exist for this example's automated tests.

## Deployment notes

All credentials in Compose are public demo values. The mock service stores one plaintext device password per tenant to make the exchange easy to inspect. Replace it with your identity service for a real deployment.

Only the MQTT port is published, on the host loopback interface. MQTT is unencrypted in this local demo; broker-to-service traffic uses HTTPS. Auth services and their test endpoints have no published host ports. TLS private keys stay in their respective Docker volumes; the broker mounts only the public CA volume.

EMQX runs in `ESSENTIAL` mode with the hardened security profile. Authentication and authorization are available without the Dashboard. The example disables authentication and authorization caches so every tested operation reaches the backend. Choose cache settings for the policy-update and load requirements of your deployment.

`getenv()` caches values after their first read. Recreate the broker with the new environment after changing a tenant service key.

For another tenant, add its service and certificate, allow its hostname in both HTTP configurations, and supply the corresponding `EMQXVAR_<tenant>_auth_token`. Restart/recreate the broker with those changes and test both allowed and denied operations.

Further reading: [HTTP authentication](https://docs.emqx.com/en/emqx/latest/guides/access-control/authn/http.html#configure-dynamic-hostname-resolution), [HTTP authorization](https://docs.emqx.com/en/emqx/latest/guides/access-control/authz/http.html#configure-dynamic-hostname-resolution), and [client attributes](https://docs.emqx.com/en/emqx/latest/develop/client-attributes/client-attributes.html).
