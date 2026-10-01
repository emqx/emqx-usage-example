"""Small tenant-owned HTTPS auth service with a single demo device record."""

import hmac
import json
import os
import ssl
import threading
from collections import deque
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

TENANT = os.environ["TENANT"]
USERNAME = os.environ["DEVICE_USERNAME"]
PASSWORD = os.environ["DEVICE_PASSWORD"]
API_KEY = os.environ["SERVICE_API_KEY"]
TEST_API = os.environ.get("ENABLE_TEST_API") == "true"
EVENTS = deque(maxlen=1000)
LOCK = threading.Lock()
FAULTS = {"unavailable": False, "reject_key": False}


class Handler(BaseHTTPRequestHandler):
    # HTTP/1.0 closes each connection, keeping this demo server simple.
    def log_message(self, *_args):
        pass  # Never log request bodies, passwords or API keys.

    def reply(self, status, body):
        data = json.dumps(body).encode()
        self.send_response(status)
        # EMQX 6.3.0's dynamic HTTP path looks up this response name in lowercase.
        self.send_header("content-type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def valid_key(self):
        return hmac.compare_digest(self.headers.get("X-API-Key", "").encode(), API_KEY.encode())

    def do_GET(self):
        if self.path == "/health":
            return self.reply(200, {"status": "ok", "tenant": TENANT})
        if TEST_API and self.path == "/__test__/events" and self.valid_key():
            with LOCK:
                events = list(EVENTS)
            return self.reply(200, events)
        self.reply(404, {})

    def do_POST(self):
        try:
            size = int(self.headers.get("Content-Length", "0"))
            if not 0 < size <= 16_384:
                raise ValueError("invalid size")
            data = json.loads(self.rfile.read(size))
            if not isinstance(data, dict):
                raise ValueError("object required")
        except (ValueError, json.JSONDecodeError):
            return self.reply(400, {"result": "deny"})

        # Test-only fault injection. No host port exposes this service.
        if TEST_API and self.path == "/__test__/faults" and self.valid_key():
            if not all(k in FAULTS and isinstance(v, bool) for k, v in data.items()):
                return self.reply(400, {})
            with LOCK:
                FAULTS.update(data)
            return self.reply(200, {"ok": True})

        if self.path not in ("/authn", "/authz"):
            return self.reply(404, {})

        with LOCK:
            faults = dict(FAULTS)
        key_ok = self.valid_key() and not faults["reject_key"]
        username_ok = data.get("username") == USERNAME
        allowed = False
        if key_ok and username_ok:
            if self.path == "/authn":
                password = data.get("password")
                allowed = isinstance(password, str) and hmac.compare_digest(password.encode(), PASSWORD.encode())
            else:
                topic = data.get("topic", "")
                allowed = (
                    data.get("action") in ("publish", "subscribe")
                    and isinstance(topic, str)
                    and topic.startswith(TENANT + "/")
                )

        status = 503 if faults["unavailable"] else 200
        result = "allow" if allowed and status == 200 else "deny"
        event = {
            "tenant": TENANT,
            "endpoint": self.path,
            "clientid": data.get("clientid"),
            "username": data.get("username"),
            "action": data.get("action"),
            "topic": data.get("topic"),
            "api_key_valid": key_ok,
            "status": status,
            "result": result,
        }
        with LOCK:
            EVENTS.append(event)
        print(json.dumps(event), flush=True)
        body = {"result": result}
        if self.path == "/authn" and result == "allow":
            body["is_superuser"] = False
        self.reply(status, body)


if __name__ == "__main__":
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.minimum_version = ssl.TLSVersion.TLSv1_2
    context.load_cert_chain("/tls/server.pem", "/tls/server.key")
    server = ThreadingHTTPServer(("0.0.0.0", 443), Handler)
    server.socket = context.wrap_socket(server.socket, server_side=True)
    print(f"{TENANT} auth service listening on HTTPS port 443", flush=True)
    server.serve_forever()
