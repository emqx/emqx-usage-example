# EMQX Enterprise Client Attributes

This is a collection of scripts and configurations to demo EMQX Enterprise 5.8.1 for below use cases.

- Mutual TLS authentication.
- Allow clients which can compute password from username using a secret salt to login.
- Deny clients which do not have client ID matching the TLS certificate common name (CN).
- Use HTTP server for MQTT client authentication and client attributes injection.
- Client attributes extraction.
  - Method 1: HTTP authentication which returns client attributes.
  - Method 2: Client attribute directly extracted from client TLS certificate subject (DN, distinguished name).
- Inject client attributes as MQTT v5 message user property.

## Mutual TLS (mTLS) authentication

Only clients with a trusted TLS certificate can access EMQX.

The test client certificate has a DN as: `CN=demo-cn-1,OU=demo-ou-1,O=MyOrgName,L=Stockholm,ST=Stockholm,C=SE`

NOTE: this demo terminates TLS at EMQX. The alternative is to terminate TLS at load-balancer and enable proxy protocol in EMQX listener to receive certificate information.

Refer to: `config/listeners.hocon`

## Client info authentication

In this demo, EMQX is configured to authenticate clients at MQTT layer with 3 checks.

1. Allow clients which can compute password from username using a secret salt to login.
2. Quick deny if MQTT client's client ID does not match TLS certificate's common name.
3. If 1 and 2 is not conclusive, send clientid, username, password, and certificate to a HTTP server.

Refer to: `config/authn.hocon`

## HTTP authentication

This is an extra authentication to demo if mTLS and client-info authentication are not enough,
an HTTP server can be used to check clients credentials, also to return extra attributes for later use.

EMQX sends MQTT client's TLS certificate (PEM format) to an HTTP serer which extracts certificate DN
from the POST request body and returns HTTP status code 200 with response body like below:

```json
{
    "result": "allow",
    "client_attrs": {
        "key1": "<ID-extracted-from-TLS-certificate-DN>"
    }
}
```

Refer to: `config/authn.hocon`

## Client attributes extraction

A part from `client_attrs` returned from the HTTP authentication server,
we also want to demo the immediate extraction of client attributes from certificate DN,
and assing to the attribute name `key2`.

Refer to: `config/mqtt.hocon`

## Client attributes injected as MQTT message user property

Both `key1` and `key2` of in client attributes will be added to MQTT v5 messages published by the client.

Refer to: `config/msg-trans.hocon`

## Start EMQX and Authentication server

Prerequisite: install docker.

```bash
./start.sh
```

Verify if authentication server is running OK:

```
$ curl -XPOST http://localhost:8000 --data @certs/auth-request.json
{"result": "allow", "client_attrs": {"key1": "demo-ou-1"}}
```

## Test MQTT client

Prerequisite: install [mqttx](https://mqttx.app/downloads)

### Start subscriber

```
./subscribe.sh
```

### Publish one message

```bash
./publish.sh
```

### Observe

Expect the subscriber console to see messages like below:

```
[10/18/2024] [8:49:05 PM] › payload: Fri 18 Oct 2024 08:49:03 PM CEST: Hello from demo-cn-1
userProperties: [
  { key: 'key1', value: 'demo-ou-1' },
  { key: 'key2', value: 'demo-ou-1' }
]
```

Where `key1` is injected from the HTTP auth server, and `key2` is injected from EMQX `client_attrs_init` config in `config/mqtt.hocon`.
