# EMQX Enterprise Multi-Tenancy

This is a collection of scripts and configurations to demo EMQX Enterprise 5.8.1 for multi-tenancy (MQTT client user groups).

We will continue to iterate on this example as more multi-tenancy features getting released in future versions.

Future features for MQTT client multi-tenancy:

- Per-tenant connection limit
- Per-tenant rate limiting
- Tenant ID as log metadata
- Tenant management
- Per-tenant client count/search

Administrator multi-tenancy is not in the scope of this deomo.

## Features

- Use TLS certificate OU (Organizational Unit) as MQTT client tenant ID.
- Use tenant ID as client topics mountpoint.
- Use HTTP authentication which can validate tenant ID.

## Tenant ID extraction

This demo shows how one can extract tenant ID from client certificate.
However it does not mean it's the only way.

Below are more possible solutions:

- TLS SNI (Server name indication). If TLS is terminated by load balancer, it can still be forwarded to EMQX in proxy-protocol headers.
- Sub-string of Client ID. For example, hyphen-separated prefix of the client ID.
- Username (or sub-string).

The tenant ID is stored as a client attribute named `tns`(as in tenant namespace).
It technically can be named something else, however our plan is to make it a conventional client attribute key.
e.g. in 5.9, we will start making `client_attrs.tns` (when found) a log message label.

Refer to: `config/mqtt.hocon`

## HTTP authentication

An HTTP server to return authentication results together with authorization (ACL).

```json
{
    "result": "allow",
    "acl": [
        {
            "permission": "allow",
            "action": "all",
            "topics": ["user-1/#"]
        }
    ]
}
```

NOTE: As of now (5.8), EMQX's ACL checks are done before topic mountpoint is added.
Meaning, the authentication server should return rules for the topics *before* mountpoint is applied.

Refer to: `config/authn.hocon`

## Start EMQX and Authentication server

Prerequisite: install docker.

```bash
./start.sh
```

Verify if authentication server is running OK:

```
$ curl -XPOST http://localhost:8000 --data @certs/auth-request.json
{"result": "allow", "acl": [....]}
```

Visit EMQX dashboard at URL [localhost:18083](http://localhost:18083). Login username is `admin` and password is also `admin`.

## Test MQTT client

Prerequisite: install [mqttx](https://mqttx.app/downloads) and [jq](https://jqlang.github.io/jq/).

### Start subscriber

```
./subscribe.sh
```

Expect to see outputs like:
```
› …  Subscribing to t/#...
› ✔  Subscribed to t/#
```

i.e. The client does not know its actually subscribed to `demo-ou-1/t/#` internally in EMQX.

### Publish one message

```bash
./publish.sh
```

Expect to see `Message published` from this shell, and the subscriber shell is expected to see pintouts like below:

```
› topic: t/1
payload: Sat 26 Oct 2024 10:25:42 PM CEST: Hello from demo-pub-1
```

i.e. The client does not know its actually publishing to `demo-ou-1/t/#` internally in EMQX.

### Inspect tenant's topic namespace

To verify that the clients are not publishing and subscribing the global namespace (without mountpoint),
we can call EMQX management API to inspect the internal subscriptions.

```
./inspect-subscriptions.sh
```

It should print the real subscriptions like below:

```
...
"topic": "demo-ou-1/t/#",
...
```
