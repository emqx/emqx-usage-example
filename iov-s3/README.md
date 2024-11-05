# EMQX Enterprise For Internet of Vehicles

This is a collection of scripts and configurations to demo EMQX Enterprise 5.8.1 for IoV (Internet of Vehicles).

Demoed features:

- TLS termination at loadbalancer (HAProxy) and proxy-protocol for client certificate information extraction (VIN extraction).
- HTTP authentication with ACL.
- Collect messages in files and upload to s3.

## Start

Prerequisite: install docker.

```bash
./start.sh
```

Verify if authentication server is running OK:

```
$ curl -XPOST http://localhost:8000 --data @certs/auth-request.json
{"result": "allow", "acl": [....], ...}
```

Visit EMQX dashboard at URL [localhost:18083](http://localhost:18083). Login username is `admin` and password is also `admin`.

## Test MQTT client

Prerequisite: install [mqttx](https://mqttx.app/downloads) and [jq](https://jqlang.github.io/jq/).

### Start subscriber

```
./subscribe.sh
```

### Publish one message

```bash
./publish.sh
```

Expect the subscriber to receive the message.

## Test s3 file upload

### Publish messages using

The EQMX rule-engine action is configured to upload 10 messages in each CSV file.

Publish N (>= 10) messages by running below command

```
./publish.sh 10
```

### Inspect uploaded files

Go to s3 brwoser: http://localhost:9001

## Stop

```
./stop.sh
```
