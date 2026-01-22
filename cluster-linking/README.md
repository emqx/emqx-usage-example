# Simple Cluster Linking Demo

This example runs two independent EMQX nodes with Cluster Linking enabled so
clients can publish/subscribe on either broker for the `linked/#` topic space.

## Quick Start

Start the nodes:

```
docker compose up -d
```

Start subscribers:

Subscriber connected to `cluster-a`:
```
mqttx sub -h localhost -p 1883 -t linked/1
✔ Connected
✔ Subscribed to linked/1
```

Subscriber connected to `cluster-b`:
```
mqttx sub -h localhost -p 2883 -t linked/1
✔ Connected
✔ Subscribed to linked/1
```

Publish a message to `cluster-a`:
```
mqttx pub -h localhost -p 1883 -t linked/1 -m 'message #1 to cluster-a'
✔ Connected
✔ Message published
```

Observe that the message has been received by both subscribers:
```
topic: linked/1, qos: 0, size: 23B
message #1 to cluster-a
```

Publish a message to `cluster-b`:
```
mqttx pub -h localhost -p 2883 -t linked/1 -m 'message #1 to cluster-b'
✔ Connected
✔ Message published
```

Observe that the message has been received by both subscribers:
```
topic: linked/1, qos: 0, size: 23B
message #1 to cluster-b
```
