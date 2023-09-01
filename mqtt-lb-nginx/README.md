# Deploying EMQX Cluster with Nginx MQTT Load Balancing

This is a quick example to show how to deploy EMQX Cluster with Nginx MQTT Load Balancing.

For more information like how to configure Nginx, please see EMQX Documentation: [MQTT Load Balancing](https://docs.emqx.com/en/enterprise/v5.1/deploy/cluster/lb-nginx.html).

## Useage

```bash
docker compose up -d
```

Open <http://localhost:18083> to visit EMQX Dashboard.

## Test

EMQX MQTT TCP

```bash
$ mqttx sub --config ./mqttx_cli_emqx_tcp.json

[2023-8-22] [22:26:21] › …  Connecting using configuration file, host: localhost, port: 1883, topic: t/1
[2023-8-22] [22:26:21] › ✔  Connected
[2023-8-22] [22:26:21] › …  Subscribing to t/1...
[2023-8-22] [22:26:21] › ✔  Subscribed to t/1
```

## Test

### EMQX MQTT TCP

Create 10 clients and subscribe to topic `t/{clientid}` with MQTT.

```bash
mqttx bench sub --config ./mqttx_cli_emqx_tcp.json
```

### EMQX MQTT TLS

Create 10 clients and subscribe to topic `t/{clientid}` with MQTT TLS.

```bash
mqttx bench sub --config ./mqttx_cli_emqx_tls.json
```