# Deploying EMQX Cluster with HAProxy MQTT Load Balancing

This is a quick example to show how to deploy EMQX Cluster with HAProxy MQTT Load Balancing.

For more information like how to configure HAProxy, please see EMQX Documentation: [MQTT Load Balancing](https://docs.emqx.com/en/enterprise/v5.1/deploy/cluster/lb-haproxy.html).

## Useage

```bash
docker compose up -d
```

Open <http://localhost:18083> to visit EMQX Dashboard.

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
