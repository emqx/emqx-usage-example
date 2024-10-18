#!/bin/bash

set -euo pipefail

# client ID must match certificate common name
CLIENTID='demo-cn-1'

mqttx pub -h localhost -p 8883 -l mqtts --key certs/client.key --cert certs/client.pem --ca certs/ca.pem -i "${CLIENTID}" -t 't/1' -m "$(date): Hello from ${CLIENTID}"
