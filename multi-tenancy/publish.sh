#!/bin/bash

set -euo pipefail

TNS='demo-ou-1'
CLIENTID='demo-pub-1'
USERNAME='user-1'

mqttx pub -h localhost -p 8883 -l mqtts --key certs/client.key --cert certs/client.pem --ca certs/ca.pem -i "${CLIENTID}" -t "${USERNAME}/t/1" -m "$(date): Hello from ${CLIENTID}" -u "${USERNAME}"
