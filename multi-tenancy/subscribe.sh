#!/bin/bash

set -euo pipefail

TNS='demo-ou-1'
CLIENTID='demo-sub-1'
USERNAME='user-1'
PASSWORD='dummy'

mqttx sub -v -h localhost -p 8883 --key certs/client.key --cert certs/client.pem --ca certs/ca.pem -i "${CLIENTID}" -u "${USERNAME}" -P "${PASSWORD}" -t "${USERNAME}/t/#"
