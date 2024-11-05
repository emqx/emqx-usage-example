#!/bin/bash

set -euo pipefail

# certs/c1.pem common-name
VIN='v1-idA001'
CLIENTID='v1-client1'
USERNAME='v1-user-1'
PASSWORD='dummy'

mqttx sub -v -h localhost -p 8883 --key certs/c1.key --cert certs/c1.pem --ca certs/ca.pem -i "${CLIENTID}" -u "${USERNAME}" -P "${PASSWORD}" -t "${VIN}/#"
