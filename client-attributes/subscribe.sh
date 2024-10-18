#!/bin/bash

set -euo pipefail

CLIENTID='demo-sub-1'
USERNAME='sub-user-1'
# This client is allowed to login with secret hash algorithm
PASSWORD="$(echo -n "${USERNAME}:magic-chicken" | sha1sum | awk '{print $1}')"

mqttx sub -h localhost -p 8883 -l mqtts --key certs/client.key --cert certs/client.pem --ca certs/ca.pem -i "${CLIENTID}" -u "${USERNAME}" -P "${PASSWORD}" -t 't/#'
