#!/bin/bash

set -euo pipefail

# cert common name of certs/c2.pem
VIN='v2-idB002'
CLIENTID='pub-2'
USERNAME='user-2'
# the subscriber's VIN
SUB_VIN='v1-idA001'

N="${1:-1}"

while true; do
    mqttx pub -h localhost -p 8883 -l mqtts --key certs/c2.key --cert certs/c2.pem --ca certs/ca.pem -i "${CLIENTID}" -t "${SUB_VIN}/t/1" -m "$(date): Hello from ${CLIENTID}" -u "${USERNAME}"
    N=$(( N - 1))
    if [ $N -eq 0 ]; then
        exit 0
    fi
done
