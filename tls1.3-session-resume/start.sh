#!/bin/bash

set -euo pipefail

cd "$(dirname "$0")"

# Start the containers
docker compose up -d

echo 'Waiting for emqx1 to start...'
while ! curl -s localhost:18083/status >/dev/null 2>&1; do
    echo -n '.'
    sleep 1
done
echo ' emqx1 ready'

echo 'Waiting for emqx2 to start...'
while ! curl -s localhost:18084/status >/dev/null 2>&1; do
    echo -n '.'
    sleep 1
done
echo ' emqx2 ready'

echo ''
echo 'Dashboards:'
echo '  emqx1: http://localhost:18083 (user: admin, password: admin)'
echo '  emqx2: http://localhost:18084 (user: admin, password: admin)'
echo ''
echo 'Test TLS 1.3 session resumption:'
echo '  Single node:     ./tls1.3-resumption-test.sh localhost:8883'
echo '  Cross-node:      ./tls1.3-resumption-test.sh localhost:8883 localhost:8884'
echo '  Python (single): ./tls1.3-resumption-test.py --no-host-check localhost:8883'
echo '  Python (cross):  ./tls1.3-resumption-test.py --no-host-check localhost:8883 localhost:8884'
