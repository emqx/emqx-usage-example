#!/bin/bash

set -euo pipefail

cd "$(dirname "$0")"

# Start the container
docker compose up -d

echo 'Waiting for emqx to start...'
while ! curl -s localhost:18083/status >/dev/null 2>&1; do
    echo -n '.'
    sleep 1
done
echo ' emqx ready'

echo ''
echo 'Dashboard: http://localhost:18083 (user: admin, password: admin)'
echo ''
echo 'Test TLS 1.2 session resumption:'
echo '  ./tls1.2-resumption-test.sh localhost:8883'
