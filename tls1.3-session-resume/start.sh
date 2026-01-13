#!/bin/bash

set -euo pipefail
docker compose up --build -d

echo -n 'Wait for emqx to start...'
while ! curl -s localhost:18083/status >/dev/null; do
    echo -n '.'
    sleep 1
done
echo ''

curl -s localhost:18083/status

echo ''
echo ''
echo 'Next steps:'
echo "Run TODO script to test TLS 1.3 session resumption"
