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
echo "Start subscriber, run ./subscribe.sh"
echo "Publish a messag, run ./publish.sh"
