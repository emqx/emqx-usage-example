#!/bin/bash

set -euo pipefail

docker run --name emqx -d --net host \
    -v $(pwd)/base.hocon:/opt/emqx/etc/base.hocon \
    emqx/emqx:6.1.1-rc.1

# Wait for EMQX to be ready by polling localhost:18083
echo "Waiting for EMQX to be ready..."
max_attempts=30
attempt=0
while [ $attempt -lt $max_attempts ]; do
    if curl -s -f http://localhost:18083/api/v5/status > /dev/null 2>&1; then
        echo "EMQX is ready!"
        echo ""
        echo "Next steps:"
        echo "  Run ./pub-ok.sh to verify authentication works"
        echo "  Run ./pub-fail.sh to verify authentication works to reject if clientid does not match substring of jwt token sub claim"
        exit 0
    fi
    attempt=$((attempt + 1))
    echo "Attempt $attempt/$max_attempts: EMQX not ready yet, waiting..."
    sleep 5s
done

echo "Error: EMQX did not become ready within expected time" >&2
exit 1
