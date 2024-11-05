#!/bin/bash

set -euo pipefail
docker compose up --build -d

## create s3 bucket
docker run --rm -it --net iov-s3_demo-net -v $(pwd)/config/create-s3-bucket.sh:/entry.sh --entrypoint /entry.sh minio/mc

echo -n 'Wait for emqx to start...'
while ! curl -s localhost:18083/status >/dev/null; do
    echo -n '.'
    sleep 1
done
echo ''

curl -s localhost:18083/status
echo ''

# load data integration configs after EMQX is up
docker exec -it emqx emqx ctl conf load /config/s3-upload.hocon
echo ''

echo 'Next steps:'
echo "Start subscriber, run ./subscribe.sh"
echo "Publish a messag, run ./publish.sh"
