#!/bin/bash

set -euo pipefail

NAME='rpush'
PKGNAME="emqx_rpush-0.1.0.tar.gz"
EMQX_VERSION='5.8.2'

docker rm -f "$NAME" || true

docker run -d --rm --net host --name "${NAME}" \
    "emqx/emqx-enterprise:${EMQX_VERSION}"

# Wait for EMQ X to start
while ! curl -s localhost:18083 > /dev/null; do
    echo "Waiting for EMQX to start..."
    sleep 1
done

docker cp _build/default/mqx_rpush/${PKGNAME} "${NAME}":/opt/emqx/plugins/
docker exec -it --user root ${NAME} chown emqx:emqx -R /opt/emqx/plugins/
