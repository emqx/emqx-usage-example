#!/bin/bash

set -euo pipefail

mkdir -p data

## EMQX in docker is running with `emqx` user, which has an unknown user ID to the host, so we change mode from host
chmod 777 data

docker compose up --build -d
