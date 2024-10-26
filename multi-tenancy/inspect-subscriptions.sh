#!/bin/bash

## This script inspects the first page of current subscriptions.
## The expected subscriptions should all have the tenant ID

set -euo pipefail

echo "This is the first page of all current subscriptions."
echo "The expected print out is to have tenant ID as topic prefix (moutpont)."
echo "The client subscribs to 't/#', and this script shows the internal namespaced subscriptions such as 'demo-ou-1/t/#'"
## apikey:apipass are bootstraped when EMQX node boots up, see docker-compose.yaml and config/api-key-bootstrap
curl -s apikey:apipass@localhost:18083/api/v5/subscriptions | jq .data[]
