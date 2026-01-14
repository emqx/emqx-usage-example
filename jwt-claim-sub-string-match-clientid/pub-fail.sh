#!/bin/bash

set -euo pipefail

mqttx pub -h localhost -p 1883 -i randomid1 -u user1 -P $(./generate_jwt.py) -t topic1 -m msg1
