#!/bin/bash

set -euo pipefail

mqttx pub -h localhost -p 1883 -i 1001010-1000010 -u user1 -P $(./generate_jwt.py) -t topic1 -m msg1
