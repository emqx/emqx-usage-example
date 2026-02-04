#!/bin/bash

set -euo pipefail

mqttx sub -v -h localhost -p 1883 -i client1 -u user1 -P "$(./generate_jwt.py)" -t 'myorg/read/#'
