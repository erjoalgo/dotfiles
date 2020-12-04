#!/bin/bash -x

set -euo pipefail
URL="https://github.com/docker/compose/releases/download/1.27.4/docker-compose-$(uname -s)-$(uname -m)"

sudo curl -L  "${URL}" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose
