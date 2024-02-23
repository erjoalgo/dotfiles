#!/bin/bash -x

set -euo pipefail
sudo apt-get install -y nginx
sudo ufw allow 80,443/tcp
docker compose up
