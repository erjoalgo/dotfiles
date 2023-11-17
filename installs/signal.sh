#!/bin/bash -x

set -euo pipefail

curl -s https://updates.signal.org/desktop/apt/keys.asc | sudo apt-key add -

echo "deb [arch=amd64] https://updates.signal.org/desktop/apt xenial main" \
    | sudo tee /etc/apt/sources.list.d/signal-xenial.list

sudo apt-get install -y apt-transport-https

sudo apt update && sudo apt install -y signal-desktop

sudo chmod 4755 /opt/Signal/chrome-sandbox
