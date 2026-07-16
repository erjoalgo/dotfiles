#!/bin/bash -x

set -euo pipefail

# Create directory for Docker cli plugins
mkdir -p ${HOME}/.docker/cli-plugins

# Download docker-rollout script to Docker cli plugins directory
curl https://raw.githubusercontent.com/wowu/docker-rollout/main/docker-rollout \
     -o ${HOME}/.docker/cli-plugins/docker-rollout

# Make the script executable
chmod +x ~/.docker/cli-plugins/docker-rollout
