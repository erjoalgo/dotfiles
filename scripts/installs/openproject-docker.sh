#!/bin/bash -x

set -euo pipefail

cd ~/git
if ! test -d openproject; then
    git clone https://github.com/opf/openproject-deploy --depth=1 --branch=stable/12 openproject
fi

cd openproject/compose

sudo apt-get install -y docker{,-compose}

sudo usermod -aG docker $USER

# export DOCKER_HOST=unix://run/docker.sock

sudo docker-compose pull

sudo docker-compose up -d
