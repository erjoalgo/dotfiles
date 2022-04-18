#!/bin/bash -x

set -euo pipefail

TOP=${HOME}/git/openproject-deploy

if ! test -d "${TOP}"; then
  git clone https://github.com/opf/openproject-deploy \
    --depth=1 --branch=stable/12 "${TOP}"
fi

cd "${TOP}/compose"

sudo apt-get install -y docker{,-compose}

command -v docker-compose

sudo usermod -aG docker $USER

# export DOCKER_HOST=unix://run/docker.sock

sudo docker-compose pull

sudo docker-compose up -d

# OPENPROJECT_DATA_ROOT=/var/lib/openproject
# OPENPROJECT_VERSION=openproject/community:11
# sudo mkdir -p "${OPENPROJECT_DATA_ROOT}/"{pgdata,assets}
# SECRET_KEY_BASE=secret
# export DOCKER_HOST=http+unix://run/containerd/containerd.sock
# docker-compose pull ${OPENPROJECT_VERSION}
# docker run -d -p 8080:80 --name openproject \
#   -e SERVER_HOSTNAME=REDACTED \
#   # The secret key base used for cookies \
#   -e SECRET_KEY_BASE="${SECRET_KEY_BASE}" \
#   -v ${OPENPROJECT_DATA_ROOT}/pgdata:/var/openproject/pgdata \
#   -v ${OPENPROJECT_DATA_ROOT}/assets:/var/openproject/assets \
#   ${OPENPROJECT_VERSION}
