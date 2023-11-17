#!/bin/bash -x

set -euo pipefail

# sudo apt-get install -y docker-compose

SELFD=$(dirname "${BASH_SOURCE[0]}")

sudo xhost + local:docker

if ! command -v docker-compose; then
    ${SELFD}/install-docker-compose.sh
fi

# docker run --rm -i -t --privileged \
#     -v /dev/bus/usb:/dev/bus/usb \
#     -v /tmp/.X11-unix:/tmp/.X11-unix \
#     -e DISPLAY=$DISPLAY \
#     pierlo1/scrcpy:intel

URL=https://github.com/pierlon/scrcpy-docker
GIT_REPO="${HOME}/git/$(basename ${URL})"
if ! test -d "${GIT_REPO}"; then
    git clone "${URL}" "${GIT_REPO}"
fi

cd "${GIT_REPO}"
sudo env SCRCPY_GRAPHICS_TYPE=intel docker-compose run --rm scrcpy  \
     scrcpy --show-touches
