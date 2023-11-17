#!/bin/bash -x

set -euo pipefail

while getopts "ha:" OPT; do
    case ${OPT} in
      a)
        SERVER_HOSTNAME=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if ! getent group docker; then
  sudo groupadd docker
fi

sudo usermod -aG docker $(whoami)

SOCKET=/var/run/docker.sock
sudo chown root:docker ${SOCKET}

sudo mkdir -p /var/lib/openproject/{pgdata,assets}

           # The public facing host name \

  # The secret key base used for cookies \
docker run -d -p 8082:80 --name openproject \
  -e SERVER_HOSTNAME=${SERVER_HOSTNAME} \
  -e SECRET_KEY_BASE=secret \
  -v /var/lib/openproject/pgdata:/var/openproject/pgdata \
  -v /var/lib/openproject/assets:/var/openproject/assets \
  openproject/community:12
