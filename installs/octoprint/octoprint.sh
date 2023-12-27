#!/bin/bash -x

set -euo pipefail

cd "$(realpath $(dirname "${BASH_SOURCE[0]}"))"

# sudo apt-get install python3
# pip3 install pyserial --break-system-packages
# python3 -m serial.tools.miniterm

docker compose up -d

PRINTER_DEVICE=$(echo /dev/ttyUSB*)

if ! test -e ${PRINTER_DEVICE}; then
  echo "3d printer not found!"; exit ${LINENO}
fi

sudo chown $(whoami):$(whoami) ${PRINTER_DEVICE}

insert-text-block '# e1b3e892-395b-4db2-9061-dc8559f2e009-set-up-device-path' .env <<EOF
PRINTER_DEVICE=${PRINTER_DEVICE}
EOF

docker compose logs -f

