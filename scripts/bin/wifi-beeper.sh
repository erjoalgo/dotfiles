#!/bin/bash -x

set -euo pipefail


# TODO(ejalfonso): use airodump
NETWORK=${1} && shift
while true; do
  if ! SIGNAL=$(nmcli d w list --rescan yes |&  \
    grep -i "${NETWORK}" |  \
    grep -Po '(?<=Mbit/s).*? ([0-9]+)' |
       sort  | tail -1); then
    echo "network not found: ${NETWORK}"
    sleep 1
    continue
  fi
  BEEP_FREQ=$(python3 -c "print(440*(1 + 2*(${SIGNAL} / 100)))")
  x-service-curl /beep -f ${BEEP_FREQ}
  espeak ${SIGNAL}
done
