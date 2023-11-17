#!/bin/bash -x

set -euo pipefail

while getopts "u:d:t:h" OPT; do
  case ${OPT} in
    u)
      URL=${OPTARG}
      ;;
    d)
      DELAY=${OPTARG}
      ;;
    t)
      TIMEOUT=${OPTARG}
      ;;
    h)
      less $0
      exit 0
      ;;
  esac
done
shift $((OPTIND -1))

TIMEOUT=${TIMEOUT:-2}
DELAY=${DELAY:-2}

while true; do
  if ! timeout ${TIMEOUT} curl google.com; then
    notify-send-stumpwm -m 'wifi disconnected'
    sleep 1
    notify-send-stumpwm -m ' '
  fi
  sleep ${DELAY}
done

