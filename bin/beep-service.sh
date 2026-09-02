#!/bin/bash -x

set -euo pipefail

ORIG_CMDLINE=( "$@" )

PORT=${PORT:-6339}

while getopts "hiop:" OPT; do
    case ${OPT} in
    i)
        INSTALL=true
        ;;
    o)
        ONCE=true
        ;;
    p)
        PORT=${OPTARG}
        ;;
    h)
        less "$0"
        exit 0
        ;;
    *)
        echo "unrecognized flag: ${OPT}" && exit ${LINENO}
        ;;
    esac
done
shift $((OPTIND -1))

CMDLINE=( "$@" )

if test "${INSTALL:-}" = true; then
    install-systemd-service.sh beep-service -u <<EOF
[Unit]
Description=Beeper pcspkr service

[Service]
ExecStart=$(realpath $0 -p${PORT} ${CMDLINE[@]}
Restart=always
RestartSec=60
Environment=PATH=${PATH}

[Install]
WantedBy=default.target

EOF
    exit 0
fi

while true; do
  { echo -ne "HTTP/1.0 200 OK\r\n\r\n"; } | nc -l -p ${PORT} -q 1

  ${CMDLINE[@]}

  if test -n "${ONCE:-}"; then
    exit $LINENO
  fi
done

