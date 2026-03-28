#!/bin/bash -x

set -euo pipefail

INSTALL=true
while getopts "irh" OPT; do
    case ${OPT} in
    i)
        INSTALL=true
        ;;
    r)
        INSTALL=false
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

SECS=30

if test "${INSTALL:-}" = true; then
    pip install websockets
    install-systemd-service.sh k1c-fan -u <<EOF
[Unit]
Description=Keep the K1C exhaust fan on

[Service]
ExecStart=nohup $(realpath $0) -r
Restart=always
RestartSec=${SECS}
Environment=PATH=${PATH}
Environment=WS_URL=ws://k1c.arpa:9999

[Install]
WantedBy=default.target

EOF
    exit 0
fi

while true; do
    which k1c-gcode.sh
    which wscat
    k1c-gcode.sh -b
    sleep ${SECS};
done
