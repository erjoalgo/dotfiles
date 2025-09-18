#!/bin/bash -x

set -euo pipefail

while getopts "t:l:r:m:hs" OPT; do
    case ${OPT} in
        t)
            # example: example.com -p 1234
            SSH_HOST_SPEC=${OPTARG}
            ;;
        l)
            # port where the local sshd is listening
            LOCAL_SSH_PORT=${OPTARG}
            ;;
        r)
            # remote port where to listen and forward connections to the local sshd
            # it must not be used
            REMOTE_SSH_LISTEN_PORT=${OPTARG}
            ;;
        m)
            # number of minutes before killing and restarting ssh forwarding
            SLEEP_INTERVAL_MINUTES=${OPTARG}
            ;;
        s)
            INSTALL_SERVICE=true
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

LOCAL_SSH_PORT=${LOCAL_SSH_PORT:-22}
REMOTE_SSH_LISTEN_PORT=${REMOTE_SSH_LISTEN_PORT:-23}
SLEEP_INTERVAL_MINUTES=${SLEEP_INTERVAL_MINUTES:-60}

if test -n "${INSTALL_SERVICE:-}"; then
    SERVICE_NAME=$(sed 's/[^a-zA-Z0-9-]/-/g' <<< ssh-auto-${SSH_HOST_SPEC})
    install-systemd-service.sh -u "${SERVICE_NAME}" <<EOF
[Unit]
Description=Auto SSH to "${SSH_HOST_SPEC}"
After=network-online.target
Wants=network-online.target

[Service]
ExecStart=/home/ealfonso/.stumpwmrc.d/bin/ssh-auto.sh -t "${SSH_HOST_SPEC}" -r "${REMOTE_SSH_LISTEN_PORT}" -l "${LOCAL_SSH_PORT}"

[Install]
WantedBy=default.target
EOF

    sudo loginctl enable-linger
    sudo loginctl enable-linger ${USER}
    exit 0
fi


while true; do
    echo connecting
    REMOTE_PORT=${REMOTE_SSH_LISTEN_PORT}
    REMOTE_PORT=$(shuf -i 2000-65000 -n 1)
    ssh ${SSH_HOST_SPEC} -R ${REMOTE_PORT}:localhost:${LOCAL_SSH_PORT} -N &
    PID=$?
    echo sleeping for ${SLEEP_INTERVAL_MINUTES}m
    sleep $((60 * ${SLEEP_INTERVAL_MINUTES}))
    echo killing
    kill $PID
done
