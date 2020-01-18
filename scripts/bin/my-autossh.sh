#!/bin/bash -x

set -euo pipefail

while getopts "ha:" OPT; do
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

while true; do
    echo connecting
    ssh ${SSH_HOST_SPEC} -R ${REMOTE_SSH_LISTEN_PORT}:localhost:${LOCAL_SSH_PORT} -N &
    PID=$?
    echo sleeping for ${SLEEP_INTERVAL_MINUTES}m
    sleep $((60 * ${SLEEP_INTERVAL_MINUTES}))
    echo killing
    kill $PID
done
