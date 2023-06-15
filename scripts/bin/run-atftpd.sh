#!/bin/bash -x

set -euo pipefail

while getopts "hd:" OPT; do
    case ${OPT} in
    d)
        DIRECTORY=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

test -d "${DIRECTORY}"
cd "${DIRECTORY}"
sudo chmod -R a+rwx "${DIRECTORY}"

while pgrep -f /sbin/atftpd | xargs kill -9 2> /dev/null; do
    sleep 1
done

sudo atftpd --no-fork --verbose=5 --logfile - --daemon "${DIRECTORY}"
