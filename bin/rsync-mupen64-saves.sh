#!/bin/bash -x

set -euo pipefail
while getopts "s:h" OPT; do
    case ${OPT} in
    s)
        HOST=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

test -n "${HOST}"
ping -c1 "${HOST}"
SAVE_PATH=.local/share/mupen64plus/save/
test -d "${HOME}/${SAVE_PATH}"
rsync -rv --update "${HOST}:${SAVE_PATH}" "${HOME}/${SAVE_PATH}"
