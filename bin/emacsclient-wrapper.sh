#!/bin/bash

set -euo pipefail

EXTRA_ARGS=()

while getopts "hn" OPT; do
    case ${OPT} in
    n)
        EXTRA_ARGS+=(-n)
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

REAL_EMACSCLIENT=/usr/bin/emacsclient.emacs

if ! pgrep emacs; then
    emacs &
    disown
fi

SERVER_DIRECTORY=$(emacs -q --batch  \
                         --eval "(require 'server)"  \
                         --eval "(print server-socket-dir)" \
                         2>/dev/null  \
                       | tail -1 | tr -d '"')
SUCCESS_COUNT=0
FAIL_COUNT=0
RET=0

for EMACS_SOCKET_NAME in  \
    $(echo /tmp/emacs*/*)  \
        $(echo /run/user/*/emacs/*); do
    if ! test -e "${EMACS_SOCKET_NAME}"; then
        echo "skipping ${EMACS_SOCKET_NAME}: missing socket file"
        continue;
    fi
    if test -n "${DESKTOP_GROUP_NUMBER:-}" &&  \
            ! [[ $(basename "${EMACS_SOCKET_NAME}") =~ ^${DESKTOP_GROUP_NUMBER}.* ]]; then
        echo "skipping ${EMACS_SOCKET_NAME}: desktop group number does not match"
        continue;
    fi
    echo "using ${EMACS_SOCKET_NAME}"
    SERVER_USER_ID=$(grep -Po "(?<=/emacs)[0-9]+(?=/)|(?<=user/)([0-9]+)(?=/emacs)" <<< "${EMACS_SOCKET_NAME}")
    SERVER_USER=$(id -un "${SERVER_USER_ID}")
    TRAMP_PREFIX=""
    SUCC=false
    # SUDO_OPT=(sudo -u "${SERVER_USER}")
    SUDO_OPT=()
    for TRAMP_PREFIX in "" "/sudo:root@$(hostname):"; do
        if ! ${SUDO_OPT[@]} env EMACS_SOCKET_NAME=${EMACS_SOCKET_NAME}  \
             "${REAL_EMACSCLIENT}" \
             ${EXTRA_ARGS[@]} \
             "--socket-name=${EMACS_SOCKET_NAME}" "${TRAMP_PREFIX}${@}"; then
            RET=$?
        else
            SUCC=true
            break
        fi
    done

    if ${SUCC} = true; then
        echo "warn: failed to talk to emacs server at ${EMACS_SOCKET_NAME}"
        FAIL_COUNT+=1
    else
        SUCCESS_COUNT+=1
    fi
done

if test "${SUCCESS_COUNT+}" = 0; then
    "${REAL_EMACSCLIENT}" "${@}"
fi

exit $RET
