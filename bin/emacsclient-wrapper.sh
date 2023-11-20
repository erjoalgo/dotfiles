#!/bin/bash -x

set -euo pipefail

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
    echo "attempting to connect to ${EMACS_SOCKET_NAME}"
    if ! test -e "${EMACS_SOCKET_NAME}"; then
        continue;
    fi
    SERVER_USER_ID=$(grep -Po "(?<=/emacs)[0-9]+(?=/)|(?<=user/)([0-9]+)(?=/emacs)" <<< "${EMACS_SOCKET_NAME}")
    SERVER_USER=$(id -un "${SERVER_USER_ID}")
    TRAMP_PREFIX=""
    if ! sudo -u "${SERVER_USER}" bash test -r "${@}"; then
        TRAMP_PREFIX="/sudo:root@$(hostname):"
    fi

    if ! sudo -u "${SERVER_USER}" env EMACS_SOCKET_NAME=${EMACS_SOCKET_NAME} "${REAL_EMACSCLIENT}" \
         "--socket-name=${EMACS_SOCKET_NAME}" "${TRAMP_PREFIX}${@}"; then
        echo "warn: failed to talk to emacs server at ${EMACS_SOCKET_NAME}"
        RET=$?
        FAIL_COUNT+=1
    else
        SUCCESS_COUNT+=1
    fi
done

if test "${SUCCESS_COUNT+}" = 0; then
    "${REAL_EMACSCLIENT}" "${@}"
fi

exit $RET
