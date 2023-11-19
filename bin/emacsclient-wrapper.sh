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
RET=0

for EMACS_SOCKET_NAME in /tmp/emacs*/* ; do
    echo "attempting to connect to ${EMACS_SOCKET_NAME}"
    SERVER_USER_ID=$(grep -Po "(?<=/emacs)[0-9]+(?=/)" <<< "${EMACS_SOCKET_NAME}")
    SERVER_USER=$(id -un "${SERVER_USER_ID}")
    TRAMP_PREFIX=""
    if ! sudo -u "${SERVER_USER}" bash test -r "${@}"; then
        TRAMP_PREFIX="/sudo:root@$(hostname):"
    fi

    if ! sudo -u "${SERVER_USER}" env EMACS_SOCKET_NAME=${EMACS_SOCKET_NAME} "${REAL_EMACSCLIENT}" \
         "--socket-name=${EMACS_SOCKET_NAME}" "${TRAMP_PREFIX}${@}"; then
        echo "warn: failed to talk to emacs server at ${EMACS_SOCKET_NAME}"
        RET=$?
    fi
done
exit $RET
