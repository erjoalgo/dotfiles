#!/bin/bash -x

set -euo pipefail

if test -n "${DESKTOP_GROUP_NUMBER:-}"; then
  SERVER_DIRECTORY=$(emacs -q --batch  \
    --eval "(require 'server)"  \
    --eval "(print server-socket-dir)" \
    2>/dev/null \
    | tail -1 | tr -d '"')
  EMACS_SOCKET_NAME="${SERVER_DIRECTORY}/${DESKTOP_GROUP_NUMBER}"
  if test -e "${EMACS_SOCKET_NAME}"; then
    echo "attempting to connect to ${EMACS_SOCKET_NAME}"
    EMACS_SOCKET_NAME=${EMACS_SOCKET_NAME} emacsclient ${*}
    exit $?
  fi
fi

emacsclient ${*}
