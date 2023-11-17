#!/bin/bash -x

set -euo pipefail

REAL_EMACSCLIENT=/usr/bin/emacsclient.emacs

if test -n "${DESKTOP_GROUP_NUMBER:-}"; then
  SERVER_DIRECTORY=$(emacs -q --batch  \
    --eval "(require 'server)"  \
    --eval "(print server-socket-dir)" \
    2>/dev/null  \
                         | tail -1 | tr -d '"')
  RET=0
  for EMACS_SOCKET_NAME in ${SERVER_DIRECTORY}/${DESKTOP_GROUP_NUMBER}.*; do
      echo "attempting to connect to ${EMACS_SOCKET_NAME}"
      EMACS_SOCKET_NAME=${EMACS_SOCKET_NAME} "${REAL_EMACSCLIENT}" \
                       "--socket-name=${EMACS_SOCKET_NAME}" "${@}"
      if test $? -ne 0; then
          RET=$?
      fi
  done
  exit $RET
fi

"${REAL_EMACSCLIENT}" "${@}"