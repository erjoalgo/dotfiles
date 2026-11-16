#!/bin/bash

set -euo pipefail

EXTRA_ARGS=()

while getopts "hne:" OPT; do
    case ${OPT} in
    n)
        EXTRA_ARGS+=(-n)
        ;;
    e)
        EXTRA_ARGS+=(--eval "${OPTARG}")
        ;;
    h)
        less "$0"
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

REAL_EMACSCLIENT=/usr/bin/emacsclient.emacs

if ! pgrep -x emacs >/dev/null; then
    emacs &
    disown
fi

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
    TRAMP_PREFIX=""
    SUCC=false
    for TRAMP_PREFIX in "" "/sudo:root@$(hostname):"; do
        ARGS=()
        for ARG in "$@"; do
            if [[ ${ARG} == [-+]* ]]; then
                ARGS+=("${ARG}")
            else
                ARGS+=("${TRAMP_PREFIX}${ARG}")
            fi
        done
        if env EMACS_SOCKET_NAME=${EMACS_SOCKET_NAME}  \
             "${REAL_EMACSCLIENT}" \
             "${EXTRA_ARGS[@]}" \
             "--socket-name=${EMACS_SOCKET_NAME}" "${ARGS[@]}"; then
            SUCC=true
            break
        else
            RET=$?
        fi
    done

    if ! ${SUCC}; then
        echo "warn: failed to talk to emacs server at ${EMACS_SOCKET_NAME}"
        FAIL_COUNT=$((FAIL_COUNT + 1))
    else
        SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
    fi
done

if test "${SUCCESS_COUNT}" -eq 0; then
    exec "${REAL_EMACSCLIENT}" "${EXTRA_ARGS[@]}" "$@"
fi

exit 0
