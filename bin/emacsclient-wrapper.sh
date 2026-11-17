#!/bin/bash
# Send files or (with -e) lisp expressions to every running Emacs server of this desktop group.
#
#   emacsclient-wrapper.sh [-n] FILE...           open files (retried via /sudo:: if that fails)
#   emacsclient-wrapper.sh [-n] -e '(expr)' ...   evaluate expressions; prints each result
#
# Exits 0 if at least one server accepted the request, else with emacsclient's exit code.

set -euo pipefail

EXTRA_ARGS=()
EVAL=false

while getopts "hne:" OPT; do
    case ${OPT} in
    n)
        EXTRA_ARGS+=(-n)
        ;;
    e)
        EVAL=true
        EXTRA_ARGS+=(--eval "${OPTARG}")
        ;;
    h)
        less "$0"
        exit 0
        ;;
    *)
        exit 2
        ;;
    esac
done
shift $((OPTIND -1))

REAL_EMACSCLIENT=/usr/bin/emacsclient.emacs

if ! pgrep -x emacs >/dev/null; then
    emacs &
    disown
fi

LISTENING=$(ss -xlH 2>/dev/null || true)
is_listening() {
    grep -qF " $1 " <<< "${LISTENING}"
}

SUCCESS_COUNT=0
TRIED_COUNT=0
RET=1

shopt -s nullglob
for EMACS_SOCKET_NAME in /tmp/emacs*/* /run/user/*/emacs/*; do
    if test -n "${DESKTOP_GROUP_NUMBER:-}" &&  \
            ! [[ $(basename "${EMACS_SOCKET_NAME}") =~ ^${DESKTOP_GROUP_NUMBER}.* ]]; then
        echo "skipping ${EMACS_SOCKET_NAME}: desktop group number does not match" >&2
        continue;
    fi
    if ! is_listening "${EMACS_SOCKET_NAME}"; then
        echo "skipping ${EMACS_SOCKET_NAME}: stale socket" >&2
        continue;
    fi
    echo "using ${EMACS_SOCKET_NAME}" >&2
    TRIED_COUNT=$((TRIED_COUNT + 1))
    TRAMP_PREFIX=""
    SUCC=false
    PREFIXES=("" "/sudo:root@$(hostname):")
    if ${EVAL}; then
        PREFIXES=("")
    fi
    for TRAMP_PREFIX in "${PREFIXES[@]}"; do
        ARGS=()
        for ARG in "$@"; do
            if [[ ${ARG} == [-+]* ]]; then
                ARGS+=("${ARG}")
            else
                ARGS+=("${TRAMP_PREFIX}${ARG}")
            fi
        done
        if "${REAL_EMACSCLIENT}" \
             "${EXTRA_ARGS[@]}" \
             "--socket-name=${EMACS_SOCKET_NAME}" "${ARGS[@]}"; then
            SUCC=true
            break
        else
            RET=$?
        fi
    done

    if ! ${SUCC}; then
        echo "warn: failed to talk to emacs server at ${EMACS_SOCKET_NAME}" >&2
    else
        SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
    fi
done

if test "${TRIED_COUNT}" -eq 0; then
    exec "${REAL_EMACSCLIENT}" "${EXTRA_ARGS[@]}" "$@"
fi

if test "${SUCCESS_COUNT}" -eq 0; then
    exit "${RET}"
fi
exit 0
