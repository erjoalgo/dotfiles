#!/bin/bash

set -euo pipefail

while getopts "t:h" OPT; do
    case ${OPT} in
    t)
        TEL=${OPTARG}
        ;;
    s)
        SIP_HOST=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))


# https://stackoverflow.com/questions/6250698/
function urldecode() {: "${*//+/ }"; echo -e "${_//%/\\x}";}

TEL=$(urldecode "${TEL}")
SIP_HOST=$(linphone-default-host.sh)
TEL=$(sed -e 's/^tel://' -e 's/^[+]1\|[.]//g' -e 's/[^0-9]//g' <<< "${TEL}")

ADDRESS=${TEL}@${SIP_HOST}

LINPHONE_COMMAND="dial"
LINPHONE_COMMAND_ARGS="${ADDRESS}"

if command -v x-service-curl; then
    RESP=$(x-service-curl  \
               /read-char  \
               -H"STUMPWM-PROMPT: call (c) or (t) text ${TEL}? ")

    if test "${RESP}" = "t"; then
        if command -v emacs-sip; then
            emacs-sip "${TEL}"
            exit $?
        fi
        CHAT=$(x-service-curl  \
               /read-line  \
               -H "STUMPWM-PROMPT: enter SMS to send to ${TEL}: ")
        LINPHONE_COMMAND="generic"
        LINPHONE_COMMAND_ARGS="chat ${ADDRESS} ${CHAT}"
    elif test "${RESP}" = "c"; then
        LINPHONE_COMMAND="dial"
        LINPHONE_COMMAND_ARGS="${ADDRESS}"
    else
        echo "unknown option: ${RESP}"
        exit ${LINENO}
    fi
fi


if ! linphonecsh ${LINPHONE_COMMAND} "${LINPHONE_COMMAND_ARGS}" 2>&1; then
    xmessage ${ERROR} -timeout 2 || true
fi
