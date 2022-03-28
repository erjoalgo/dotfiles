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


# https://stackoverflow.com/a/37840948/1941755
function urldecode() { : "${*//+/ }"; echo -e "${_//%/\\x}"; }

TEL=$(urldecode "${TEL}")
TEL=$(sed -e 's/^tel://' -e 's/^[+]1//g' -e 's/[^0-9]//g' <<< "${TEL}")
if hostname | md5sum | grep -F 24e9ebe4849f568cda45d05f4884ffbd; then
    USE_GOOGLE_VOICE=true
else
    USE_GOOGLE_VOICE=""
    SIP_HOST=$(linphone-default-host.sh)
    ADDRESS=${TEL}@${SIP_HOST}
    LINPHONE_COMMAND="dial"
    LINPHONE_COMMAND_ARGS="${ADDRESS}";
fi

if command -v x-service-curl; then
    RESP=$(x-service-curl  \
               /read-char  \
               -H"STUMPWM-PROMPT: call (c) or (t) text ${TEL}? ")

    if test "${RESP}" = "t"; then
        # text
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
        # call
        if test -n "${USE_GOOGLE_VOICE}"; then
            x-www-browser "https://voice.google.com/u/0/calls?a=nc,%2B%201${TEL}"
            exit
        else
            LINPHONE_COMMAND="dial"
            LINPHONE_COMMAND_ARGS="${ADDRESS}"
        fi
    else
        echo "unknown option: ${RESP}"
        exit ${LINENO}
    fi
fi


if ! linphonecsh ${LINPHONE_COMMAND} "${LINPHONE_COMMAND_ARGS}" 2>&1; then
    xmessage ${ERROR} -timeout 2 || true
fi
