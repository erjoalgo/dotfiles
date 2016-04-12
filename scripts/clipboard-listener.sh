#!/bin/bash

while getopts "v" OPT; do
    case ${OPT} in
	v)
	    VERBOSE=true
	    ;;
    esac
done
#listen for incoming text on a given port
#and paste it to the clipboard
#use to transfer snippets/urls between machines
PORT="${1:-1234}"
echo "listening on port ${PORT}"
while true; do
    MESSAGE=$(nc -lp "${PORT}")
    if test -n "${VERBOSE}"; then
	echo "got  ${MESSAGE}"
    fi
    echo "${MESSAGE}" | xsel -ip;
    #echo "last clipboard:  $(xsel -o)"
done
