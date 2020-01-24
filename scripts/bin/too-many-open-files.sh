#!/bin/bash

# debug "too many open files" error
set -euo pipefail

while getopts "n:h" OPT; do
    case ${OPT} in
    n)
        TOP_N=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

TOP_N=${TOP_N:-10}

TOP=$(
    for PROC in /proc/*/fd; do
    echo -e "$(sudo ls -1 $PROC | wc -l)\t$(basename $(dirname $PROC))\n"
    done | sort -n | tail -n ${TOP_N})

echo "${TOP}"

OLDIFS=$IFS
IFS=$'\n'
for LINE in ${TOP}; do
    echo ${LINE}
    COUNT=$(cut -f1  <<< "${LINE}")
    PID=$(cut -f2  <<< "${LINE}")
    echo "${COUNT} $(ps -fp ${PID} -o command=)"
done
IFS=$OLDIFS

TOP_PID=${PID}
sudo ls -l /proc/${TOP_PID}/fd

