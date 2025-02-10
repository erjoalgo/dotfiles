#!/bin/bash -x

set -euo pipefail

while getopts "hd" OPT; do
    case ${OPT} in
    d)
        PRINTER=$(lpstat -p -d | grep -Po '(?<=system default destination: ).*')
        ;;
    h)
        less "$0"
        exit 0
        ;;
    *)
        echo "unrecognized flag: ${OPT}" && exit ${LINENO}
        ;;
    esac
done
shift $((OPTIND -1))

PDF=${1} && shift

if test -z "${PRINTER:-}"; then
    PRINTERS=$(lpstat -t | grep -Po "^[^ ]+?(?= accepting requests)")
    echo "${PRINTERS}" 1>&2
    select PRINTER in ${PRINTERS}; do
        break
    done
fi


lpr "${PDF}" -P "${PRINTER}" -o outputorder=reverse
