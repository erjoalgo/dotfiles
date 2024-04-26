#!/bin/bash -x

set -euo pipefail

while getopts "hn:i:" OPT; do
    case ${OPT} in
    i)
        IP_ADDRESS=${OPTARG}
        ;;
    n)
        PRINTER_NAME=${OPTARG}
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

sudo lpadmin -p "${PRINTER_NAME}" -v "ipp://${IP_ADDRESS}/ipp/print"
