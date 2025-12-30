#!/bin/bash -x

set -euo pipefail

while getopts "u:h" OPT; do
    case ${OPT} in
    u)
        USERNAME=${OPTARG}
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

ha auth reset  \
   --username "${USERNAME}" \
   --interactive
