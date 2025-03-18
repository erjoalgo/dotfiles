#!/bin/bash -x

set -euo pipefail

STATE=${STATE:-on}

while getopts "n:s:h" OPT; do
    case ${OPT} in
        n)
            # shelly device hostname
            HOSTNAME=${OPTARG}
            ;;
        s)
            # on, toggle or off
            STATE=${OPTARG}
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


curl http://${HOSTNAME}/relay/0?turn=${STATE}
