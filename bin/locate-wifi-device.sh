#!/bin/bash -x

set -euo pipefail

CMD=(sudo horst)

while getopts "i:c:e:h" OPT; do
    case ${OPT} in
    i)
        CMD+=(-i ${OPTARG})
        ;;
    c)
        CMD+=(-c ${OPTARG})
        ;;
    e)
        CMD+=(-e ${OPTARG})
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

${CMD[@]}
