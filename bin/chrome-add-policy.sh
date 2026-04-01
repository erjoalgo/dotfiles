#!/bin/bash -x

set -euo pipefail

while getopts "b:c:h" OPT; do
    case ${OPT} in
        b)
            BASENAME=${OPTARG}
            ;;
        c)
            CONTENTS=${OPTARG}
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

if test -z "${CONTENTS:-}"; then
    CONTENTS=$(cat /dev/stdin)
fi

EXT="${BASENAME#*.}"

test "${EXT}" = json

for POLICIES_DIR in  \
    /etc/opt/chrome/policies/ \
                            /etc/chromium/policies; do
    sudo mkdir -p ${POLICIES_DIR}/{managed,recommended}
    sudo tee ${POLICIES_DIR}/managed/${BASENAME}  <<< "${CONTENTS}"
done
