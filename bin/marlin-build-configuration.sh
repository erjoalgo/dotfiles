#!/bin/bash -x

set -euo pipefail

SKIP_BRANCH_SELECTION=false
while getopts "d:m:sh" OPT; do
    case ${OPT} in
    d)
        CONFIG_DIR=${OPTARG}
        ;;
    m)
        MARLIN_DIR=${OPTARG}
        ;;
    s)
        SKIP_BRANCH_SELECTION=true
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))


CONFIG_DIR=${CONFIG_DIR:-${HOME}/git/Configurations/}
MARLIN_DIR=${MARLIN_DIR:-${HOME}/git/Marlin}
# may not exist until after switching branches
CONFIG_PATH=${CONFIG_DIR}/config/examples/Creality/Ender-3\ Pro/CrealityV422

test -d "${CONFIG_DIR}"
test -d "${MARLIN_DIR}"

if test "${SKIP_BRANCH_SELECTION}" != true; then
    for DIR in "${CONFIG_DIR}" "${MARLIN_DIR}"; do
    cd "${DIR}"
    echo "select $(basename $(pwd)) branch: " 1>&2
    BRANCHES=$(git for-each-ref --format='%(refname:short)' refs)
    select BRANCH in ${BRANCHES}; do
        git checkout -- .
        git checkout "${BRANCH}"
        break
    done
    done
fi

cp -t ${MARLIN_DIR}/Marlin  \
   "${CONFIG_PATH}"/{Configuration.h,Configuration_adv.h,_Bootscreen.h,_Statusscreen.h}


if ! command -v platformio; then
    pip install platformio
fi

cd "${MARLIN_DIR}"


function marlin-get-motherboard {
    grep -Po " *(?<=#define MOTHERBOARD BOARD_).*" "${MARLIN_DIR}/Marlin/Configuration.h"
}

function marlin-list-environments-for-motherboard {
    MB=${1} && shift
    grep -FA1 "MB(${MB})" "${MARLIN_DIR}/Marlin/src/pins/pins.h" |  \
        sed 's/ /\n/g' | grep -Po '(?<=env:).*'
}

MB=$(marlin-get-motherboard)
ENVS=$(marlin-list-environments-for-motherboard "${MB}")

echo "select environment: " 1>&2
select ENV in ${ENVS}; do
    break
done

platformio run -e "${ENV}"

echo "note: built using platformio env: ${ENV}"
