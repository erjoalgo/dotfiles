#!/bin/bash -x

set -euo pipefail

while getopts "d:m:" OPT; do
    case ${OPT} in
    d)
        CONFIG_DIR=${OPTARG}
        ;;
    m)
        MARLIN_DIR=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))


CONFIG_DIR=${CONFIG_DIR:-${HOME}/git/Configurations/config/examples/Creality/Ender-3\ Pro/CrealityV422/}
MARLIN_DIR=${MARLIN_DIR:-${HOME}/git/Marlin}

test -d "${CONFIG_DIR}"
test -d "${MARLIN_DIR}"

cp -t ${MARLIN_DIR}/Marlin "${CONFIG_DIR}"/Configuration{,_adv}.h
cp -t ${MARLIN_DIR}/Marlin "${CONFIG_DIR}"/{_Bootscreen.h,_Statusscreen.h}


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
