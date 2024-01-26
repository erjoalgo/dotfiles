#!/bin/bash -x

set -euo pipefail

INTERACTIVE=false
INTERACTIVE_BRANCH_SELECTION=false
while getopts "d:m:ibn:qh" OPT; do
    case ${OPT} in
    d)
        CONFIG_DIR=${OPTARG}
        ;;
    m)
        MARLIN_DIR=${OPTARG}
        ;;
    i)
        INTERACTIVE=true
        ;;
    b)
        INTERACTIVE_BRANCH_SELECTION=true
        ;;
    n)
        CUSTOM_NAME_PREFIX=${OPTARG}
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
CUSTOM_NAME_PREFIX=${CUSTOM_NAME_PREFIX:-"Ernesto's"}

test -d "${CONFIG_DIR}"
test -d "${MARLIN_DIR}"

if test "${INTERACTIVE_BRANCH_SELECTION:-}" = true; then
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

CONFIGURATIONS=$(echo "${MARLIN_DIR}"/Marlin/Configuration{,_adv}.h)

sed -i "s/^#define CUSTOM_MACHINE_NAME \"/\0${CUSTOM_NAME_PREFIX} /" ${CONFIGURATIONS}

ENABLE_FEATURES=$(cat<<EOF
PROBE_MANUALLY
LCD_BED_LEVELING
MESH_BED_LEVELING
NOZZLE_PARK_FEATURE
ADVANCED_PAUSE_FEATURE
FILAMENT_RUNOUT_SENSOR
GLOBAL_MESH_Z_OFFSET
PROBE_OFFSET_WIZARD
BABYSTEPPING
BABYSTEP_ZPROBE_OFFSET
BABYSTEP_GLOBAL_Z
EOF
)

for FEATURE in ${ENABLE_FEATURES}; do
    sed -i "s|^\( *\)//* *\(#define *${FEATURE}\)|\1\2|g" ${CONFIGURATIONS}
done

DISABLE_FEATURES=$(cat<<EOF
MANUAL_PROBE_START_Z
EOF
)

for FEATURE in ${DISABLE_FEATURES}; do
    sed -i "s|^\(.*#define *${FEATURE}\)|// \1|g" ${CONFIGURATIONS}
done


if test "${INTERACTIVE:-}" = true; then
    read -p"proceeding to view enabled/disabled features: "
    grep '#define' ${CONFIGURATIONS} --color=always | less || true
fi

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

if test "${INTERACTIVE:-}" = true; then
    echo "select environment: " 1>&2
    select ENV in ${ENVS}; do
        break
    done
else
    ENV=$(head -1 <<< "${ENVS}")
fi

rm -f "${MARLIN_DIR}"/.pio/build/*/firmware*
platformio run -e "${ENV}"

echo "note: built using platformio env: ${ENV}"
