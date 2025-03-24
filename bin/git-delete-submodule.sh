#!/bin/bash -x

set -euo pipefail

while getopts "d:ih" OPT; do
    case ${OPT} in
    d)
        SUBMODULE_PATH=${OPTARG}
        ;;
    i)
        IN_PLACE=true
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
SUBMODULE_PATH=${SUBMODULE_PATH:-${1}} && shift

# ensure submodule path makes sense since we will delete it
[[ "${SUBMODULE_PATH}" =~ submodules/* ]]

function remove-submodule-block {
    CONFIG_FILE=${1} && shift
    SUBMODULE_PATH=${1} && shift
    if ! A=$(grep -Fn "[submodule \"${SUBMODULE_PATH}\"]" "${CONFIG_FILE}"  \
                 | cut -f1 -d:); then
        echo "submodule block already removed from ${CONFIG_FILE}"
        return 0
    fi
    if B_DELTA=$(tail +$((${A} + 1)) < "${CONFIG_FILE}" | \
                    grep -Pn '^[[]' | head -1 |  \
                    cut -f1 -d:); then
        B=$((${A} + ${B_DELTA} - 1))
    else
        B=$(wc -l "${CONFIG_FILE}" | cut -f1 -d' ')
    fi
    if test -n "${IN_PLACE:-}"; then
        IN_PLACE_OPT="-i"
    else
        IN_PLACE_OPT="";
    fi

    sed ${IN_PLACE_OPT} "${A},${B} d" "${CONFIG_FILE}"
}

remove-submodule-block .gitmodules "${SUBMODULE_PATH}"
remove-submodule-block .git/config "${SUBMODULE_PATH}"

git rm --cached "${SUBMODULE_PATH}" || true

RM_CMD=(rm -rf "${SUBMODULE_PATH}")
if test -z "${IN_PLACE:-}"; then
    RM_CMD=(echo ${RM_CMD[@]})
fi
echo ${RM_CMD[@]}
read -p "confirm running the dangerous rm rf -command above: "
${RM_CMD[@]}

echo "Don't forget to commit your changes afterward!"
