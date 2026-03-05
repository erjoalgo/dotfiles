#!/bin/bash -x

set -euo pipefail

while getopts "h" OPT; do
    case ${OPT} in
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

RESOLUTION=${1} && shift

WIDTH=$(cut -f1 -dx  <<< "${RESOLUTION}")
HEIGHT=$(cut -f2 -dx  <<< "${RESOLUTION}")

test -n "${WIDTH:-}"
test -n "${HEIGHT:-}"

for i in ScreenWidth,${WIDTH}  \
                     ScreenHeight,"${HEIGHT}"; do
    IFS=",";
    set -- $i;
    VARNAME=${1} && shift
    VALUE=${1} && shift
    sed -i "s/^${VARNAME} *=.*/${VARNAME} = ${VALUE}/"  \
        ~/.config/mupen64plus/mupen64plus.cfg
done
