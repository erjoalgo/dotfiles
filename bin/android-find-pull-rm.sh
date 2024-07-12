#!/bin/bash -x

set -euo pipefail

ADB_CMD=(adb)
FIND_CMD=(find)
ADB_DEVICE=

while getopts "s:p:e:nh" OPT; do
    case ${OPT} in
        p)
            # find path
            FIND_CMD+=(${OPTARG})
            _FIND_PATH_SET=true
            ;;
        s)
            # device
            export ADB_DEVICE=${OPTARG}
            ADB_CMD+=("-s" "${OPTARG}")
            ;;
        e)
            IS_FIRST=true
            for EXT in $(sed 's/,/ /g' <<< ${OPTARG}); do
                if test ${IS_FIRST} != true; then
                    FIND_CMD+=("-o")
                else
                    IS_FIRST=false
                fi
                FIND_CMD+=("-iname")
                FIND_CMD+=("*.${EXT}")
            done
            ;;
        n)
            # test mode
            NO_PULL=true
            ;;
        h)
            # example to pull videos and images:
            # android-find-pull-rm.sh sdcard/DCIM -name "'*mp4'" -o -name "'*jpg'"
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

test "${_FIND_PATH_SET:-}" = true

"${ADB_CMD[@]}" shell command -v find

if test -z "${NO_PULL:-}"; then
    PULL=(xargs -L1 $(which adb-pull-rm.sh))
else
    PULL=(cat)
fi

"${ADB_CMD[@]}" shell "${FIND_CMD[@]}" | "${PULL[@]}"
