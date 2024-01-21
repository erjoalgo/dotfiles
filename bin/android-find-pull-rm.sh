#!/bin/bash -x

ADB_CMD=(adb)
FIND_CMD=()
FIND_ARGS=()
ADB_DEVICE=

while getopts "s:p:e:nh" OPT; do
    case ${OPT} in
        p)
            # find path
            FIND_CMD=(find ${OPTARG})
            ;;
        s)
            # device
            export ADB_DEVICE=${OPTARG}
            ADB_CMD+=("-s" "${OPTARG}")
            ;;
        e)
            for EXT in $(sed 's/,/ /g' <<< ${OPTARG}); do
                if test "${#FIND_ARGS[@]}" -gt 0; then
                    FIND_ARGS+=("-o")
                fi
                FIND_ARGS+=("-name")
                FIND_ARGS+=("*.${EXT}")
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

test -n "${FIND_PATH:-}"
FIND_CMD+=("${FIND_ARGS[@]}")

"${ADB_CMD[@]}" shell command -v find

if test -z "${NO_PULL:-}"; then
    PULL=(xargs -L1 $(which adb-pull-rm.sh))
else
    PULL=(cat)
fi

"${ADB_CMD[@]}" shell "${FIND_CMD[@]}" | "${PULL[@]}"
