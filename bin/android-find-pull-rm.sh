#!/bin/bash -x

set -euo pipefail

ADB_CMD=(adb)
FIND_CMD=(find)

PULL_IMAGES_PATH=sdcard/DCIM/Camera
EXTS=jpg,jpeg,mp4
SAVE_HOME=${ANDROID_MEDIA_HOME:-}

while getopts "s:p:e:nh" OPT; do
    case ${OPT} in
        p)
            PULL_IMAGES_PATH=${OPTARG}
            ;;
        s)
            DEVICE_ID=${OPTARG}
            ;;
        e)
            EXTS=${OPTARG}
            ;;
        n)
            # test mode
            NO_PULL=true
            ;;
        d)
            SAVE_HOME=${OPTARG}
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


FIND_CMD+=(${PULL_IMAGES_PATH})

for EXT in $(sed 's/,/ /g' <<< ${EXTS}); do
    FIND_CMD+=("-iname")
    FIND_CMD+=("*.${EXT}")
    FIND_CMD+=("-o")
done
unset FIND_CMD[-1] # remove the last -o

if test -z "${DEVICE_ID:-}"; then
    DEVICE_ID=$(adb shell getprop ro.serialno)
fi
ADB_CMD+=("-s" "${DEVICE_ID}")

test -d "${SAVE_HOME}"
SAVE_DIR="${SAVE_HOME}/${DEVICE_ID}"
mkdir -p "${SAVE_DIR}"
cd "${SAVE_DIR}"

"${ADB_CMD[@]}" shell command -v find

if test -z "${NO_PULL:-}"; then
    PULL=(xargs -L1 $(which adb-pull-rm.sh))
else
    PULL=(cat)
fi

"${ADB_CMD[@]}" shell "${FIND_CMD[@]}" | "${PULL[@]}"
