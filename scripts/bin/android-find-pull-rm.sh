#!/bin/bash -x

while getopts "n:h" OPT; do
    case ${OPT} in
        n)
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


adb shell command -v find

export ADB_CMD=adb

if test -z "${NO_PULL:-}"; then
    sudo adb shell "find ${@}" \
        | xargs -L1 $(which adb-pull-rm.sh)
else
    sudo adb shell "find ${@}"
fi
