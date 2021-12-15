#!/bin/bash -x

while getopts "h" OPT; do
    case ${OPT} in
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
sudo adb shell "find ${@}" \
     | xargs -L1 $(which adb-pull-rm.sh)
