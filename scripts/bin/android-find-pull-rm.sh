#!/bin/bash

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


FIND_ARGS=${*}
echo ${FIND_ARGS}

adb shell command -v find

OLDIFS=$IFS
export ADB_CMD=adb
sudo adb shell find ${FIND_ARGS} -print0 \
     | xargs -r0 -I Z sh -c "sudo $(which adb-pull-rm.sh) 'Z'"
