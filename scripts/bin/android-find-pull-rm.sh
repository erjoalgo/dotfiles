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

sudo adb shell find ${FIND_ARGS} \
    | sed 's/[[:space:]]//g' \
    | tee -a $(basename ${0}).log \
    | xargs -rL1 adb-pull-rm.sh
