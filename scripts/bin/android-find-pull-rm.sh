#!/bin/bash -x

# example to pull videos and images:
# android-find-pull-rm.sh sdcard/DCIM -name "'*mp4'" -o -name "'*jpg'"

FIND_ARGS=${*}
echo ${FIND_ARGS}

sudo adb shell find ${FIND_ARGS} \
    | sed 's/[[:space:]]//g' \
    | tee -a $(basename ${0}).log \
    | xargs -L1 adb-pull-rm.sh
