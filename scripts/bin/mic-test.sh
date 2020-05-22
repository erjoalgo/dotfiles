#!/bin/bash -x

set -euo pipefail


DEVICES=$(sudo arecord -l |  \
              grep ^card |  \
              sed 's/^card \([0-9]*\).*device \([0-9]*\).*/\1,\2/g')
TEST_WAV=/tmp/test-mic.wav

for DEVICE in ${DEVICES}; do
    DEVICE_SPEC="hw:${DEVICE}"
    sudo arecord -f S16_LE -d 10 -r 16000 -c 2 --device=${DEVICE_SPEC}  \
         ${TEST_WAV}
    aplay ${TEST_WAV}
done
