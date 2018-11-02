#!/bin/bash -x

test -n "${1}" || { echo "usage: record-audio.sh FILENAME"; exit ${LINENO}; }
FN="${1}.ogg"
arecord -f cd -t raw | oggenc -r -o "${FN}" -
