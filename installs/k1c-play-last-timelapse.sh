#!/bin/bash -x

set -euo pipefail


# DIR=/afs/asus.erjoalgo.com/public/k1c/timelapse/
DIR=/afs/asus.erjoalgo.com/public/k1c/data/creality/userdata/delay_image/video/

BASE=$(ls -1tr "${DIR}" | tail -1)
FILE="${DIR}/${BASE}"

test -e "${FILE}"

celluloid "${FILE}"
