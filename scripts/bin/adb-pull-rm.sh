#!/bin/bash -x

set -euo pipefail

function usage  {
    echo "usage: $(basename ${0}) <ANDROID_FILE_PATH>"
}

if test $# -ne 1; then
    usage
    exit 1
fi

ADB_CMD=${ADB_CMD:-"sudo adb"}
ANDROID_PATH="${1}"
NAME=$(basename "${ANDROID_PATH}")

# TODO make sure 'adb shell ls' succeeds
OUT=$(${ADB_CMD} shell ls "${ANDROID_PATH}")

echo "${ANDROID_PATH}"

! grep -i "no such file" <<< "${OUT}" || false

test -e "${NAME}" || ${ADB_CMD} pull "${ANDROID_PATH}"

MD5_HDD=$(md5sum "${NAME}" | cut -f1)
MD5_ANDROID=$(${ADB_CMD} shell md5sum "${ANDROID_PATH}" | cut -f1)

if test "${MD5_ANDROID}" = "${MD5_HDD}"; then
    ${ADB_CMD} shell rm "${ANDROID_PATH}"
else
    echo "unexpected checksum for ${ANDROID_PATH}: ${MD5_ANDOIRD} vs ${MD5_HDD}"
    echo "${ANDROID_PATH}\t$(${ADB_CMD} shell du -b ${ANDROID_PATH})"
    echo "${NAME}\t$(du -b ${NAME})"
fi
