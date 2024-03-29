#!/bin/bash

set -euo pipefail

function usage  {
    echo "usage: $(basename ${0}) <ANDROID_FILE_PATH>"
}

ADB_CMD=("adb")
if test -n "${ADB_DEVICE:-}"; then
    ADB_CMD+=("-s" "${ADB_DEVICE}")
fi

PATH_PREFIX=""

FILE="${*}"
echo ${FILE}
ANDROID_PATH="${PATH_PREFIX}/$(tr -d '\r' <<< ${FILE})"
LOCAL_NAME=$(sed 's|//*|/|g'  <<< "$(pwd)/${ANDROID_PATH}")

# TODO make sure 'adb shell ls' succeeds
OUT=$(${ADB_CMD[@]} shell ls "\"${ANDROID_PATH}\"")
echo "${ANDROID_PATH}"

if grep -i "no such file\|does not exist" <<< "${OUT}"; then
    echo "${OUT}"
    exit ${LINENO}
fi

if ! test -e "${LOCAL_NAME}"; then
    mkdir -p $(dirname "${LOCAL_NAME}")
    ${ADB_CMD[@]} pull "${ANDROID_PATH}" "${LOCAL_NAME}"
fi

MD5_HDD=$(md5sum "${LOCAL_NAME}" | tr ' ' '\t' | cut -f1)
MD5_ANDROID=$(${ADB_CMD[@]} shell md5sum "\"${ANDROID_PATH}\"" | tr ' ' '\t' | cut -f1)

if test "${MD5_ANDROID}" = "${MD5_HDD}"; then
    ${ADB_CMD[@]} shell rm "\"${ANDROID_PATH}\""
    echo "success!"
else
    echo "unexpected checksum for ${ANDROID_PATH}: ${MD5_ANDROID} vs ${MD5_HDD}"
    echo "${ANDROID_PATH}\t$(${ADB_CMD[@]} shell du -b ${ANDROID_PATH})"
    echo "${LOCAL_NAME}\t$(du -b ${LOCAL_NAME})"
fi
