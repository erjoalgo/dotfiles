#!/bin/bash

if test $# -ne 1; then
    echo "invalid number of arguments" && exit ${LINENO}
fi

ANDROID_PATH="${1}"
NAME=$(basename "${ANDROID_PATH}") || exit ${LINENO}

ADB="sudo adb"

# TODO make sure 'adb shell ls' succeeds
OUT=$(${ADB} shell ls "${ANDROID_PATH}") || exit ${LINENO}

echo "${ANDROID_PATH}"

if grep -i "no such file"<<<"${OUT}"; then
    echo  "the android path ${ANDROID_PATH} doesn't exist" && exit 0
fi


if ! test -e "${NAME}"; then
    ${ADB} pull "${ANDROID_PATH}" || exit ${LINENO}
fi

MD5_HDD=$(md5sum "${NAME}" | sed -E 's/[[:space:]]+/\t/g' | cut -f1) || exit ${LINENO}
MD5_ANDROID=$(${ADB} shell md5sum "${ANDROID_PATH}" | sed -E 's/[[:space:]]+/\t/g' | cut -f1) || exit ${LINENO}

if test ${MD5_ANDROID} = ${MD5_HDD}; then
    ${ADB} shell rm "${ANDROID_PATH}" || exit ${LINENO}
else
    echo "corruption with ${ANDROID_PATH}" && exit ${LINENO}
fi
