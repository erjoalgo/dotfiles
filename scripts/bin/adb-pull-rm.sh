#!/bin/bash

set -euo pipefail

function usage  {
    echo "usage: $(basename ${0}) <ANDROID_FILE_PATH>"
}

PATH_PREFIX=""
while getopts "hp:" OPT; do
    case ${OPT} in
    p)
        PATH_PREFIX=${OPTARG}
        ;;
    h)
        less $0
        usage
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

ADB_CMD=${ADB_CMD:-"sudo adb"}

for FILE in ${*}; do
    echo ${FILE}
    ANDROID_PATH="${PATH_PREFIX}/$(tr -d '\r' <<< ${FILE})"
    NAME=$(basename "${ANDROID_PATH}")

    # TODO make sure 'adb shell ls' succeeds
    OUT=$(${ADB_CMD} shell ls "${ANDROID_PATH}")

    echo "${ANDROID_PATH}"

    if grep -i "no such file" <<< "${OUT}"; then
        echo "${OUT}"
        exit ${LINENO}
    fi
    if ! test -e "${NAME}"; then
        ${ADB_CMD} pull "${ANDROID_PATH}"
    fi

    MD5_HDD=$(md5sum "${NAME}" | tr ' ' '\t' | cut -f1)
    MD5_ANDROID=$(${ADB_CMD} shell md5sum "${ANDROID_PATH}" | tr ' ' '\t' | cut -f1)

    if test "${MD5_ANDROID}" = "${MD5_HDD}"; then
        ${ADB_CMD} shell rm "${ANDROID_PATH}"
    else
        echo "unexpected checksum for ${ANDROID_PATH}: ${MD5_ANDROID} vs ${MD5_HDD}"
        echo "${ANDROID_PATH}\t$(${ADB_CMD} shell du -b ${ANDROID_PATH})"
        echo "${NAME}\t$(du -b ${NAME})"
    fi
done
