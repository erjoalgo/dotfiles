#!/bin/bash -x

ADB_CMD=adb
while getopts "n:s:h" OPT; do
    case ${OPT} in
        n)
            NO_PULL=true
            ;;
        s)
            ADB_CMD="adb -s ${OPTARG}"
            ;;
        h)
            # example to pull videos and images:
            # android-find-pull-rm.sh sdcard/DCIM -name "'*mp4'" -o -name "'*jpg'"
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

${ADB_CMD} shell command -v find

export ADB_CMD

if test -z "${NO_PULL:-}"; then
    ${ADB_CMD} shell "find ${@}" | xargs -L1 $(which adb-pull-rm.sh)
else
    ${ADB_CMD} shell "find ${@}"
fi
