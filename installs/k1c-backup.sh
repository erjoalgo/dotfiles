#!/bin/bash -x

set -euo pipefail

# INSTALL=true
while getopts "irh" OPT; do
    case ${OPT} in
    i)
        INSTALL=true
        ;;
    r)
        INSTALL=false
        ;;
    h)
        less "$0"
        exit 0
        ;;
    *)
        echo "unrecognized flag: ${OPT}" && exit ${LINENO}
        ;;
    esac
done
shift $((OPTIND -1))

SECS=$((60 * 60))

if test "${INSTALL:-}" = true; then
    install-systemd-service.sh k1c-backup -u <<EOF
[Unit]
Description=Back up files from the K1C

[Service]
ExecStart=$(realpath $0) -r
Restart=always
RestartSec=${SECS}
Environment=PATH=${PATH}

[Install]
WantedBy=default.target

EOF
    exit 0
fi

AFS_ROOT=/afs/asus.erjoalgo.com/public/k1c
K1C=k1c.arpa

sshpass -p creality_2023 rsync -rv root@${K1C}:/usr/data ${AFS_ROOT}

cd ${AFS_ROOT}/timelapse
for FILE in *main_output*; do
    NO_EXT="${FILE%.*}"
    EXT=${FILE##*.}
    SUM=$(md5sum "${FILE}" | cut -f1 -d' ')
    NEW_NAME="${SUM}.${EXT}"
    echo mv -n "${FILE}" "${NEW_NAME}"
done

