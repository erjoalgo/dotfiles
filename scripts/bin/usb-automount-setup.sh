#!/bin/bash -x

set -euo pipefail

while getopts "l:d:h" OPT; do
    case ${OPT} in
        p)
            PARTITION=${OPTARG}
            ;;
        l)
            SYMLINK=${OPTARG}
            ;;
        d)
            HOMEDIR_PATH=${OPTARG}
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done


SYMLINK=${SYMLINK:-${HOME}/.usb-drive-symlink}
if test -z "${PARTITION:-}"; then
    select PARTITION in $(sudo blkid | cut -f1 -d:); do
        break
    done
fi


MOUNT_POINT=$(udev-gen-rule-for-stick.sh -M -p "${PARTITION}" |  \
                  tee /dev/stderr |  \
                  grep -Po "(?<=mounting to ).*")

test -d ${MOUNT_POINT}

if test -n "${SYMLINK:-}"; then
   test -L "${SYMLINK}" && sudo unlink "${SYMLINK}"
   if ! test -e "${SYMLINK}"; then
       test -n "${HOMEDIR_PATH:-}" ||  \
           read -p"enter relative path to home directory within external drive: " \
                HOMEDIR_PATH
       HOMEDIR="${MOUNT_POINT}/${HOMEDIR_PATH}"
       test -d "${HOMEDIR}"
       ln -sf "${HOMEDIR}" "${SYMLINK}";
   fi
fi

sudo mount "${PARTITION}" "${MOUNT_POINT}" || true

for SECRET in \
    .password-store \
    .gnupg; do

    DEST="${HOME}/${SECRET}"
    SRC="${SYMLINK}/nosync/${SECRET}"

    if test -d "${DEST}" -a ! -L "${DEST}"; then
        CMD="mv ${DEST} ${DEST}.bak"
        read -p "${CMD}: "
        ${CMD}
    fi

    if ! test -d "${SRC}"; then
        echo "warning: missing secret at ${SRC}"
    else
        ln -sfT "${SRC}" "${DEST}"
    fi
done
