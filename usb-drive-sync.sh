#!/bin/bash -x

set -euo pipefail

UNISON_OPTS=${*}

MNT=${HOME}/.usb-drive-symlink
HOME_TWO_WAY=${HOME}/private-data

mkdir -p ${HOME_TWO_WAY}

unison -dontchmod -perms 0  \
       ${UNISON_OPTS} \
       ${MNT}/sync-two-ways ${HOME_TWO_WAY}

# -merge = Name *.org -> emacsclient --eval  \
    # '(ediff-merge-files-with-ancestor "CURRENT1" "CURRENT2" "CURRENTARCH" nil "NEW")'


ROOT_DEVICE_PATH=$(sed "s/[ 	][ 	]*/ /g" /etc/fstab | grep ' / ' | cut -f1 -d' ')
MACHINE_UUID=$(lsblk ${ROOT_DEVICE_PATH} -o uuid | tail -1)

MNT_ONE_WAY=${MNT}/machines/${MACHINE_UUID}

mkdir -p ${MNT_ONE_WAY}
mkdir -p ${HOME}/private-data-one-way

for ONE_WAY_REL in .bash_history  \
                       private-data-one-way/ \
                   ; do
    SRC=${HOME}/${ONE_WAY_REL}
    DEST=${MNT_ONE_WAY}/${ONE_WAY_REL}
    if test -e ${SRC}; then
        rsync -rv ${SRC} ${DEST}
    fi
done

# TODO rsnapshot
