#!/bin/bash -x

set -euo pipefail

# based on https://askubuntu.com/questions/996155/

while getopts "h:" OPT; do
    case ${OPT} in
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

function select-luks-partition {
    sudo lsblk -o PATH,NAME,MODEL,SIZE,FSTYPE,MOUNTPOINT |  \
        grep --color=always -E '.*/crypto_LUKS|$' 1>&2
    echo "select LUKS partition" 1>&2
    select LUKS_PARTITION in $(sudo lsblk -o PATH,FSTYPE | grep -F crypto_LUKS | cut -f1 -d' '); do
        echo "${LUKS_PARTITION}"
        break
    done
}

function select-boot-partition {
    echo "select boot partition" > 2
    sudo lsblk -o PATH,NAME,MODEL,SIZE,FSTYPE,MOUNTPOINT |  \
        grep --color=always -E '.*/boot|$' 1>&2
    select BOOT_PARTITION in $(sudo lsblk -o PATH,MOUNTPOINT | grep -F /boot | cut -f1 -d' '); do
        echo "${BOOT_PARTITION}"
        break
    done
}

function partition-uid {
    PARTITION_PATH=${1} && shift
    sudo lsblk -o PATH,UUID | grep -F "${PARTITION_PATH}" | tr -s ' ' '\t' | cut -f2
}

LUKS_PARTITION=${LUKS_PARTITION:-$(select-luks-partition)}
BOOT_PARTITION=${BOOT_PARTITION:-$(select-boot-partition)}
BOOT_PARTITION_UID=$(partition-uid "${BOOT_PARTITION}")
BOOT_PARTITION_PATH="/dev/disk/by-uuid/${BOOT_PARTITION_UID}"

ls "${LUKS_PARTITION}"
ls "${BOOT_PARTITION}"
ls "${BOOT_PARTITION_PATH}"

# My /boot partition is /dev/sda1.
# My LVM volume that I want to decrypt is /dev/sda3.
KEY_FILE=/boot/keyfile

sudo dd if=/dev/urandom of=${KEY_FILE} bs=1024 count=4
# Then set read permission for root and nothing for anyone else:

sudo chmod 0400 "${KEY_FILE}"

# You will then be prompted to type the encryption password.

# Find the UUID of the /boot partition with the following comamnd (this one doesn't require you be root):

# sudo ls -l /dev/disk/by-uuid/
# Here's an example of what the output looks like (it's not the actual output as I grabbed this from another machine):

# $ ls -l /dev/disk/by-uuid/
# total 0
# lrwxrwxrwx 1 root root 10 Jan 15 03:36 025c66a2-c683-42c5-b17c-322c2188fe3f -> ../../sda2
# lrwxrwxrwx 1 root root 10 Jan 15 03:36 9e7a7336-3b81-4bbe-9f1a-d43415df1ccb -> ../../sda1
# Then edit /etc/crypttab with your favourite editor:



# sda3_crypt UUID=025c66a2-c683-42c5-b17c-322c2188fe3f /dev/disk/by-uuid/9e7a7336-3b81-4bbe-9f1a-d43415df1ccb:/keyfile luks,keyscript=/lib/cryptsetup/scripts/passdev
LINE=$(grep -Po "(?<=# )sda3_crypt UUID=[^ ]+" /etc/crypttab | head -1)
sudo sed -i 's/^[^#]/# \&/g' /etc/crypttab
sudo insert-text-block \
     '# 3a00b570-d55a-4c81-aaf5-8cfc9fe1c6df-auto-decrypt-luks'  \
     /etc/crypttab<<EOF
${LINE} ${BOOT_PARTITION_PATH}:/$(basename ${KEY_FILE}) luks,keyscript=/lib/cryptsetup/scripts/passdev
EOF

sudo cryptsetup -v luksAddKey "${LUKS_PARTITION}" "${KEY_FILE}"

sudo update-initramfs -u
