#!/usr/bin/env bash
# grow-vm-disk-guest.sh
set -euo pipefail

# Needs growpart: apt install cloud-guest-utils  (Debian/Ubuntu)
#                 dnf install cloud-utils-growpart (Fedora/RHEL)
command -v growpart >/dev/null || { echo "install growpart first"; exit 1; }

ROOT_SRC=$(findmnt -no SOURCE /)
FSTYPE=$(findmnt -no FSTYPE /)

grow_partition() {   # arg: partition device, e.g. /dev/vda2
  local part="$1"
  local name disk partnum
  name=$(basename "$(readlink -f "$part")")
  disk="/dev/$(lsblk -no PKNAME "/dev/$name")"
  partnum=$(cat "/sys/class/block/$name/partition")
  echo "Growing $disk partition $partnum"
  growpart "$disk" "$partnum" || true   # exits non-zero if nothing to do
}

if [[ "$ROOT_SRC" == /dev/mapper/* ]]; then
  # LVM: grow the partition(s) backing each PV, then PV, then LV
  LV=$(readlink -f "$ROOT_SRC")
  VG=$(lvs --noheadings -o vg_name "$LV" | xargs)
  for pv in $(pvs --noheadings -o pv_name -S "vg_name=$VG" | xargs); do
    grow_partition "$pv"
    pvresize "$pv"
  done
  lvextend -r -l +100%FREE "$LV"     # -r also resizes the filesystem
else
  grow_partition "$ROOT_SRC"
  case "$FSTYPE" in
    ext2|ext3|ext4) resize2fs "$ROOT_SRC" ;;
    xfs)            xfs_growfs / ;;
    btrfs)          btrfs filesystem resize max / ;;
    *) echo "Unsupported fs: $FSTYPE"; exit 1 ;;
  esac
fi

df -h /
