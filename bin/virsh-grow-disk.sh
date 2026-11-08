#!/usr/bin/env bash
# grow-vm-disk-host.sh
set -euo pipefail

command -v virsh >/dev/null || { echo "virsh not found"; exit 1; }

# Use the system libvirt instance by default (override: LIBVIRT_URI=qemu:///session)
virsh() { command virsh -c "${LIBVIRT_URI:-qemu:///system}" "$@"; }
human() { numfmt --to=iec --suffix=B "$1"; }

# --- Pick the VM ---
mapfile -t VMS < <(virsh list --all --name | sed '/^$/d')
[[ ${#VMS[@]} -gt 0 ]] || { echo "No VMs found"; exit 1; }

echo "Select a VM:"
PS3="VM number: "
select VM in "${VMS[@]}"; do
  [[ -n "${VM:-}" ]] && break
done

STATE=$(virsh domstate "$VM")
echo "VM: $VM ($STATE)"

# --- Pick the disk (skips cdroms) ---
mapfile -t DISKS < <(virsh domblklist "$VM" --details | awk 'NR>2 && $2=="disk" {print $3 "  " $4}')
[[ ${#DISKS[@]} -gt 0 ]] || { echo "No disks found"; exit 1; }

if [[ ${#DISKS[@]} -eq 1 ]]; then
  CHOICE="${DISKS[0]}"
else
  echo "Select a disk:"
  PS3="Disk number: "
  select CHOICE in "${DISKS[@]}"; do
    [[ -n "${CHOICE:-}" ]] && break
  done
fi
TARGET=$(awk '{print $1}' <<<"$CHOICE")
SRC=$(awk '{print $2}' <<<"$CHOICE")

# --- Show current sizes ---
INFO=$(virsh domblkinfo "$VM" "$TARGET")
CAP=$(awk '/Capacity:/ {print $2}' <<<"$INFO")
ALLOC=$(awk '/Allocation:/ {print $2}' <<<"$INFO")

echo
echo "Disk:             $TARGET ($SRC)"
echo "Current size:     $(human "$CAP")"
echo "Allocated on host: $(human "$ALLOC")"

AVAIL=""
if [[ -f "$SRC" ]]; then
  AVAIL=$(df -B1 --output=avail "$(dirname "$SRC")" | tail -1 | tr -d ' ')
  echo "Host free space:  $(human "$AVAIL") (on $(df --output=target "$(dirname "$SRC")" | tail -1))"
else
  echo "Host free space:  n/a (disk is a block device or LVM volume; check with vgs/lvs)"
fi

# --- Ask for the new size ---
echo
read -rp "New size (absolute like 60G, or increment like +20G): " INPUT
INPUT="${INPUT// /}"

if [[ "$INPUT" == +* ]]; then
  NEW=$(( CAP + $(numfmt --from=iec "${INPUT#+}") ))
else
  NEW=$(numfmt --from=iec "$INPUT")
fi

if (( NEW <= CAP )); then
  echo "New size ($(human "$NEW")) must be larger than current ($(human "$CAP")). Shrinking isn't supported here."
  exit 1
fi

GROWTH=$(( NEW - CAP ))
echo "Growing $TARGET: $(human "$CAP") -> $(human "$NEW") (+$(human "$GROWTH"))"

if [[ -n "$AVAIL" ]] && (( GROWTH > AVAIL )); then
  echo "WARNING: growth exceeds free space on the host. This is fine for thin/sparse images"
  echo "         only until the guest actually fills the space."
fi

read -rp "Proceed? [y/N] " OK
[[ "$OK" =~ ^[Yy]$ ]] || { echo "Aborted."; exit 0; }

# --- Resize ---
if [[ "$STATE" == "running" ]]; then
  virsh blockresize "$VM" "$TARGET" --size "${NEW}B"
else
  [[ -f "$SRC" ]] || { echo "VM is off and disk isn't a file; resize it manually."; exit 1; }
  qemu-img resize "$SRC" "$NEW"
fi

echo
virsh domblkinfo "$VM" "$TARGET"
echo "Done. Now run the guest-side script inside the VM."
