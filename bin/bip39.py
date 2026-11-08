#!/usr/bin/env bash
# grow-vm-disk.sh
set -euo pipefail

virsh() { command virsh -c "${LIBVIRT_URI:-qemu:///system}" "$@"; }
human() { numfmt --to=iec --suffix=B "$1"; }

# Globals set by the functions below
VM="" STATE="" TARGET="" SRC="" CAP=0 ALLOC=0 AVAIL="" NEW=0

# ---------- guest script (sent over, never saved on the guest) ----------
guest_script() {
cat <<'GUEST'
set -euo pipefail

command -v growpart >/dev/null || { echo "install growpart first (cloud-guest-utils / cloud-utils-growpart)"; exit 1; }

ROOT_SRC=$(findmnt -no SOURCE /)
FSTYPE=$(findmnt -no FSTYPE /)

grow_partition() {   # arg: partition device, e.g. /dev/vda2
  local part="$1" name disk partnum
  name=$(basename "$(readlink -f "$part")")
  disk=$(lsblk -no PKNAME "/dev/$name")
  partnum=$(cat "/sys/class/block/$name/partition")
  echo 1 | sudo tee "/sys/class/block/$disk/device/rescan" >/dev/null || true
  echo "Growing /dev/$disk partition $partnum"
  sudo growpart "/dev/$disk" "$partnum" || true   # non-zero when nothing to do
}

if [[ "$ROOT_SRC" == /dev/mapper/* ]]; then
  LV=$(readlink -f "$ROOT_SRC")
  VG=$(sudo lvs --noheadings -o vg_name "$LV" | xargs)
  for pv in $(sudo pvs --noheadings -o pv_name -S "vg_name=$VG" | xargs); do
    grow_partition "$pv"
    sudo pvresize "$pv"
  done
  sudo lvextend -r -l +100%FREE "$LV"
else
  grow_partition "$ROOT_SRC"
  case "$FSTYPE" in
    ext2|ext3|ext4) sudo resize2fs "$ROOT_SRC" ;;
    xfs)            sudo xfs_growfs / ;;
    btrfs)          sudo btrfs filesystem resize max / ;;
    *) echo "Unsupported fs: $FSTYPE"; exit 1 ;;
  esac
fi

df -h /
GUEST
}

# ---------- host-side steps ----------
check_requirements() {
  command -v virsh >/dev/null || { echo "virsh not found"; exit 1; }
}

select_vm() {
  local vms
  mapfile -t vms < <(virsh list --all --name | sed '/^$/d')
  [[ ${#vms[@]} -gt 0 ]] || { echo "No VMs found"; exit 1; }
  echo "Select a VM:"
  PS3="VM number: "
  select VM in "${vms[@]}"; do [[ -n "${VM:-}" ]] && break; done
  STATE=$(virsh domstate "$VM")
  echo "VM: $VM ($STATE)"
}

select_disk() {
  local disks choice
  mapfile -t disks < <(virsh domblklist "$VM" --details | awk 'NR>2 && $2=="disk" {print $3 "  " $4}')
  [[ ${#disks[@]} -gt 0 ]] || { echo "No disks found"; exit 1; }
  if [[ ${#disks[@]} -eq 1 ]]; then
    choice="${disks[0]}"
  else
    echo "Select a disk:"
    PS3="Disk number: "
    select choice in "${disks[@]}"; do [[ -n "${choice:-}" ]] && break; done
  fi
  TARGET=$(awk '{print $1}' <<<"$choice")
  SRC=$(awk '{print $2}' <<<"$choice")
}

show_disk_info() {
  local info
  info=$(virsh domblkinfo "$VM" "$TARGET")
  CAP=$(awk '/Capacity:/ {print $2}' <<<"$info")
  ALLOC=$(awk '/Allocation:/ {print $2}' <<<"$info")

  echo
  echo "Disk:              $TARGET ($SRC)"
  echo "Current size:      $(human "$CAP")"
  echo "Allocated on host: $(human "$ALLOC")"

  AVAIL=""
  if [[ -f "$SRC" ]]; then
    AVAIL=$(df -B1 --output=avail "$(dirname "$SRC")" | tail -1 | tr -d ' ')
    echo "Host free space:   $(human "$AVAIL") (on $(df --output=target "$(dirname "$SRC")" | tail -1))"
  else
    echo "Host free space:   n/a (block device or LVM volume; check vgs/lvs)"
  fi
}

prompt_new_size() {
  local input growth ok
  echo
  read -rp "New size (absolute like 60G, or increment like +20G): " input
  input="${input// /}"

  if [[ "$input" == +* ]]; then
    NEW=$(( CAP + $(numfmt --from=iec "${input#+}") ))
  else
    NEW=$(numfmt --from=iec "$input")
  fi

  if (( NEW <= CAP )); then
    echo "New size ($(human "$NEW")) must be larger than current ($(human "$CAP"))."
    exit 1
  fi

  growth=$(( NEW - CAP ))
  echo "Growing $TARGET: $(human "$CAP") -> $(human "$NEW") (+$(human "$growth"))"

  if [[ -n "$AVAIL" ]] && (( growth > AVAIL )); then
    echo "WARNING: growth exceeds host free space. Fine for thin/sparse images"
    echo "         only until the guest actually fills the space."
  fi

  read -rp "Proceed? [y/N] " ok
  [[ "$ok" =~ ^[Yy]$ ]] || { echo "Aborted."; exit 0; }
}

resize_disk() {
  if [[ "$STATE" == "running" ]]; then
    virsh blockresize "$VM" "$TARGET" --size "${NEW}B"
  else
    [[ -f "$SRC" ]] || { echo "VM is off and disk isn't a file; resize it manually."; exit 1; }
    qemu-img resize "$SRC" "$NEW"
  fi
  echo
  virsh domblkinfo "$VM" "$TARGET"
}

# ---------- guest-side execution ----------
run_guest_ssh() {
  local guess dest b64
  guess=$(virsh domifaddr "$VM" 2>/dev/null | awk '/ipv4/ {split($4,a,"/"); print a[1]; exit}' || true)
  read -rp "SSH target (user@host) [${guess:+$USER@$guess}]: " dest
  dest="${dest:-${guess:+$USER@$guess}}"
  [[ -n "$dest" ]] || { echo "No target given."; return 1; }

  b64=$(guest_script | base64 -w0)
  ssh -t "$dest" "bash <(echo '$b64' | base64 -d)"
}

run_guest_agent() {
  command -v jq >/dev/null || { echo "jq is required for the guest agent method"; return 1; }
  local b64 pid out
  b64=$(guest_script | base64 -w0)

  pid=$(virsh qemu-agent-command "$VM" \
    "{\"execute\":\"guest-exec\",\"arguments\":{\"path\":\"/bin/bash\",\"arg\":[\"-s\"],\"input-data\":\"$b64\",\"capture-output\":true}}" \
    | jq -r '.return.pid')

  while :; do
    out=$(virsh qemu-agent-command "$VM" \
      "{\"execute\":\"guest-exec-status\",\"arguments\":{\"pid\":$pid}}")
    [[ "$(jq -r '.return.exited' <<<"$out")" == "true" ]] && break
    sleep 1
  done

  jq -r '.return["out-data"] // empty' <<<"$out" | base64 -d
  jq -r '.return["err-data"] // empty' <<<"$out" | base64 -d >&2
  return "$(jq -r '.return.exitcode' <<<"$out")"
}

grow_guest() {
  local method
  echo
  echo "Grow the partition/filesystem inside the guest now?"
  PS3="Method: "
  select method in "SSH" "QEMU guest agent" "Skip"; do [[ -n "${method:-}" ]] && break; done
  case "$method" in
    SSH)                run_guest_ssh ;;
    "QEMU guest agent") run_guest_agent ;;
    Skip)               echo "Skipped. Run the guest script inside the VM when ready." ;;
  esac
}

# ---------- main ----------
main() {
  check_requirements
  select_vm
  select_disk
  show_disk_info
  prompt_new_size
  resize_disk
  grow_guest
}

main "$@"
