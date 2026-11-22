#!/bin/bash

set -euo pipefail

# Alert when /boot is nearly full or dpkg has half-configured packages:
# either one means kernel upgrades are failing, which otherwise only
# shows up at the next reboot.

THRESHOLD=${1:-80}
PROBLEMS=()

USED=$(df --output=pcent /boot | tail -1 | tr -dc 0-9)
MOUNT=$(df --output=target /boot | tail -1)
if test "${USED}" -ge "${THRESHOLD}"; then
    if test "${MOUNT}" = /boot; then
        PROBLEMS+=("/boot is ${USED}% full")
    else
        PROBLEMS+=("${MOUNT} (holding /boot) is ${USED}% full")
    fi
fi
if test -n "$(dpkg --audit)"; then
    PROBLEMS+=("dpkg has unconfigured packages, see dpkg --audit")
fi

if test ${#PROBLEMS[@]} -gt 0; then
    MSG="$(hostname): $(printf '%s; ' "${PROBLEMS[@]}")"
    MSG=${MSG%; }
    echo "${MSG}" >&2
    "$(dirname "$(realpath "$0")")/alert-now" -n "${MSG}"
    exit 1
fi
