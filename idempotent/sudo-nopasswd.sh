#!/bin/bash -x

#ealfonso ALL=(ALL:ALL) NOPASSWD:ALL

LINE="${USER} ALL=(ALL:ALL) NOPASSWD:ALL"
SUDOERS="/etc/sudoers"
if ! sudo grep -F "${LINE}" "${SUDOERS}"; then
    echo "exit status: $?"
    echo "${LINE}"| sudo tee -a "${SUDOERS}"
fi
