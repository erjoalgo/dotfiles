#!/bin/bash -x

LINE="HandleLidSwitch=ignore"
FN="/etc/systemd/logind.conf"

if ! grep -F "${LINE}" "${FN}"; then
    echo "${LINE}" | \
	sudo tee -a "${FN}"
fi
