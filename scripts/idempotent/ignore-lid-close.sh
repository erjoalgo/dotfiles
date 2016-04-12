#!/bin/bash

FN="/etc/systemd/logind.conf"

for LINE in "HandleLidSwitch=ignore"\
		"HandlePowerKey=ignore"; do

    
    if ! grep -F "${LINE}" "${FN}" >/dev/null; then
	echo "adding line: ${LINE}"
	echo "${LINE}" | \
	    sudo tee -a "${FN}"
	sudo service systemd-logind restart
    fi
    
done
