#!/bin/bash

FN="/etc/systemd/logind.conf"

CHANGED=""
for LINE in "HandleLidSwitch=ignore"\
		"HandlePowerKey=ignore"; do

    
    if ! grep -F "${LINE}" "${FN}" >/dev/null; then
	echo "adding line: ${LINE}"
	echo "${LINE}" | \
	    sudo tee -a "${FN}"
	CHANGED=true
    fi
    
done

if test -n ${CHANGED}; then
    sudo service systemd-logind restart
fi
