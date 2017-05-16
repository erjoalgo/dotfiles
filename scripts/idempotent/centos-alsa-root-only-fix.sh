#!/bin/bash -x

LINE="chmod -R a+rw /dev/snd/*"
FILE="/etc/rc.d/rc.local"

if ! grep -F "${LINE}" "${FILE}"; then
    sudo tee -a ${FILE} <<< "${LINE}"
fi

