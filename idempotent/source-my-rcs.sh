#!/bin/bash -x

LINE="source ${HOME}/.my_profile.rc"
PROFILERC="${HOME}/.profile"

if ! grep -F "${LINE}" "${PROFILERC}"; then
    echo "${LINE}"| tee -a "${PROFILERC}"
fi


BASHRC="${HOME}/.bashrc"
LINE="source ${HOME}/.my_bashrc.sh"

if ! grep -F "${LINE}" "${PROFILERC}"; then
    echo "${LINE}"| tee -a "${PROFILERC}"
fi
