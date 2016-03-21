#!/bin/bash -x

LINE="source ${HOME}/.my-profile"
PROFILE="${HOME}/.profile"

if ! grep -F "${LINE}" "${PROFILE}"; then
    echo "${LINE}"| tee -a "${PROFILE}"
fi


BASHRC="${HOME}/.bashrc"
LINE="source ${HOME}/.my-bashrc"

if ! grep -F "${LINE}" "${BASHRC}"; then
    echo "${LINE}"| tee -a "${BASHRC}"
fi
