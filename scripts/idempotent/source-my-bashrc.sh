#!/bin/bash

LINE="source ${HOME}/.my-bashrc"
BASHRC="${HOME}/.bashrc"

if ! grep -F "${LINE}" "${BASHRC}"; then
    echo "${LINE}" >> "${BASHRC}"
fi
