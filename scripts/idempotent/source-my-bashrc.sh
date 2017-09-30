#!/bin/bash

LINE="source ${HOME}/.my-bashrc"
BASHRC="${HOME}/.bashrc"

if ! grep -F "${LINE}" "${BASHRC}"; then
    echo "${LINE}" >> "${BASHRC}"
fi
sed -i '/^HIST\(FILE\)\?SIZE=[0-9]*/d' "${BASHRC}"
