#!/bin/bash

STUMPWM_TOP="${HOME}/repos/stumpwm/"
INITS_TOP="${STUMPWM_TOP}/inits/"

ln -s "${INITS_TOP}/zathurarc" "${HOME}/.config/zathura/"
ln -s "${INITS_TOP}/roxterm.sourceforge.net" "${HOME}/.config/"

ln -s "${INITS_TOP}/.bash_aliases" "${HOME}/"
ln -s "${INITS_TOP}/.pythonrc.py" "${HOME}/"
ln -s "${INITS_TOP}/.inputrc" "${HOME}/" 
ln -s "${INITS_TOP}/.my_bashrc.sh" "${HOME}/" 

