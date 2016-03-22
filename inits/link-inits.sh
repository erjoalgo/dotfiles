#!/bin/bash -x

STUMPWM_TOP="${HOME}/repos/stumpwm"
INITS_TOP="${STUMPWM_TOP}/inits"

ln -sf "${INITS_TOP}/zathurarc" "${HOME}/.config/zathura/"
ln -sf "${INITS_TOP}/roxterm.sourceforge.net" "${HOME}/.config/"

ln -sf "${INITS_TOP}/.bash_aliases" "${HOME}/"
ln -sf "${INITS_TOP}/.pythonrc.py" "${HOME}/"
ln -sf "${INITS_TOP}/.inputrc" "${HOME}/" 
ln -sf "${INITS_TOP}/.my-bashrc" "${HOME}/" 
ln -sf "${INITS_TOP}/.my-bash-funs" "${HOME}/" 

ln -sf "${INITS_TOP}/.xinitrc" "${HOME}/" 
ln -sf "${INITS_TOP}/.my-profile" "${HOME}/" 
ln -sf "${INITS_TOP}/.my-startups.sh" "${HOME}/" 

ln -sf "${INITS_TOP}/.my-bash-completions" "${HOME}"


ln -sf "${STUMPWM_TOP}/.stumpwmrc" "${HOME}/"

${STUMPWM_TOP}/idempotent/source-my-rcs.sh
