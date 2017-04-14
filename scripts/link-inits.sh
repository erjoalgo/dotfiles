#!/bin/bash -x

STUMPWM_TOP=$(dirname $(dirname $(readlink -f $0)))
INITS_TOP="${STUMPWM_TOP}/inits"

ln -sf "${INITS_TOP}/zathurarc" "${HOME}/.config/zathura/"
ln -sf "${INITS_TOP}/roxterm.sourceforge.net" "${HOME}/.config/"

ln -sf "${INITS_TOP}/.bash_aliases" "${HOME}/"
ln -sf "${INITS_TOP}/.pythonrc.py" "${HOME}/"
ln -sf "${INITS_TOP}/.inputrc" "${HOME}/" 
ln -sf "${INITS_TOP}/.my-bashrc" "${HOME}/" 
ln -sf "${INITS_TOP}/.my-bash-funs" "${HOME}/" 
ln -sf "${INITS_TOP}/.xmodmap" "${HOME}/" 


ln -sf "${INITS_TOP}/.xinitrc" "${HOME}/" 
ln -sf "${INITS_TOP}/.bash_profile" "${HOME}/" 
ln -sf "${INITS_TOP}/.my-startups.sh" "${HOME}/" 

ln -sf "${INITS_TOP}/.my-bash-completions" "${HOME}"
ln -s "${INITS_TOP}/.gitconfig" "${HOME}"

ln -sf "${INITS_TOP}/keynavs/.keynavrc.mine" "${HOME}/.keynavrc"

ln -sf "${STUMPWM_TOP}/lisp/.stumpwmrc" "${HOME}/"
ln -sf "${HOME}/git/dotemacs/lisp/.emacs" "${HOME}/"

#link the top itself
#if existss and is a directory, link will be put inside the directory symlink
test ! -e "${HOME}/.stumpwmrc.d" && ln -sf "${STUMPWM_TOP}" "${HOME}/.stumpwmrc.d"
ln -sf "${HOME}/git/dotemacs" "${HOME}/"

