#!/bin/bash -x

STUMPWM_TOP=$(dirname $(dirname $(dirname $(readlink -f $0))))
INITS_TOP="${STUMPWM_TOP}/inits"

for LINK in \
    .bash_aliases \
    .pythonrc.py \
    .inputrc \
    .my-bashrc \
    .my-bash-funs \
    .xmodmap \
    .xinitrc \
    .bash_profile \
    .my-startups.sh \
    .my-bash-completions \
    .gitconfig \
    .tmux.conf \
    .xscreensaver \
    ; do \

    ln -sf "${INITS_TOP}/${LINK}" "${HOME}/"

done
    
ln -sf "${INITS_TOP}/zathurarc" "${HOME}/.config/zathura/"
ln -sf "${INITS_TOP}/roxterm.sourceforge.net" "${HOME}/.config/"
ln -sf "${INITS_TOP}/keynavs/.keynavrc.mine" "${HOME}/.keynavrc"
ln -sf "${STUMPWM_TOP}/lisp/.stumpwmrc" "${HOME}/"
ln -sf "${HOME}/git/dotemacs/lisp/.emacs" "${HOME}/"

#link the top itself
#if existss and is a directory, link will be put inside the directory symlink
if ! test -d "${HOME}/.stumpwmrc.d"; then
    ln -sf "${STUMPWM_TOP}" "${HOME}/.stumpwmrc.d"
fi
