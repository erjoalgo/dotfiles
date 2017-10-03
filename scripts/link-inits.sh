#!/bin/bash -x

STUMPWM_TOP=$(dirname $(dirname $(readlink -f $0)))
INITS_TOP="${STUMPWM_TOP}/inits"

function safe_ln	{
    SRC=${1} && shift
    DST=${1} && shift
    if test -f ${DST} -a ! -L ${DST}; then
	echo "warning: skipping symlink to existent non-symlink ${DST} of  ${SRC}"
    else
	ln -sf ${SRC} ${DST}
    fi

}

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

    safe_ln "${INITS_TOP}/${LINK}" "${HOME}/"

done

safe_ln "${INITS_TOP}/zathurarc" "${HOME}/.config/zathura/"
safe_ln "${INITS_TOP}/roxterm.sourceforge.net" "${HOME}/.config/"
safe_ln "${INITS_TOP}/keynavs/.keynavrc.mine" "${HOME}/.keynavrc"
safe_ln "${STUMPWM_TOP}/lisp/.stumpwmrc" "${HOME}/"
safe_ln "${HOME}/git/dotemacs/lisp/.emacs" "${HOME}/"

#link the top itself
#if existss and is a directory, link will be put inside the directory symlink
if ! test -d "${HOME}/.stumpwmrc.d"; then
    safe_ln "${STUMPWM_TOP}" "${HOME}/.stumpwmrc.d"
fi
