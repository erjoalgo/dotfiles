#!/bin/bash

set -euo pipefail

STUMPWM_TOP=$(dirname $(dirname $($(which {g,}readlink|head -1) -f $0)))
INITS_TOP="${STUMPWM_TOP}/inits"

function safe_ln	{
    SRC=${1} && shift
    DST=${1} && shift
    DSTFILE=${DST}
    if grep '.*/$'<<<"${DSTFILE}" >/dev/null; then
	DSTFILE=${DST}$(basename ${SRC})
    fi

    DIR=$(dirname "${DSTFILE}")

    if test -e ${DSTFILE} -a ! -L ${DSTFILE}; then
	# echo "warning: skipping symlink to existent non-symlink ${DST} of  ${SRC}"
	echo "warning: moving ${DSTFILE} to ${DSTFILE}.bak"
	mv -n ${DSTFILE}{,.bak}
    elif test ! -d $DIR; then
	echo "warning: skipping symlink to non-existent parent dir: $DIR"
	return
    fi
    ln -sf ${SRC} ${DST}
}

for LINK in \
    .bash_aliases \
    .pythonrc.py \
    .inputrc \
    .my-bashrc \
    .my-bash-funs \
    .xmodmap \
    .xinitrc \
    .my-bash-profile \
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
