#!/bin/bash
REPOS="${HOME}git"
if test -d "${REPOS}"; then
    mkdir "${REPOS}"
fi

for REPO in \
    erjoalgo-stumpwmrc \
    erjoalgo-gnu-scripts \
    dotemacs \
    erjoalgo-firefox-addons \
    erjoalgo-vimfx-config \
    ;do

    test -d ${REPO} ||  \
	git clone "https://github.com/erjoalgo/${REPO}" || exit ${LINENO}

done
