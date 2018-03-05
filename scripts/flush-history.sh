#!/bin/bash -x

set -euo pipefail


ls -Ris ${HOME} > ls-rec

FILES=$(cat <<EOF
${HOME}/master.org \
${HOME}/.lesshst \
${HOME}/search-history \
${HOME}/.bash_history \
${HOME}/.emacs.auto-save \
${HOME}/.emacs.backups \
${HOME}/pictures \
ls-rec
EOF
)

FILENAME=$(hostname)-$(date "+%F-%T")

which zip

zip -r ${FILENAME}.zip -9 ${FILES}

SHRED_ARGS=-zufn10
for FN in ${FILES}; do
    read -p "rec shred ${FN}?"
    if test -d "${FN}"; then
	find "${FN}" -type f -exec shred ${SHRED_ARGS} {} \;
	find "${FN}" -depth -type d -exec rmdir {} \;
    else
	shred ${SHRED_ARGS} "${FN}"
    fi
done
