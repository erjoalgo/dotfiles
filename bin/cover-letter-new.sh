#!/bin/bash -x

set -euo pipefail

COMPANY_NAME=${1} && shift

TMPL=/home/ealfonso/git/resume/v2/applications/template/
DEST="$(dirname ${TMPL})/${COMPANY_NAME}"

cp -r "${TMPL}" "${DEST}"

TEX="${DEST}/cover-letter.tex"

DATESTR=$(date +"%dth of %B, %Y")
RECIPIENT="${COMPANY_NAME} recruitment team"

sed -i \
    -e "s/^\\\\recipient.*/\\\\recipient{${RECIPIENT}}{}/g" \
    -e "s/^\\\\date.*/\\\\date{${DATESTR}}/g" \
    "${TEX}"

emacsclient-wrapper.sh "${TEX}"
