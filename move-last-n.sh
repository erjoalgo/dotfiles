#!/bin/bash -x

DOWNLOADS="${HOME}/Downloads"
DEST="${1}"
LAST_N_MINS=5

if test -z "${DEST}"; then
    DEST=$(pwd)
fi

if ! test -d "${DEST}"; then
    mkdir -p "${DEST}"
fi

for FILE in $(find "${DOWNLOADS}" -cmin -${LAST_N_MINS} -type f); do
    BASE=$(basename "${FILE}")
    mv -t "${DEST}" "${FILE}"
    FN="${DEST}/${BASE}"
    FN_EXT="${FN##*.}"
    for EXT_PROGRAM in \
	pdf,zathura \
	    jpg,eog\
	; do
	IFS=","
	set -- $EXT_PROGRAM
	EXT="${1}"
	PROGRAM="${2}"
	if test "${EXT}" = "${FN_EXT}"; then
	    ${PROGRAM} "${FN}"
	    break
	fi
    done
done
