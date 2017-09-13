#!/bin/bash

while getopts "m:nht:" OPT; do
    case ${OPT} in
	m)
	    LAST_N_MINS="${OPTARG}"
	    ;;
	n)
	    NO_OPEN=true
	    ;;
	t)
	    DEST="${OPTARG}"
	    ;;
	d)
	    DOWNLOADS="${OPTARG}"
	    ;;
	h)
	    less $0
	    exit 0
	    ;;
    esac
done

LAST_N_MINS=${LAST_N_MINS:-5}
DEST=${DEST:-${1}}
DOWNLOADS=${DOWNLOADS:-"${HOME}/Downloads"}

if test -z "${DEST}"; then
    DEST=$(pwd)
fi

if ! test -d "${DEST}"; then
    mkdir -p "${DEST}"
fi

find "${DOWNLOADS}" -cmin -${LAST_N_MINS} -type f|\
while read FILE; do
    if grep '.part$' <<< "${FILE}"; then
	continue;
    fi
    PART="${FILE}.part"
    while test -e "${PART}"; do
	echo "waiting for ${PART}..."
	sleep 1;
    done
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
	if test -z "${NO_OPEN}" -a "${EXT}" = "${FN_EXT}"; then
	    nohup ${PROGRAM} "${FN}" > /dev/null &
	    break
	fi
    done
done
