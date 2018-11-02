#!/bin/bash -x

# SCANNER=gt68xx:libusb:002:005
SCANNER=$(sudo scanimage -L | grep Visioneer | cut -f1 -d\' | cut -f2 -d\`)

test -n "${SCANNER}" || exit ${LINENO}
DOC_NAME=""
while true; do
    while test -z ${DOC_NAME}; do
	read -e -p 'enter document name: ' DOC_NAME
	DOC_NAME=$(tr -d '/' <<< "${DOC_NAME}")
    done

    test -e ${DOC_NAME} || mkdir ${DOC_NAME} || exit ${LINENO}
    cd ${DOC_NAME}

    PAGE=1
    while true; do
	test ! -e ${PAGE}.pnm && break
	PAGE=$(expr ${PAGE} + 1)
    done

    FMT=pnm
    while true; do
	echo "scanning page ${PAGE} of ${DOC_NAME}..."
	time sudo scanimage --device ${SCANNER} --format ${FMT} --mode color > ${PAGE}.${FMT}
	read -p "enter new doc name to scan a new doc, <Return> to scan more ${DOC_NAME} pages, q to quit: " NEW_DOC_NAME
	test -z "${NEW_DOC_NAME}" || break
	PAGE=$(expr ${PAGE} + 1)
    done

    find . -name "*${FMT}" -exec convert {} -quality 15 {}.jpg \; && \
    	convert *jpg ${DOC_NAME}.pdf || exit ${LINENO} &

    cd ..
    test "${NEW_DOC_NAME}" = "q" && break
    DOC_NAME="${NEW_DOC_NAME}"
done
