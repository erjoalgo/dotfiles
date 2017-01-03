#!/bin/bash -x

# SCANNER=gt68xx:libusb:002:005
SCANNER=$(sudo scanimage -L | grep Visioneer | cut -f1 -d\' | cut -f2 -d\`)

test -n ${SCANNER} || exit ${LINENO}
while true; do
    DOC_NAME=""
    while test -z ${DOC_NAME} || test -e ${SCAN_NAME}.pnm; do
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
	
    while true; do
	echo "scanning page ${PAGE} of ${DOC_NAME}..."
	time sudo scanimage --device ${SCANNER} > ${PAGE}.pnm
	read -p 'press n to stop scanning more ${DOC_NAME} pages: ' CONT
	test -z ${CONT} || break
	PAGE=$(expr ${PAGE} + 1)
    done

    find . -name '*pnm' -exec convert {} -quality 15 {}.jpg \; && \
    	convert *jpg ${DOC_NAME}.pdf || exit ${LINENO} &

    cd ..
done
