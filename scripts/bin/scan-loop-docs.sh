#!/bin/bash

# SCANNER=gt68xx:libusb:002:005
# SCANNER=$(sudo scanimage -L | grep Visioneer | cut -f1 -d\' | cut -f2 -d\`)
# test -n "${SCANNER}" || exit ${LINENO}

if command -v imagescan; then
    SCAN_CMD="/usr/lib/x86_64-linux-gnu/utsushi/utsushi-scan --no-interface"
elif sudo scanimage -L; then
    SCAN_CMD="sudo scanimage"
    # TODO
else
    echo "unable to find a scanner." && exit ${LINENO}
fi

DOC_NAME=""
FMT=pnm

while true; do
    while test -z ${DOC_NAME}; do
        read -e -p 'enter document name: ' DOC_NAME
        DOC_NAME=$(tr -d '/' <<< "${DOC_NAME}")
    done

    test -e ${DOC_NAME} || mkdir ${DOC_NAME} || exit ${LINENO}
    cd ${DOC_NAME}

    if ! PAGE_NO=$(expr $(basename $(ls -1 [0-9]*.${FMT} 2>/dev/null| tail -1) .${FMT}) + 1); then
        PAGE_NO=1
    fi

    while true; do
        echo "scanning page ${PAGE_NO} of ${DOC_NAME}..."
        # time sudo scanimage --device ${SCANNER} --format ${FMT} --mode color > ${PAGE_NO}.${FMT}
        while true; do
            if time ${SCAN_CMD} > ${PAGE_NO}.${FMT}; then
                break
            else
                echo "retry scan..."
                sleep 1
            fi
        done

        echo "enter new doc name to scan a new doc, "
        echo "<Return> to scan more ${DOC_NAME} pages, "
        read -p "q to quit: "  NEW_DOC_NAME
        test -z "${NEW_DOC_NAME}" || break
        PAGE_NO=$(expr ${PAGE_NO} + 1)
    done

    convert $(ls -1 *${FMT} | sort -n) ${DOC_NAME}.pdf

    cd ..
    test "${NEW_DOC_NAME}" = "q" && break
    DOC_NAME="${NEW_DOC_NAME}"
done
