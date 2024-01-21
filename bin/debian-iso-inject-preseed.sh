#!/bin/bash -x

set -euo pipefail

while getopts "hi:p:" OPT; do
    case ${OPT} in
    i)
        ISO=${OPTARG}
        ;;
    p)
        PRESEED=${OPTARG}
        ;;
    o)
        OUTPUT=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if test -z "${ISO:-}" -o -z "${PRESEED:-}"; then
    echo "usage: $(basename $0) -i <ISO> -p <PRESEED_FILE>"
    exit ${LINENO}
fi

OUTPUT=${OUTPUT:-$(pwd)/preseeded-$(basename "${ISO}")}

# sudo apt-get install -y p7zip-full udevil
# sudo apt-get install -y udevil genisoimage
sudo apt-get install -y p7zip-full genisoimage syslinux-utils isolinux

DIR=$(mktemp -d)

7z x "-o${DIR}" "${ISO}"
# udevil mount "${ISO}"
# cp -rT /media/$(basename "${ISO}") .

cd "${DIR}"

chmod +w -R install.*/

gunzip install.*/initrd.gz

PRESEED_CFG=preseed.cfg
cp "${PRESEED}" "${PRESEED_CFG}"
echo "${PRESEED_CFG}" | cpio -H newc -o -A -F install.*/initrd

INITRD=$(echo $(pwd)/install.*/initrd)
pushd .
cd $(mktemp -d)
cp "${INITRD}" .
cpio -idv < ./initrd || true
diff "${PRESEED}" preseed.cfg
popd

gzip install.*/initrd

chmod -w -R install.*/

chmod +w md5sum.txt

find -follow -type f ! -name md5sum.txt -print0 |  \
    xargs -0 md5sum > md5sum.txt
chmod -w md5sum.txt

cd ..

genisoimage -r -J -b  \
            isolinux/isolinux.bin -c isolinux/boot.cat \
            -no-emul-boot -boot-load-size 4 -boot-info-table \
            -o "${OUTPUT}" "${DIR}"

isohybrid "${OUTPUT}"

echo "successfully wrote to ${OUTPUT}"
