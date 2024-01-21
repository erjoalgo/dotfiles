#!/bin/bash -x

set -euo pipefail

while getopts "hi:p:" OPT; do
    case ${OPT} in
    i)
        ISO=${OPTARG}
        ;;
    # optional
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

if test -z "${ISO:-}" -o ! -e "${ISO}"; then
    echo "usage: $(basename $0) -i <ISO>"
    exit ${LINENO}
fi

PRESEED=${PRESEED:-${HOME}/git/dotfiles/data/public/debian-preseed.cfg}
test -e "${PRESEED}"

OUTPUT=${OUTPUT:-$(pwd)/preseeded-$(basename "${ISO}")}

sudo apt-get install -y p7zip-full genisoimage syslinux-utils isolinux

DIR=$(mktemp -d)

7z x "-o${DIR}" "${ISO}"

cd "${DIR}"
chmod +w -R install*/

INITRD_GZ=$(echo $(pwd)/install*/initrd.gz)
INITRD="${INITRD_GZ%.*}"

gunzip "${INITRD_GZ}"

PRESEED_CFG=preseed.cfg
cp "${PRESEED}" "${PRESEED_CFG}"
echo "${PRESEED_CFG}" | cpio -H newc -o -A -F "${INITRD}"

# show a diff with the original preseed.cfg file
pushd .
cd $(mktemp -d)
cp "${INITRD}" .
cpio -idv < ./initrd || true
diff "${PRESEED}" preseed.cfg
popd

gzip "${INITRD}"

chmod -w -R install*/

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
