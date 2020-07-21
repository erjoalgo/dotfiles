#!/bin/bash -x

set -euo pipefail


shift $((OPTIND -1))

if ! command -v imagescan; then
    RELEASE=$(lsb_release -r -s)

    case ${RELEASE} in
        10)
            URL=https://download2.ebz.epson.net/imagescanv3/debian/latest1/deb/x64/imagescan-bundle-debian-10-3.63.0.x64.deb.tar.gz
            ;;
        9)
            URL=https://download2.ebz.epson.net/imagescanv3/debian/latest2/deb/x64/imagescan-bundle-debian-9-3.63.0.x64.deb.tar.gz
            ;;
        *)
            echo "debian release ${RELEASE} not supported."
            echo "consinder checking in http://support.epson.net/linux/en/imagescanv3.php"
            exit ${LINENO}
            ;;
    esac

    ROOT=/tmp/epson-es-50
    rm -rf ${ROOT} && mkdir ${ROOT}
    cd ${ROOT}
    wget ${URL}
    tar -xvf *.tar.gz

    cd $(find . -name 'imagescan*' -maxdepth 1 -type d)
    sudo apt-get install -y libboost-program-options1.67.0
    sudo dpkg -i core/imagescan*.deb
    which imagescan
fi

sudo sed -i  \
     's/rights="none" pattern="PDF"/rights="read | write" pattern="PDF"/'  \
     /etc/ImageMagick-*/policy.xml
