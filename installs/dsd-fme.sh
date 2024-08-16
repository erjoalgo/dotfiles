#!/bin/bash -x

set -euo pipefail

sudo apt install -y libpulse-dev pavucontrol libsndfile1-dev libfftw3-dev liblapack-dev socat libusb-1.0-0-dev rtl-sdr librtlsdr-dev libusb-1.0-0-dev cmake git wget make build-essential libitpp-dev libncurses-dev libncurses6 libcodec2-dev

chkitpp=$(sudo apt list libitpp-dev 2>&1|grep -ci "installed")

if [ "$chkitpp" -lt "1" ];then
   printf "\nFor some reason, ITPP did NOT install!\nYou may have to manually install it.\n\n"
   read -p "Press Enter to exit..." x
   exit 1
fi

function git-clone-cd {
    URL=${1} && shift
    DEST=${HOME}/git/$(basename "${URL}")
    if ! test -d "${DEST}"; then
        git clone "${URL}" "${DEST}"
    fi
    cd "${DEST}"
}

git-clone-cd https://github.com/lwvmobile/mbelib
git checkout ambe_tones
mkdir -p build
cd build
cmake ..
make -j $(nproc)
sudo make install
sudo ldconfig
git-clone-cd https://github.com/lwvmobile/dsd-fme
mkdir -p build
cd build
cmake ..
make -j $(nproc)
sudo make install
sudo ldconfig
