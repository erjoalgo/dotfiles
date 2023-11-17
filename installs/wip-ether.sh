#!/bin/bash -x

set -euo pipefail

GIT=${HOME}/git
cd ${GIT}

if ! command -v geth; then
    URL=https://github.com/ethereum/go-ethereum
    test -d $(basename ${URL})/.git || git clone ${URL}
    cd $(basename ${URL})
    make geth
    make all
    ln -sf $(realpath ./build/bin/geth) ~/.local/bin
    # sudo make install
fi

# sudo apt-get install -y cmake
# cd ${GIT}
# URL=https://github.com/ethereum/aleth.git
# test -d $(basename ${URL} .git)/.git || git clone --recursive "${URL}"
# cd aleth
# # rm -rf build
# mkdir -p build; cd build  # Create a build directory.
# cmake ..               # Configure the project.
# cmake --build .

sudo apt-get install -y mesa-common-dev libdbus-1-dev
URL=https://github.com/ethereum-mining/ethminer
cd ${GIT}
test -d $(basename ${URL} .git)/.git || git clone "${URL}"
cd $(basename "${URL}")
git submodule update --init --recursive
rm -rf build
mkdir -p build
cd build
pwd
cmake ..
cmake --build .
sudo make install
# ln -sf $(realpath ./build/ethminer/ethminer) ~/.local/bin
