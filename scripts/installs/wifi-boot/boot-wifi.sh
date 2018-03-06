#!/bin/bash -x

set -euo pipefail

LINE=$(lspci | grep -i network)

if grep -i centrino <<< "$LINE"; then
    sudo apt-get install firmware-iwlwifi
    sudo modprobe -r iwlwifi
    sudo modprobe iwlwifi
elif false; then
    echo false
fi
sudo apt-get install -y expect

# apt-get install -y firmware-realtek
# apt-get install -y firmware-iwlwifi

# modprobe -r realtek
# modprobe realtek

# 108  modprobe -r iwlwifi; modproblem iwlwifi
# 109  modprobe -r realtek ; modprobe realtek
