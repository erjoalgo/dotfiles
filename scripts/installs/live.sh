#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y python3-hid libusb-1.0-0-dev libudev-dev ledger-wallets-udev

# python3 -m pip install btchip-python coldcard digitalbitbox keepkey \
#         ledger safe_t trezor -U


python3 -m pip install btchip-python
