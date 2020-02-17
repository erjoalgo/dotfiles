#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y build-essential autoconf automake libtool pkg-config libnl-3-dev libnl-genl-3-dev libssl-dev ethtool shtool rfkill zlib1g-dev libpcap-dev libsqlite3-dev libpcre3-dev libhwloc-dev libcmocka-dev hostapd wpasupplicant tcpdump screen iw usbutils

URL=https://download.aircrack-ng.org/aircrack-ng-1.6.tar.gz
install-from-source -u "${URL}"
