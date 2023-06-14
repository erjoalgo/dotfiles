#!/bin/bash -x

set -euo pipefail

cd /home/ealfonso/Downloads/netboot/
sudo chmod -R a+rwx .
sudo atftpd --no-fork --verbose=7 --logfile - --daemon .
