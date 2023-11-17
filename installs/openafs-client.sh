#!/bin/bash -x

set -euo pipefail

cd "$(realpath $(dirname "${BASH_SOURCE[0]}"))"

sudo dpkg-reconfigure debconf

sudo apt-get install -y openafs-{modules-dkms,client,krb5}

./openafs-cache-partition.sh


