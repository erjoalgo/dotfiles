#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y openafs-client openafs-modules-dkms openafs-krb5 \
     openafs-fileserver openafs-dbserver
