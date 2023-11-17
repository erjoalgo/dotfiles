#!/bin/bash -x

set -euo pipefail

IMG=${IMG:-/var/cache/openafs.img}
MNT=${MNT:-/var/cache/openafs}

CACHE_KB=$(cut /etc/openafs/cacheinfo -f3 -d:)
CACHE_MB=${CACHE_MB:-$(python3 -c "print(${CACHE_KB} / 1024)")}
BLOCK_COUNT=$(python3 -c "print(int(1 + ${CACHE_MB} / 10))")
sudo dd if=/dev/zero "of=${IMG}" bs=10M count=410   # (~4.1 GB partition)
sudo mkfs.ext4 "${IMG}"
insert-text-block '# 7078ff09-dcc5-43e2-8a35-cd397843c0ce-add-openafs-cache-partition' /etc/fstab<<EOF
${IMG} ${MNT} ext4 defaults,loop 0 2
EOF
sudo tune2fs -c 0 -i 0 -m 0 "${IMG}"

sudo mount "${MNT}"
