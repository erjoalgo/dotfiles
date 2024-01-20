#!/bin/bash -x

set -euo pipefail


ELF_FILES=$(cat <<EOF
/speculos/speculos/cxlib/nanos-cx-2.0.elf
/speculos/speculos/cxlib/nanos-cx-2.1.elf
/speculos/speculos/cxlib/nanosp-api-level-cx-1.elf
/speculos/speculos/cxlib/nanosp-api-level-cx-12.elf
/speculos/speculos/cxlib/nanosp-api-level-cx-5.elf
/speculos/speculos/cxlib/nanosp-cx-1.0.3.elf
/speculos/speculos/cxlib/nanosp-cx-1.0.elf
/speculos/speculos/cxlib/nanox-api-level-cx-1.elf
/speculos/speculos/cxlib/nanox-api-level-cx-12.elf
/speculos/speculos/cxlib/nanox-api-level-cx-5.elf
/speculos/speculos/cxlib/nanox-cx-2.0.2.elf
/speculos/speculos/cxlib/nanox-cx-2.0.elf
/speculos/speculos/cxlib/stax-api-level-cx-1.elf
/speculos/speculos/cxlib/stax-api-level-cx-10.elf
/speculos/speculos/cxlib/stax-api-level-cx-11.elf
/speculos/speculos/cxlib/stax-api-level-cx-12.elf
/speculos/speculos/cxlib/stax-api-level-cx-13.elf
/speculos/speculos/cxlib/stax-api-level-cx-14.elf
/speculos/speculos/cxlib/stax-api-level-cx-3.elf
/speculos/speculos/cxlib/stax-api-level-cx-5.elf
/speculos/speculos/cxlib/stax-api-level-cx-7.elf
/speculos/speculos/cxlib/stax-api-level-cx-8.elf
/speculos/speculos/cxlib/stax-api-level-cx-9.elf
EOF
)

OLDIFS=$IFS
IFS=$'\n'
for ELF in ${ELF_FILES}; do
    MODEL=$(basename "${ELF}" | cut -f1 -d-)
    if docker run --rm -it -v $(pwd)/apps:/speculos/apps \
              --publish 41000:41000 \
              "ghcr.io/ledgerhq/speculos" \
              --display headless \
              --vnc-port 41000 \
              -m "${MODEL}" \
              "${ELF}" \
       ; then
        break
    fi
done
