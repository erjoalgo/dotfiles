#!/bin/bash -x

set -euo pipefail

PDF=${1} && shift

PRINTERS=$(lpstat -t | grep -Po "^[^ ]+?(?= accepting requests)")

echo "${PRINTERS}" 1>&2
select PRINTER in ${PRINTERS}; do
    break
done

lpr ${PDF} -P ${PRINTER} -o outputorder=reverse
