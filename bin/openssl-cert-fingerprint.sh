#!/bin/bash -x

set -euo pipefail

while getopts "h12:" OPT; do
    case ${OPT} in
    2)
        ALGO=sha256
        ;;
    1)
        ALGO=sha1
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if ! test -n "${ALGO}"; then
    echo "missing ALGO parameter" && exit ${LINENO}
fi

echo "computing ${ALGO} fingerprints..."
for CERT in ${*}; do
    echo -n "${CERT} "
    if ! openssl x509 -noout -fingerprint -${ALGO} -in ${CERT}; then
        echo "Failed to compute cert for ${CERT}"  >&2
    fi
done
