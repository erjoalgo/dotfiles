#!/bin/bash -x

set -euo pipefail

EXT=png
while getopts "e:p:ho:s" OPT; do
    case ${OPT} in
    e)
        ESSID=${OPTARG}
        ;;
    p)
        PASS=${OPTARG}
        ;;
    o)
        OUTPUT=${OPTARG}
        ;;
    s)
        EXT=svg
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done

shift $((OPTIND -1))

function escape {
    TEXT=${1} && shift
    sed 's/[:;,"\]/\\\0/g' <<< "${TEXT}"
}

test -n "${ESSID}"
test -n "${PASS}"

PASS=$(escape "${PASS}")
ESSID=$(escape "${ESSID}")
OUTPUT=${OUTPUT:-"wifi-${ESSID}.${EXT}"}

# reference
# https://github.com/zxing/zxing/wiki/Barcode-Contents#wi-fi-network-config-android-ios-11

qrencode -t "${EXT}" -o "${OUTPUT}" "WIFI:T:WPA;S:${ESSID};P:${PASS};H:false;"

echo "wrote to ${OUTPUT}"
