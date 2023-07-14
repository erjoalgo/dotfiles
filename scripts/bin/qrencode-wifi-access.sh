#!/bin/bash -x

set -euo pipefail

while getopts "s:p:h" OPT; do
    case ${OPT} in
    s)
        SSID=${OPTARG}
        ;;
    p)
        PASS=${OPTARG}
        ;;
    o)
        OUTPUT=${OPTARG}
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

test -n "${SSID}"
test -n "${PASS}"

PASS=$(escape "${PASS}")
SSID=$(escape "${SSID}")
OUTPUT=${OUTPUT:-"wifi-${SSID}.png"}

# reference
# https://github.com/zxing/zxing/wiki/Barcode-Contents#wi-fi-network-config-android-ios-11

qrencode -o "${OUTPUT}" "WIFI:T:WPA;S:${SSID};P:${PASS};H:false;"
