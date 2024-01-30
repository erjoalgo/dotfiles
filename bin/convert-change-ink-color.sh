#!/bin/bash -x

set -euo pipefail

INPUT_IMAGE=${1} && shift

while getopts "ha:" OPT; do
    case ${OPT} in
    c)
        COLOR=${OPTARG}
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

COLOR=${COLOR:-blue}
OUTPUT=${OUTPUT:-"${INPUT_IMAGE%.*}-${COLOR}.png"}

convert -density 150 -trim -verbose "${INPUT_IMAGE}" +level-colors "${COLOR}" \
        -quality 100 -sharpen 0x1.0 -flatten "${OUTPUT}"

echo "wrote to ${OUTPUT}"
