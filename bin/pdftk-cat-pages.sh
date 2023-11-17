#!/bin/bash -x

set -euo pipefail

while getopts "h" OPT; do
    case ${OPT} in
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

IN=${1} && shift
PAGE_RANGE=${1} && shift
OUT=${1} && shift

pdftk "A=${IN}" cat "A${PAGE_RANGE}" output "${OUT}"
