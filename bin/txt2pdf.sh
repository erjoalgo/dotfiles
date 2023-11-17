#!/bin/bash -x

set -euo pipefail
while getopts "f:h" OPT; do
    case ${OPT} in
    f)
        FILENAME_TXT=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))
FILENAME_TXT="${FILENAME_TXT:-$1}"
test -e "${FILENAME_TXT}"

NO_EXT="${FILENAME_TXT%.*}"
OUT_PS=$(tempfile --suffix=.ps)
OUT_PDF="${NO_EXT}.pdf"

enscript "${FILENAME_TXT}" -p "${OUT_PS}"
ps2pdf "${OUT_PS}" "${OUT_PDF}"
