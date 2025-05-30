#!/bin/bash

set -euo pipefail

while getopts "ha:s:r:o:7c:x" OPT; do
    case ${OPT} in
    s)
        SENDER_FILENAME=${OPTARG}
        ;;
    r)
        RECIPIENT_FILENAME=${OPTARG}
        ;;
    o)
        RECIPIENT_OFFSET_INCHES=${OPTARG}
        ;;
    1)
        ENVELOPE_TYPE="10"
        ;;
    7)
        ENVELOPE_TYPE="7"
        ;;
    c)
        INK_COLOR=${OPTARG}
        ;;
    x)
        X_WWW_BROWSER=true
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

case "${ENVELOPE_TYPE}" in

    10)
        RECIPIENT_OFFSET_INCHES=${RECIPIENT_OFFSET_INCHES:-5.5}
        SIZE=4.12in,9.50in
        ;;

    7)
        RECIPIENT_OFFSET_INCHES=${RECIPIENT_OFFSET_INCHES:-2}
        SIZE=5in,7in
        ;;
    *)
        echo "unknown envelope type" && exit ${LINENO}
        ;;
esac

if ! command -v evince || ! command -v pdftk || ! command -v pdflatex; then
    sudo apt-get install -y evince pdftk
    ~/git/dotfiles/installs/tex.sh
fi

function read-lines {
    FILENAME=${1} && shift
    cat ${FILENAME} | sed 's/$/\n/g'
}

function escape-ampersand {
    sed 's/&/\\&/g'
}

echo "reading sender lines..."
SENDER_LINES=$(read-lines "${SENDER_FILENAME}" | escape-ampersand)

echo "reading recipient lines..."
RECIPIENT_LINES=$(read-lines "${RECIPIENT_FILENAME}" | escape-ampersand)

TEX=$(mktemp XXXXX.tex --tmpdir)
PREFIX="${TEX%.*}"

cat <<EOF > ${TEX}

\documentclass{letter}
\usepackage[left=.8in,top=.8in,papersize={${SIZE}},landscape,twoside=false]{geometry}
\setlength\parskip{0pt}
\pagestyle{empty}

\begin{document}

\LARGE
${SENDER_LINES}

\vspace{.5in}\LARGE
\setlength\parindent{${RECIPIENT_OFFSET_INCHES}in}

${RECIPIENT_LINES}
\end{document}
EOF

cd /tmp
pdflatex ${TEX} # -output-directory /tmp


PDF="${PREFIX}.pdf"
ROTATED="${PREFIX}-rotated.pdf"
pdftk "${PDF}" cat 1-endright output "${ROTATED}"


OUTPUT="${ROTATED}"

if test -n "${INK_COLOR:-}"; then
    COLORED_PDF="${PREFIX}-rotated-colored.pdf"
    convert-replace-foreground-color.sh -i "${ROTATED}" -f "${INK_COLOR}" -o "${COLORED_PDF}"
    OUTPUT="${COLORED_PDF}"
fi

echo "tempfile: ${PDF}"
echo ""
echo ""
echo ""
echo "INSTRUCTIONS"
echo "print using settings 'Paper Size: #${ENVELOPE_TYPE} envelope' size and 'Orientation: Portrait'. "
echo "feed #${ENVELOPE_TYPE} envelope into tray with the stamp on the top-left"

# TODO...
# lpr ${ROTATED} -o media=COM10

if test "${X_WWW_BROWSER:-}" = true; then
    x-www-browser-local-file.sh "${OUTPUT}"
else
    evince "${OUTPUT}"
fi
