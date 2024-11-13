#!/bin/bash

set -euo pipefail

RECIPIENT_OFFSET_INCHES=${RECIPIENT_OFFSET_INCHES:-5.5}

while getopts "ha:s:r:o:" OPT; do
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
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

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
\usepackage[left=.8in,top=.8in,papersize={4.12in,9.50in},landscape,twoside=false]{geometry}
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

echo ""
echo ""
echo ""
echo "INSTRUCTIONS"
echo "print using settings 'Paper Size: #10 envelope' size and 'Orientation: Portrait'. "
echo "feed #10 envelope into tray with the stamp on the top-left"

# lpr ${ROTATED} -o media=COM10
# x-www-browser-local-file.sh "${ROTATED}"
evince ${ROTATED}
