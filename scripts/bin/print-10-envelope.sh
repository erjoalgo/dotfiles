#!/bin/bash

set -euo pipefail

while getopts "ha:s:r:" OPT; do
    case ${OPT} in
    a)
        SOMEARG=${OPTARG}
        ;;
    s)
        SENDER_FILENAME=${OPTARG}
        ;;
    r)
        RECIPIENT_FILENAME=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if ! command -v evince || ! command -v pdftk; then
    sudo apt-get install -y evince pdftk
fi

function read-lines {
    FILENAME=${1} && shift
    cat ${FILENAME} | sed 's/$/\n/g'
}
echo "reading sender lines..."
SENDER_LINES=$(read-lines "${SENDER_FILENAME}")

echo "reading recipient lines..."
RECIPIENT_LINES=$(read-lines "${RECIPIENT_FILENAME}")

TEX=$(mktemp XXXXX.tex --tmpdir)
PREFIX="${TEX%.*}"

cat <<EOF > ${TEX}

\documentclass{letter}
\usepackage[left=.8in,top=.8in,papersize={4.12in,9.50in},landscape,twoside=false]{geometry}
\setlength\parskip{0pt}
\pagestyle{empty}

\begin{document}

\large{
${SENDER_LINES}
}

\vspace{1.0in}\LARGE
\setlength\parindent{3.5in}

${RECIPIENT_LINES}
\end{document}
EOF

cd /tmp
pdflatex ${TEX} # -output-directory /tmp


PDF="${PREFIX}.pdf"
ROTATED="${PREFIX}-rotated.pdf"

pdftk "${PDF}" cat 1-endright output "${ROTATED}"

echo "print using #10 envelope size and portrait orientation. "
echo "feed #10 envelope into tray with the stamp on the top-left"
read -p "confirm print instructions: "

evince "${ROTATED}"
