#!/bin/bash

set -euo pipefail

while getopts "ha:s:r:o:716c:x" OPT; do
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
            ENVELOPE_TYPE="5x7"
            ;;
        6)
            ENVELOPE_TYPE="6x9"
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

RECIPIENT_VERTICAL_OFFSET_INCHES=${RECIPIENT_VERTICAL_OFFSET_INCHES:-.5in}

# PWG media name passed to lp (https://www.pwg.org/ = Printer Working Group)
LP_MEDIA=""

case "${ENVELOPE_TYPE}" in

    10)
        RECIPIENT_OFFSET_INCHES=${RECIPIENT_OFFSET_INCHES:-5.5}
        SIZE=4.12in,9.50in
        LP_MEDIA="na_number-10_4.125x9.5in"
        ;;

    5x7)
        RECIPIENT_OFFSET_INCHES=${RECIPIENT_OFFSET_INCHES:-2}
        SIZE=5in,7in
        LP_MEDIA="na_5x7_5x7in"
        ;;
    6x9)
        RECIPIENT_OFFSET_INCHES=${RECIPIENT_OFFSET_INCHES:-4}
        RECIPIENT_VERTICAL_OFFSET_INCHES=2in
        SIZE=6in,9in
        LP_MEDIA="na_6x9_6x9in"
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
\usepackage[left=.8in,top=.8in,right=0in,papersize={${SIZE}},landscape,twoside=false]{geometry}
\setlength\parskip{0pt}
\pagestyle{empty}

\begin{document}

\LARGE
${SENDER_LINES}

\vspace{${RECIPIENT_VERTICAL_OFFSET_INCHES}}\LARGE
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

if test -z "${LP_MEDIA}"; then
    echo ""
    echo "INSTRUCTIONS"
    echo "print using settings 'Paper Size: #${ENVELOPE_TYPE} envelope' size and 'Orientation: Portrait'. "
    echo "feed #${ENVELOPE_TYPE} envelope into tray with the stamp on the top-left"
fi

# Open the preview in the background so the prompt below can appear while it's open
if test "${X_WWW_BROWSER:-}" = true; then
    x-www-browser-local-file.sh "${OUTPUT}" &
else
    evince "${OUTPUT}" &
fi

if test -n "${LP_MEDIA}"; then
    echo ""
    echo "feed #${ENVELOPE_TYPE} envelope into tray with the stamp on the top-left"
    ANSWER=""
    read -r -p "Print ${OUTPUT} on #${ENVELOPE_TYPE} envelope? [y/N] " ANSWER || true

    if [[ "${ANSWER}" =~ ^[Yy]$ ]]; then
        # Look up the default printer only after confirmation
        PRINTER=$(lpstat -d 2>/dev/null | sed -n 's/^system default destination: //p' || true)
        if test -z "${PRINTER}"; then
            echo "no default printer set (check: lpstat -d)" >&2
            exit 1
        fi
        echo "printing to ${PRINTER}..."
        lp -d "${PRINTER}" \
           -o media="${LP_MEDIA}" \
           -o print-scaling=none \
           "${OUTPUT}"
    else
        echo "print cancelled."
    fi
fi
