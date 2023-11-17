#!/bin/bash -x

set -euo pipefail

while getopts "hg:" OPT; do
    case ${OPT} in
    g)
        GCODE=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

test -e "${GCODE}"

octoprint-cli files upload "${GCODE}"
octoprint-cli print select $(basename "${GCODE}")
octoprint-cli print start
