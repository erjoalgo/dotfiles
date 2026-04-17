#!/bin/bash -x

set -euo pipefail

while getopts "s:g:h" OPT; do
    case ${OPT} in
        s)
            SERVER_ADDRESS=${OPTARG}
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

GCODE=${1} && shift

if ! command -v octoprint-cli; then
    pip install octoprint-cli
fi

function write-ini {
    INI=${HOME}/.config/octoprint-cli.ini
    SERVER_ADDRESS=${1} && shift
    API_KEY=${1} && shift

    insert-text-block '# 0754c16b-b5aa-447d-81d6-b4fa758a56bc-octoprint-cli-config' \
                      "${INI}" <<EOF
[server]
;Set OctoPrint server address and x-api-key
ServerAddress = ${SERVER_ADDRESS}
ApiKey = ${API_KEY}

[preferences]
;Set if the program uses colored or formatted text, this setting is turned off on windows due to cmd and powershell limitations
FormattedText = true
;Set if the program should check for updates
UpdateCheck = true

[printer]
;Set maximum temperature that printer can be set to
MaxExtruderTemp = 250
MaxBedTemp = 85
EOF
}

if test -n "${SERVER_ADDRESS:-}"; then
    x-www-browser "${SERVER_ADDRESS}"
    read -p"enter octorpint API key: " API_KEY
    write-ini "${SERVER_ADDRESS}" "${API_KEY}"
fi

if ! octoprint-cli files list; then
    echo "failed to connect? maybe update server address"
    exit ${LINENO}
fi

test -e "${GCODE}"

octoprint-cli files upload "${GCODE}"
octoprint-cli print select $(basename "${GCODE}")
octoprint-cli print start
