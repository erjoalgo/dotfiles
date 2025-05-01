#!/bin/bash -x

set -euo pipefail

TARGET_TEMP=72
ON_TIME_MINUTES=40
OFF_TIME_MINUTES=5
INSTALL_ONLY=""

while getopts "t:n:f:ih" OPT; do
    case ${OPT} in
    t)
        TARGET_TEMP=${OPTARG}
        ;;
    n)
        ON_TIME_MINUTES=${OPTARG}
        ;;
    f)
        OFF_TIME_MINUTES${OPTARG}
        ;;
    i)
        INSTALL_ONLY=true
        ;;
    h)
        less "$0"
        exit 0
        ;;
    *)
        echo "unrecognized flag: ${OPT}" && exit ${LINENO}
        ;;
    esac
done
shift $((OPTIND -1))

function install {
    cd "$(realpath $(dirname "${BASH_SOURCE[0]}"))"
    SELF="${BASH_SOURCE[0]}"
    install-systemd-service.sh cool <<EOF
[Unit]
Description=HVAC cooling script with intermittent defrost
StartLimitInterval=0

[Service]
ExecStart=${SELF}
Restart=always
RestartSec=5
Environment=PATH=$PATH:$(realpath $(pwd)/../bin)
Environment=HASS_SERVER=$HASS_SERVER
Environment=HASS_TOKEN=$HASS_TOKEN

[Install]
WantedBy=multi-user.target
EOF
}

if test "${INSTALL_ONLY}" = true; then
    install
    exit 0
fi


function current-temp {
    hass-cli  \
        --table-format tsv  \
        state get sensor.dean_acre_ecobee_current_temperature

    hass-cli  \
        --table-format tsv  \
        state get sensor.dean_acre_ecobee_current_temperature |  \
        tail -1 |  \
        cut -f3 |  \
        tr -d ' '
}

function current-state {
    hass-cli --table-format=tsv state get climate.dean_acre_ecobee_2 | cut -f3 | tail -1
}

function cooling-off {
    hass-cli state edit climate.dean_acre_ecobee_2 off
}

function cooling-on {
    hass-cli state edit climate.dean_acre_ecobee_2 cool
}

function temp-monitor {
    set +x
    while true; do
        TEMP=$(current-temp)
        echo "current temperature: ${TEMP}F"
        sleep 60
    done
}

function cool-loop {
    while true; do
        TEMP=$(current-temp)
        STATE=$(current-state)
        if (( $(echo "${TEMP} > ${TARGET_TEMP}" |bc) )); then
            echo "cooling for ${ON_TIME_MINUTES}m ..."
            cooling-on
            sleep $((${ON_TIME_MINUTES} * 60))
            echo "defrosting for ${OFF_TIME_MINUTES}m ..."
            cooling-off
            sleep $((${OFF_TIME_MINUTES} * 60))
        fi
    done
}

temp-monitor &

# cool-loop

wait
