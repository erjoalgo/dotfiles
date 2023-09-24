#!/bin/bash

while getopts "m:d:ha:" OPT; do
    case ${OPT} in
    m)
        MAC=${OPTARG}
        ;;
    d)
        DUMP_DIR=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

MAC=${MAC:-38:01:46:C3:DC:17}
# MAC=b4:8a:0a:c1:0b:51
DUMP_DIR=${DUMP_DIR:-/tmp/dump/}

FILE=$(ls -1tr ${DUMP_DIR}/*log.csv | tail -1)
echo "${FILE}"
LINE=$(grep -i ${MAC} ${FILE} | tail -1)
echo "${LINE}"

PWR=$(tail -1 <<< ${LINE} | cut -d, -f5)
echo "PWR is ${PWR}"
if test -n "${PWR:-}"; then
    FREQ=$(python3 /dev/stdin<<EOF
pwr = ${PWR}
pwr_min = min(-92, pwr)
pwr_max = -30
percent = 100 * (pwr - pwr_min) // (pwr_max - pwr_min)
assert(pwr_min <= pwr <= pwr_max)

freq_min = 440
freq_max = 1760
freq = freq_min + (freq_max - freq_min) * percent / 100

print(freq)
EOF
)
   fake-beep.sh -f ${FREQ} -d .5 > /dev/null 2>&1
fi
