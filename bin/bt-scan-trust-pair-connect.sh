#!/bin/bash -x

set -euo pipefail

while getopts "m:rh" OPT; do
    case ${OPT} in
    m)
        MAC=${OPTARG}
        ;;
    r)
        REMOVE=true
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

BLE_COMPANY_IDS=/usr/share/bt/manufacturers.yaml
if ! test -e "${BLE_COMPANY_IDS}"; then
    sudo mkdir -p $(dirname "${BLE_COMPANY_IDS}")
    URL=https://bitbucket.org/bluetooth-SIG/public/raw/main/assigned_numbers/company_identifiers/company_identifiers.yaml
    curl "${URL}" | sudo tee "${BLE_COMPANY_IDS}"
fi


if test -z "${MAC:-}"; then
    TMPFILE=$(mktemp)
    expect -f - <<EOF &
set timeout -1
log_file ${TMPFILE}
# exp_internal 1

spawn bluetoothctl
send -- "scan on\r"
send -- "devices\r"
expect {
    -re { Device ([A-Z0-9:]+)} {
        set device \$expect_out(1,string);
        # send -- "info \$device\r"
        exp_continue
    }
    -re {ManufacturerData Key: (0x[0-9a-zA-Z]+)} {
        set key \$expect_out(1,string);
        puts "key is \$key"
        # set status [catch {exec grep -A1 -F "\$key" "$BLE_COMPANY_IDS"} output]
        # puts "grep output: \$output"
        exp_continue
    }
}
EOF
    SCAN_PID=$!
    trap "kill $SCAN_PID" SIGINT
    wait ${SCAN_PID} || true
    trap - SIGINT

    DEVICES=$(grep -Po '(?<=Device )[A-Z0-9:]+.*' < "${TMPFILE}" | sort | uniq)
    # rm "${TMPFILE}"
    echo "select device: " 1>&2
    OLDIFS=$IFS
    IFS=$'\n'
    select LINE in $(sed 's/\r$//' <<< ${DEVICES}); do
        MAC=$(cut -f1 -d' ' <<< "${LINE}")
        echo "${MAC}"
        break
    done
    IFS=$OLDIFS
fi


if test "${REMOVE:-}" = true; then
    expect -f - <<EOF
set timeout -1
set prompt "# "
spawn bluetoothctl
send -- "scan off\r"
expect -- \$prompt
send -- "cancel-pairing ${MAC}\r"
expect -- \$prompt
send -- "disconnect ${MAC}\r"
expect -- \$prompt
send -- "untrust ${MAC}\r"
expect -- \$prompt
send -- "remove ${MAC}\r"
expect -- \$prompt
EOF
else
    expect -f <(cat <<EOF
exp_internal 1
set timeout -1
spawn bluetoothctl
send -- "scan off\r"
send -- "trust ${MAC}\r"
expect -- "trust succeeded"
send -- "pair ${MAC}\r"
expect {
    "Confirm passkey" {
        expect_user -re "(.+)\[\r\n]"
        send "\$expect_out(1,string)\r\n"
    }
    "Pairing successful" {}
}
send -- "connect ${MAC}\r"
expect -- "Connection successful"
EOF
)
fi
