#!/bin/bash

while getopts "ohp:P:e:m:aAE:i:t:" OPT; do
    case ${OPT} in
        o)
            # flag to disregard any existing password
            OVERWRITE="true"
            ;;
        e)
            # connect to given essid
            # required when connecting to a hidden essid
            ESSID="${OPTARG}"
            ;;
        p)
            # the wpa password
            PASS="${OPTARG}"
            OVERWRITE="true"
            ;;
        P)
            # a plaintext file with several passwords to try
            # implies OVERWRITE=true since we don't know the password
            PASS_CANDS_FILE=$(realpath "${OPTARG}")
            OVERWRITE="true"
            if ! test -e "${PASS_CANDS_FILE}"; then
                exit "no such file: ${PASS_CANDS_FILE}"
            fi
            ;;

        m)
            # one-character flag to be passed to macchanger
            MACCHANGE_OPT="${OPTARG}"
            ;;
        i)
            # wireless interface
            IFACE="${OPTARG}"
            ;;
        a)
            # flag to prompt for essid, even if matching entry exists
            ASK_ESSID="true"
            ;;
        A)
            # don't prompt for essid, when there are multiple matches
            # required when connecting to a hidden essid
            ASK_ESSID="never"
            ;;
        E)
            # sets the network's encryption
            # required when connecting to a hidden essid
            ENC="${OPTARG}"
            ;;
        t)
            TRIES=${OPTARG}
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done

X_NOTIFIY=true
function message        {
    if test -z "${X_NOTIFIY}"; then
        notify-send ${*}
    else
        echo ${*}
    fi
}

if ! command -v iwlist > /dev/null \
        || ! command -v iwconfig > /dev/null
then
    message "missing wireless-tools" && exit ${LINENO}
fi


IFACE=${IFACE:-$(ip link | grep -Po '^wlan[0-9]|wlp[^:]*' | head -1)}

if test 0 -ne $? || test -z ${IFACE}; then
    message "wireless iface not found" && exit ${LINENO}
fi

if test -n "${MACCHANGE_OPT}"; then
    if ! command -v macchanger &> /dev/null; then
        message "macchanger not installed" && exit ${LINENO}
    fi
    sudo ip link set ${IFACE} down || exit ${LINENO}
    sudo macchanger "-${MACCHANGE_OPT}" "${IFACE}" #|| exit ${LINENO}
fi

sudo ip link set ${IFACE} up

SCAN_RESULTS_FN=/tmp/wifi-scan-results

TRIES=${TRIES:-7}
if test -n "${ENC}" -a -n "${ESSID}"; then
    # no need to scan, we know the ESSID and the encyrption
    # need to skip scan to not err when connecting to hidden essid
    TRIES=0
    ESSIDS="${ESSID}"
fi

for I in $(seq ${TRIES}); do
    FAILED=""
    IWLIST_OUT=$(sudo iw dev ${IFACE} scan | tee ${SCAN_RESULTS_FN})
    # IWLIST_OUT=$(cat ${SCAN_RESULTS_FN})
    if test 0 -ne $?; then
        message "failed to scan"
        FAILED=true
    else
        ESSIDS=$(grep "^	SSID" <<< "${IWLIST_OUT}" \
                     | sed 's/.*SSID: \(.*\).*/\1/g')
        if test 0 -ne $? || test -z "${ESSIDS}"; then
            message "failed to read essids"
            FAILED=true
        fi
    fi

    if test "${FAILED}" != true; then
        break
    elif test $I -eq ${TRIES}; then
        echo "maximum retries exceeded."
        exit 1
    else
        message "retrying scan (${I}/${TRIES})..." && sleep 1
    fi
done


NETWORKS_DIR=${HOME}/.config/wifi-connect
mkdir -p ${NETWORKS_DIR}
cd ${NETWORKS_DIR}

COUNT=$(wc -l <<< "${ESSIDS}")
case ${COUNT} in
    1)
        ESSID="${ESSIDS}"
        ;;
    0)
        message "no networks found" && exit ${LINENO}
        ;;
    *)
        if test -z "${ESSID}"; then
            COMM=$(comm -12 <(ls -1 "${NETWORKS_DIR}" | sort) \
                        <(sort <<< "${ESSIDS}") \
                       | tee /tmp/common-essids)
            echo "known essids: ${COMM}"
            test -n "${COMM}" && COMM_LEN=$(wc -l <<< "${COMM}") || COMM_LEN=0
            if test "${ASK_ESSID}" != true -a ${COMM_LEN} = 1 \
                    -o "${ASK_ESSID}" = never -a ${COMM_LEN} -ge 1; then
                ESSID=$(head -1 <<< "${COMM}")
            elif test "${ASK_ESSID}" != never; then
                OLDIFS=$IFS
                IFS=$'\n'
                select ESSID in ${ESSIDS}; do
                    break
                done
                IFS=$OLDIFS
            else
                echo "no known essids"
                exit 1
            fi
        elif ! grep -F "${ESSID}" <<< "${ESSIDS}"; then
            message -e "specified essid '${ESSID}' not found in:\n${ESSIDS}" && exit ${LINENO}
        fi
esac

test -n "${ESSID}"
message "selected ${ESSID}"

# ENC=${ENC:-$(grep -B1 -F "${ESSID}" <<< "${IWLIST_OUT}"  | head -1 \
    #          | sed 's/.*Encryption key:\(.*\)/\1/')}

if test -s "${ESSID}"; then
    ENC=on
elif test -f "${ESSID}"; then
    ENC=off
else
    echo "select ecryption type: "
    select ENC in on off; do
        break
    done
fi

sudo pkill -e wpa_supplicant
sudo pkill -e dhclient

case "${ENC}" in
    on) #assume wpa
        if ! command -v wpa_supplicant > /dev/null \
                || ! command -v wpa_passphrase > /dev/null \
                || ! command -v expect > /dev/null; then
            message "missing wpasupplicant or expect" && exit ${LINENO}
        fi



        if test -n "${PASS_CANDS_FILE}"; then
            PASS_CANDS=$(cat "${PASS_CANDS_FILE}")
        elif test ! -f "${ESSID}" -o -n "${OVERWRITE}"; then
            if test -z "${PASS}"; then
                read -p "enter password for ${ESSID}: " PASS
            fi
            PASS_CANDS="${PASS}"
        else
            SKIP_WPA_PASSPHRASE=true
            PASS_CANDS="dummy"
        fi

        # TODO allow spaces
        CONNECTED=""
        for PASS in ${PASS_CANDS}; do
            if test "${SKIP_WPA_PASSPHRASE}" != true; then
                wpa_passphrase "${ESSID}" "${PASS}" | tee "${ESSID}"
            fi
            expect -df - <<EOF
set timeout -1
spawn sudo wpa_supplicant -i ${IFACE} -c "${ESSID}" -D nl80211,wext &
# expect "Established DTLS connection"
# expect -re "CTRL-EVENT-CONNECTED - Connection to 00:1a:1e:87:3c:01 completed"
# expect -re "CTRL-EVENT-CONNECTED - Connection to [0-9a-z:]+ completed"
expect {

"CTRL-EVENT-CONNECTED" {
       send -- "success\r"
       if {[fork]!=0} exit
       disconnect
}

"WRONG_KEY" {
       exec sudo kill [exp_pid]
       close
       exit 1
}

"Invalid configuration line" {
       exec sudo kill [exp_pid]
       close
       exit 1
}

}

EOF
            if test 0 = $?; then
                CONNECTED=true
                break
            fi
        done

        if test "${CONNECTED}" != true; then
            echo "error: wrong key?"
            exit 1
        fi

        # in the absence of expect....
        # sudo wpa_supplicant -i ${IFACE} -c ${ESSID} -D nl80211,wext &
        # sleep 10

        message "post CTRL-EVENT-CONNECTED expect"
        ;;

    off)
        sudo iwconfig ${IFACE} essid "${ESSID}" || exit ${LINENO}
        test -f "${ESSID}" || sudo touch "${ESSID}"
        ;;
    *)
        message "unknown encryption ${ENC}" && exit ${LINENO}
        ;;
esac

sudo dhclient -v ${IFACE} || exit ${LINENO}
