#!/bin/bash


while getopts "of:hp:e:m:" OPT; do
    case ${OPT} in
	o)
	    OVERWRITE="true"
	    ;;
	e)
	    ESSID="${OPTARG}"
	    ;;
	p)
	    PASS="${OPTARG}"
	    ;;
	m)
	    MACCHANGE_OPT="${OPTARG}"
	    ;;
	i)
	    IFACE="${OPTARG}"
	    ;;
	h)
	    less $0
	    exit 0
	    ;;
    esac
done

if ! command -v iwlist > /dev/null \
	|| ! command -v iwconfig > /dev/null
then
    echo "missing wireless-tools" && exit ${LINENO}
fi


IFACE=${IFACE:-$(ifconfig -a | grep -Po '^wlan[0-9]' | head -1)}

if test 0 -ne $? || test -z ${IFACE}; then
    echo "wireless iface not found" && exit ${LINENO}
fi

if test -n "${MACCHANGE_OPT}"; then
    if ! command -v macchanger &> /dev/null; then
	echo "macchanger not installed" && exit ${LINENO}
    fi
    sudo ifconfig ${IFACE} down || exit ${LINENO}
    sudo macchanger "-${MACCHANGE_OPT}" "${IFACE}" #|| exit ${LINENO}
fi

sudo ifconfig ${IFACE} up

TRIES=7
for i in $(seq ${TRIES}); do
    IWLIST_OUT=$(sudo iwlist ${IFACE} scan)
    ESSIDS=$(grep ESSID <<< "${IWLIST_OUT}" | sed 's/.*ESSID:"\(.*\)".*/\1/g')
    if test 0 -ne $? || test -z "${ESSIDS}"; then
	if test $i -eq ${TRIES}; then
	    echo "couldn't scan for wireless networks or no networks available" && exit ${LINENO}
	else
	    echo "retrying scan..." && sleep 1
	fi
    else
	break
    fi
done





COUNT=$(wc -l <<< "${ESSIDS}")
case ${COUNT} in
    1)
	ESSID="${ESSIDS}"
	;;
    0)
	echo "no networks found" && exit ${LINENO}
	;;
    *)
	if test -z "${ESSID}"; then
	    IFS=$'\n'
	    select ESSID in ${ESSIDS}; do
		break
	    done
	elif ! grep -F "${ESSID}" <<< "${ESSIDS}"; then
	     echo -e "specified essid '${ESSID}' not found in:\n${ESSIDS}" && exit ${LINENO}
	fi
esac

echo "selected ${ESSID}"
ENC=$(grep -B1 -F "${ESSID}" <<< "${IWLIST_OUT}"  | head -1 \
	 | sed 's/.*Encryption key:\(.*\)/\1/')


sudo pkill -e wpa_supplicant
sudo pkill -e dhclient

case "${ENC}" in
    on) #assume wpa
	if ! command -v wpa_supplicant > /dev/null \
		|| ! command -v wpa_passphrase > /dev/null \
		|| ! command -v expect > /dev/null; then
	    echo "missing wpasupplicant or expect" && exit ${LINENO}
	fi

	NETWORKS_DIR=/etc/wpa-connect
	test -d ${NETWORKS_DIR} || sudo mkdir -p ${NETWORKS_DIR}
	cd ${NETWORKS_DIR}

	if test ! -f "${ESSID}" -o -n "${OVERWRITE}"; then
	    read -p "enter password for ${ESSID}: " PASS
	    wpa_passphrase "${ESSID}" "${PASS}" | sudo tee "${ESSID}"
	fi

	cat <<EOF | expect -df -
set timeout -1
eval spawn sudo wpa_supplicant -i ${IFACE} -c ${ESSID} -D nl80211,wext &
# expect "Established DTLS connection"
# expect -re "CTRL-EVENT-CONNECTED - Connection to 00:1a:1e:87:3c:01 completed"
# expect -re "CTRL-EVENT-CONNECTED - Connection to [0-9a-z:]+ completed"
expect -re "CTRL-EVENT-CONNECTED - Connection to .* completed"
disconnect
exit

EOF
	# sudo wpa_supplicant -i ${IFACE} -c ${ESSID} -D nl80211,wext &
	# sleep 10

	echo "post CTRL-EVENT-CONNECTED expect"
	;;

    off)
	sudo iwconfig ${IFACE} essid "${ESSID}" || exit ${LINENO}
	;;
    *)
	echo "unknown encryption ${ENC}" && exit ${LINENO}
	;;
esac

sudo dhclient -v ${IFACE}
