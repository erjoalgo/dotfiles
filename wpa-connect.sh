#!/bin/bash -x


if ! command -v wpa_supplicant > /dev/null \
	|| ! command -v wpa_passphrase > /dev/null \
	|| ! command -v expect > /dev/null; then
    echo "missing wireless-tools or expect" && exit ${LINENO}
fi

IFACE=${1:-$(ifconfig -a | grep -Po '^wlan[0-9]' | head -1)}

if test 0 -ne $? || test -z ${IFACE}; then
    echo "wireless iface not found" && exit ${LINENO}
fi

sudo ifconfig ${IFACE} up

IWLIST_OUT=$(sudo iwlist ${IFACE} scan)
ESSIDS=$(grep ESSID <<< "${IWLIST_OUT}" | sed 's/.*ESSID:"\(.*\)".*/\1/g')
if test 0 -ne $? || test -z ${ESSIDS}; then
    sudo iwlist ${IFACE} scan
    echo "couldn't scan for wireless networks" && exit ${LINENO}
fi

NETWORKS_DIR=/tmp/wpa
test -d ${NETWORKS_DIR} || mkdir -p ${NETWORKS_DIR}
cd ${NETWORKS_DIR}

COUNT=$(wc -l <<< "${ESSIDS}")
case ${COUNT} in
    1)
	ESSID="${ESSIDS}"
	;;
    0)
	echo "no networks found" && exit ${LINENO}
	;;
    *)
	IFS=$'\n'
	select ESSID in ${ESSIDS}; do
	    break
	done
esac

echo "selected ${ESSID}"
ENC=$(grep -B1 -F "${ESSID}" <<< "${IWLIST_OUT}"  | head -1 \
	 | sed 's/.*Encryption key:\(.*\)/\1/')


case "${ENC}" in
    on) #assume wpa
	read -p "enter password for ${ESSID}: " PASS
	wpa_passphrase "${ESSID}" "${PASS}" > "${ESSID}"
	
	sudo pkill -e wpa_supplicant
	sudo pkill -e dhclient

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
	echo "unknown encrption ${ENC}" && exit ${LINENO}
	;;
esac

sudo dhclient -v ${IFACE} 
