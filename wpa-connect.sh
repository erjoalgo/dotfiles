#!/bin/bash -x

IFACE=${1:-$(ifconfig | grep -Po '^wlan[0-9]' | head -1)}

if ! command -v wpa_supplicant || ! command -v wpa_passphrase ||  \
	! command -v expect; then
    echo "missing wireless-tools or expect" && exit ${LINENO}
fi

if test 0 -ne $? || test -z ${IFACE}; then
    echo "wireless iface not found"
fi

IWLIST_OUT=$(sudo iwlist ${IFACE} scan)
NETWORKS=$(grep ESSID <<< "${IWLIST_OUT}" | sed 's/.*ESSID:"\(.*\)".*/\1/g')
if test 0 -ne $? || test -z ${NETWORKS}; then
    sudo iwlist ${IFACE} scan
    echo "couldn't scan for wireless networks" && exit ${LINENO}
fi

NETWORKS_DIR=/tmp/wpa
test -d ${NETWORKS_DIR} || mkdir -p ${NETWORKS_DIR}
cd ${NETWORKS_DIR}

COUNT=$(wc -l <<< "${NETWORKS}")
case ${COUNT} in
    1)
	NETWORK="${NETWORKS}"
	;;
    0)
	echo "no networks found" && exit ${LINENO}
	;;
    *)
	IFS=$'\n'
	select NETWORK in ${NETWORKS}; do
	    break
	done
esac

echo "selected ${NETWORK}"

read -p "enter password for ${NETWORK}: " PASS
wpa_passphrase "${NETWORK}" "${PASS}" > "${NETWORK}"
    
sudo pkill -e wpa_supplicant
sudo pkill -e dhclient

cat <<EOF | expect -df - 
set timeout -1
eval spawn sudo wpa_supplicant -i ${IFACE} -c ${NETWORK} -D nl80211,wext &
# expect "Established DTLS connection"
# expect -re "CTRL-EVENT-CONNECTED - Connection to 00:1a:1e:87:3c:01 completed"
# expect -re "CTRL-EVENT-CONNECTED - Connection to [0-9a-z:]+ completed"
expect -re "CTRL-EVENT-CONNECTED - Connection to .* completed"
disconnect
exit

EOF
# sudo wpa_supplicant -i ${IFACE} -c ${NETWORK} -D nl80211,wext &
# sleep 10

echo "post CTRL-EVENT-CONNECTED expect"
sudo dhclient -v ${IFACE} 
