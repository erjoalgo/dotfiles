#!/bin/bash -x

IFACE=${1:-$(ifconfig | grep -Po '^wlan[0-9]' | head -1)}

if ! command -v wpa_supplicant || ! command -v wpa_passphrase ||  \
	! command -v expect; then
    echo "missing wireless-tools or expect" && exit ${LINENO}
fi

if test 0 -ne $? || test -z ${IFACE}; then
    echo "wireless iface not found"
fi

NETWORKS=$(sudo iwlist ${IFACE} scan |& grep ESSID | sed 's/.*ESSID:"\(.*\)".*/\1/g')
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
	select NETWORK in ${NETWORKS}; do
	    echo 1
	done
esac

echo "selected ${NETWORK}"

read PASS -p "enter password for ${NETWORK}"
wpa_passphrase "${NETWORK}" "${PASS}" > "${NETWORK}"
    
sudo pkill -e wpa_supplicant
sudo pkill -e dhclient

cat <<EOF | expect -df - 
set timeout -1
eval spawn echo sudo wpa_supplicant -i ${WLAN} -c ${NETWORK} -D nl80211,wext &
# expect "Established DTLS connection"
# expect -re "CTRL-EVENT-CONNECTED - Connection to 00:1a:1e:87:3c:01 completed"
# expect -re "CTRL-EVENT-CONNECTED - Connection to [0-9a-z:]+ completed"
expect -re "CTRL-EVENT-CONNECTED - Connection to .* completed"
disconnect
exit

EOF
echo "post CTRL-EVENT-CONNECTED expect"
sudo dhclient -v ${IFACE} 
