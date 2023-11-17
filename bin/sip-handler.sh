#!/bin/bash -x

set -euo pipefail

echo ${*} >> /tmp/sip.log

TEL=$(sed "s/[^0-9]//g" <<< ${1}) && shift

echo ${TEL} >> /tmp/sip.log

DOMAIN="sanjose2.voip.ms"
SIP="sip:1${TEL}@${DOMAIN}"

# xmessage "calling ${SIP}" -timeout 2 -center

ekiga -c "${SIP}"
