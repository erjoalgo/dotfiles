#!/bin/bash -x

SUBNET=$1

if test -z ${SUBNET}; then
    LOCALHOST='127.0.0.1'
    SUBNETS=$(ifconfig -a |\
		     grep -oP 'inet addr:.*? '|\
		     cut -d: -f2|\
		     grep -v ${LOCALHOST})

   if test $(wc -l <<< "${SUBNETS}") -gt 1; then
       select SUBNET in ${SUBNETS}; do
	   echo "selected ${SUBNET}"
	   break
       done
   else
       SUBNET=$(tr -d ' ' <<< ${SUBNETS})
   fi
   SUBNET=$(sed 's|[0-9]*$|1/24|' <<< ${SUBNET})
fi

# subnet = re.sub("[0-9]+$", "1/24", subnet)
nmap -sP ${SUBNET}
