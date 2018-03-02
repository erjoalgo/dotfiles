#!/bin/bash -x

if true; then
    LOCALHOST='127.0.0.1'
    SUBNETS=$(ifconfig -a |\
		  grep -oP '(?<=inet addr:)[0-9.]+' |\
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

ARGS=${*}

if test $# -lt 1; then
    ARGS=-sn
fi

nmap ${SUBNET} ${ARGS}
