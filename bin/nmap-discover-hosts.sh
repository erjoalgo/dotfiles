#!/bin/bash -x

if true; then
    LOCALHOST='127.0.0.1'
    SUBNETS=$(ip address |\
		  grep -oP 'inet [0-9./]*'|\
		  cut -d' ' -f2|\
		  grep -v ${LOCALHOST})

   if test $(wc -l <<< "${SUBNETS}") -gt 1; then
       select SUBNET in ${SUBNETS}; do
	   echo "selected ${SUBNET}"
	   break
       done
   else
       SUBNET=$(tr -d ' ' <<< ${SUBNETS})
   fi
fi

ARGS=${*}

if test $# -lt 1; then
    ARGS=-sn
fi

nmap ${SUBNET} ${ARGS}
