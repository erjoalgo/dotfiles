#!/bin/bash

set -euo pipefail

# read jar file paths from STDIN
# produce a list of class -> jar mappings
# useful for determining if a class is defined
# by multiple jars

while read JAR
do
    for CLASS in $(jar tf ${JAR} | grep '[.]class'); do
	if grep "META-INF\|[$]" <<< ${CLASS} > /dev/null; then
	    continue
	fi
	
	echo $(sed -e 's|/|.|g' -e 's/[.]class//' <<< ${CLASS}) \
	     ${JAR}
    done;
done < ${1:-/dev/stdin}
