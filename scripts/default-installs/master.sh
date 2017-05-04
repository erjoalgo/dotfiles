#!/bin/bash

DEFAULT_INSTALLS=$(dirname $(readlink -f $0))
cd ${DEFAULT_INSTALLS}

pwd
for SCRIPT in $(find . -type l | sort); do
    echo "running ${SCRIPT}"
    ${SCRIPT}
    if test 0 -ne $?; then
	 echo "script ${SCRIPT} failed" && exit ${LINENO}
    fi
done
echo "all scripts ran successfully"
