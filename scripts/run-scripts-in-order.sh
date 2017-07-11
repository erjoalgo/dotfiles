#!/bin/bash

SELF=$(readlink -f $0)
SELF_D=$(dirname "${SELF}")
SCRIPTS_D=${1:-$SELF_D}
SHORT_CIRCUIT=${SHORT_CIRCUIT:-true}

cd ${SCRIPTS_D} || exit ${LINENO}
echo "running scripts in ${SCRIPTS_D}"

for SCRIPT in $(find -L . -maxdepth 1 -type f -not -samefile "${0}"| sort); do
    echo "running ${SCRIPT}..."
    ${SCRIPT}
    continue
    if test 0 -ne $?; then
	echo "script ${SCRIPT} failed!"
	test $SHORT_CIRCUIT != true || exit ${LINENO}
    fi
done

echo "all scripts ran successfully"
