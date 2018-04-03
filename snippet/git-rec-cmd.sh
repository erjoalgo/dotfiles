#!/bin/bash -x

CMD=${CMD:-git pull --ff-only}

# DEPTHLIMIT=${DEPTHLIMIT_OPT:-1}
if test -n "${DEPTHLIMIT_OPT}"; then
    DEPTHLIMIT_OPT="-maxdepth ${DEPTHLIMIT_OPT}"
fi

ORIG_PWD=$(pwd)
for DIR in $(find . ${DEPTHLIMIT_OPT} -name '.git' -type d); do
    cd "${ORIG_PWD}"
    cd "${DIR}/.." || continue
    ${CMD}
    if test $? -ne 0; then
	echo "failed '${CMD}' in $(pwd). recedit..."
	bash
    fi
done
