#!/bin/bash

TIMES=${1:-1} && shift
IN="${1}" && shift
ROTATED="rotated-${IN}"

case ${TIMES} in
    1)
	TRANSPOSE='transpose=1'
	;;
    2)
	TRANSPOSE='transpose=1, transpose=1'
	;;
    3)
	TRANSPOSE='transpose=1, transpose=1, transpose=1'
	;;
    *)
	echo "bad # of 90-degree rotations" && exit ${LINENO}
	;;
esac

avconv -i "${IN}" -strict experimental -vf "${TRANSPOSE}" -v quiet "${ROTATED}" || exit ${LINENO}
mv "${ROTATED}" "${IN}"
