#!/bin/bash

FFPID=$(pidof firefox-esr)
if test 0 -ne $? || test -z "${FFPID}"; then
    echo "couldn't get ff pid" && exit ${LINENO}
fi

STAT=$(ps -p ${FFPID} -o stat | tail -1)
case "${STAT}" in
    Tl)
	SIG=SIGCONT
	;;
    Sl)
	SIG=SIGSTOP
	;;
    *)
	echo "unknown state: ${STAT}" && exit ${LINENO}
	;;
esac

echo "sending ${SIG}"
kill -s ${SIG} ${FFPID}
