#!/bin/bash -x

LOGFILE=${LOGFILE:-/var/log/$(basename ${1})}
# test -w ${LOGFILE} -o ! -e ${LOGFILE} || exit ${LINENO}

sudo touch ${LOGFILE} && sudo chown $(whoami):$(whoami) ${LOGFILE}

${*} >> ${LOGFILE} 2>&1 < /dev/null &
