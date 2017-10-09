#!/bin/bash -x

set -euo pipefail

# this script uses netcallback to expose a local port to the public
# by default it uses erjoalgo.com service port 1917, and exposes VNC port 5900

while getopts "H:s:p:Ph" opt; do
    case $opt in
	H)
	    NCB_HOST=$OPTARG
	    ;;
	s)
	    NCB_SERVICE_PORT=$OPTARG
	    ;;
	p)
	    LOCALHOST_PORT=${OPTARG}
	    ;;
	P)
	    PUBLIC=true
	    ;;
	h)
	    less $0
	    exit 0
	    ;;
    esac
done

PUBLIC=${PUBLIC:-}

if ! which java; then
    echo "java not available" && exit ${LINENO}
fi

test "${NCB_HOST}" -a -n "${NCB_SERVICE_PORT}" -a -n "${LOCALHOST_PORT}"

JAR="${HOME}/.local/bin/netcallback.jar"
mkdir -p $(dirname "${JAR}")


URL=http://netcallback.sourceforge.net/release/netcallback-1.3.1.zip


if ! test -e "${JAR}"; then
    cd /tmp
    wget "${URL}"
    unzip $(basename "${URL}")
    cp "$(basename "${URL}" .zip)/bin/$(basename ${JAR})" "${JAR}"
fi


JAR_MD5=be6e761718e9dd0b577eda1469b8e4d9

MD5_CMD=$(which md5 md5sum 2>/dev/null || true)
which ${MD5_CMD}

if ! ${MD5_CMD} "${JAR}" | grep -F "${JAR_MD5}"; then
    echo "unexpected md5 sum for jar. exiting..." && exit ${LINENO}
fi

echo "starting netcallback, exposing ${LOCALHOST_PORT} via ${NCB_HOST}..."

if test -z "${PUBLIC}"; then
    java -jar "${JAR}" -private -service ${NCB_HOST} ${NCB_SERVICE_PORT}  \
	 -tcp 127.0.0.1 ${LOCALHOST_PORT}
else
    java -jar "${JAR}" -public -servicePort ${NCB_SERVICE_PORT}  -tcpPort  ${LOCALHOST_PORT}
fi

