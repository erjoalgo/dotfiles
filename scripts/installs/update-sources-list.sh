#!/bin/bash -x

SOURCES=/etc/apt/sources.list

if ! test -e ${SOURCES}.bak; then
    cp /etc/apt/sources.list{,.bak}
fi

if ! test -z "${TEST:-}"; then
    SOURCES=${SOURCES}.bak
fi

. /etc/os-release
if test -n "${VERSION_CODENAME}"; then
    CODENAME=$VERSION_CODENAME
else
    CODENAME=$(printf '%s\n' "$VERSION" | grep -o '[a-z]*')
fi

INDICATOR_LINE="# c1346404-34f5-4223-9142-90f9468c98a2-update-sources"

if ! grep -F "${INDICATOR_LINE}" "${SOURCES}"; then
    cat << EOF >> ${SOURCES}
${INDICATOR_LINE}
deb http://ftp.debian.org/debian $CODENAME main
deb-src http://ftp.debian.org/debian $CODENAME main

deb http://ftp.debian.org/debian ${CODENAME}-updates main
deb-src http://ftp.debian.org/debian ${CODENAME}-updates main
EOF
else
    echo "skipping updating ${SOURCES} since indicator line was found: "
    echo "${INDICATOR_LINE}"
fi

sed -i 's/^deb cdrom/# &/g' $SOURCES

apt-get update
