#!/bin/bash -x

SOURCES=/etc/apt/sources.list

if ! test -e ${SOURCES}.bak; then
    cp /etc/apt/sources.list{,.bak}
fi

if ! test -e "${TEST}"; then
    SOURCES=${SOURCES}.bak
fi

. /etc/os-release
if test -n "${VERSION_CODENAME}"; then
    CODENAME=$VERSION_CODENAME
else
    CODENAME=$(printf '%s\n' "$VERSION" | grep -o '[a-z]*')
fi

cat << EOF >> $SOURCES
deb http://ftp.debian.org/debian $CODENAME main
deb-src http://ftp.debian.org/debian $CODENAME main

deb http://ftp.debian.org/debian ${CODENAME}-updates main
deb-src http://ftp.debian.org/debian ${CODENAME}-updates main
EOF

sed -i 's/^deb cdrom/# &/g' $SOURCES

apt-get update
