#!/bin/bash -x

if test -n "$TEST"; then
    cp /etc/apt/sources.list{,.bak}
    SOURCES=/etc/apt/sources.list.bak
else
    SOURCES=/etc/apt/sources.list
fi

CODENAME=$(
. /etc/os-release
printf '%s\n' "$VERSION" | grep -o '[a-z]*'
)

cat << EOF >> $SOURCES
deb http://ftp.debian.org/debian $CODENAME main
deb-src http://ftp.debian.org/debian $CODENAME main

deb http://ftp.debian.org/debian ${CODENAME}-updates main
deb-src http://ftp.debian.org/debian ${CODENAME}-updates main
EOF

sed -i 's/^deb cdrom/# &/g' $SOURCES

apt-get update
