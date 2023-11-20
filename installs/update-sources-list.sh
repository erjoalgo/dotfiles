#!/bin/bash -x

SOURCES=/etc/apt/sources.list.d/base.list

if test ${UID} -ne 0; then
    sudo "$0" "${@}"
    exit $?
fi

if grep ubuntu /etc/apt/sources.list; then
  echo "on ubuntu. skipping sources.list update"
  exit 0
fi

rm -rf /etc/apt/sources.list~

if ! test -z "${TEST:-}"; then
    cp /etc/apt/sources.list{,.test}
    SOURCES=${SOURCES}.test
fi

. /etc/os-release
if test -n "${VERSION_CODENAME}"; then
    CODENAME=$VERSION_CODENAME
else
    CODENAME=$(printf '%s\n' "$VERSION" | grep -o '[a-z]*')
fi

INDICATOR_LINE="# c1346404-34f5-4223-9142-90f9468c98a2-update-sources"

cat << EOF >> ${SOURCES}
${INDICATOR_LINE}
# do not edit this file by hand!

deb https://deb.debian.org/debian $CODENAME main
deb-src https://deb.debian.org/debian $CODENAME main

deb https://deb.debian.org/debian-security/ $CODENAME-security main
deb-src https://deb.debian.org/debian-security/ $CODENAME-security main

deb https://deb.debian.org/debian $CODENAME-updates main
deb-src https://deb.debian.org/debian $CODENAME-updates main

${INDICATOR_LINE}
EOF

for SOURCE in $(find /etc/apt/ -name '*list'); do
    sed -i 's/^deb cdrom/# &/g' $SOURCE
    sed -i 's/^.*ftp/# &/g' $SOURCE
done

sudo sed -i 's/^[^#]/# &/g' /etc/apt/sources.list

apt-get update
