#!/bin/bash -x

SOURCES=/etc/apt/sources.list.d/base.list
ADDITIONAL_COMPONENTS=${ADDITIONAL_COMPONENTS:-}

if grep ubuntu /etc/apt/sources.list; then
  echo "on ubuntu. skipping sources.list update"
  exit 0
fi

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
deb https://ftp.debian.org/debian $CODENAME main ${ADDITIONAL_COMPONENTS}
deb-src https://ftp.debian.org/debian $CODENAME main ${ADDITIONAL_COMPONENTS}

deb https://ftp.debian.org/debian ${CODENAME}-updates main ${ADDITIONAL_COMPONENTS}
deb-src https://ftp.debian.org/debian ${CODENAME}-updates main ${ADDITIONAL_COMPONENTS}
${INDICATOR_LINE}
EOF

sed -i 's/^deb cdrom/# &/g' $SOURCES

apt-get update
