#!/bin/bash -x

set -euo pipefail

while getopts "hm:" OPT; do
    case ${OPT} in
    m)
        MINS=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

command -v at
MINS=${MINS:-2}
test "${MINS}" -le 60

function genpasswd {
    LEN=${1:-16}
    tr -dc A-Za-z0-9_ < /dev/urandom  \
        | head -c ${LEN} \
        | tr -d ' '  \
        || true # prevent SIGPIPE from causing 141 exit code
}

USERNAME=$(genpasswd 8)
PASS=$(genpasswd 12)

TOMORROW=$(date '+%Y-%m-%d' -d '+1days')
sudo useradd "${USERNAME}" --expiredate "${TOMORROW}"
sudo chpasswd <<EOF
${USERNAME}:${PASS}
EOF

LINE_ID="# 56921664-b73c-4409-8219-e63c61b8f589-allow-tempuser-${USERNAME}"
SSHD_CONFIG=/etc/ssh/sshd_config

sudo insert-text-block "${LINE_ID}" "${SSHD_CONFIG}" <<EOF
Match User "${USERNAME}"
	PasswordAuthentication yes
EOF
sudo service ssh restart

sudo at now + "${MINS}" minutes<<EOF
logger "locking and deleting user ${USERNAME}"
killall --user ${USERNAME}
usermod --lock ${USERNAME}
userdel -f ${USERNAME}
insert-text-block '${LINE_ID}' ${SSHD_CONFIG} -d
logger "deleted user ${USERNAME}"
EOF

echo "${USERNAME} ${PASS}"
