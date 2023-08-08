#!/bin/bash -x

set -euo pipefail

while getopts "d:p:c:fh" OPT; do
    case ${OPT} in
        d)
            HOST=${OPTARG}
            ;;
        p)
            PORT=${OPTARG}
            ;;
        c)
            CONFIG_FILENAME=${OPTARG}
            ;;
        f)
            FORCE=true
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

go install -v github.com/shadowsocks/go-shadowsocks2@latest

CONFIG_FILENAME=${CONFIG_FILENAME:-/etc/shadowsocks/config.json}
sudo mkdir -p $(dirname "${CONFIG_FILENAME}")

HOST=${HOST:-$(whats-my-ip.sh)}
PORT=${PORT:-46859}

if test -e "${CONFIG_FILENAME}" -a ! "${FORCE:-}" = true; then
    # read -p"confirm overwriting existing config: ${CONFIG_FILENAME}"
    true
fi


genpasswd () {
    local LEN=${1:-16};
    tr -dc A-Za-z0-9_ < /dev/urandom | head -c ${LEN} | xargs
}

PASS=$(genpasswd) || true
# METHOD=aes-256-cfb
METHOD=AEAD_AES_256_GCM

cat <<EOF | sudo tee ${CONFIG_FILENAME} > /dev/null
{
    "server": "${HOST}",
    "server_port":${PORT},
    "password":"${PASS}",
    "timeout":120,
    "method": "${METHOD}"
}
EOF

SANS_EXT="${CONFIG_FILENAME%.*}"

ENV_FILE="${SANS_EXT}.env"

function ss-url {
    echo ss://${METHOD}:${PASS}@${HOST}:${PORT}
}

SS_URL_LOCAL=$(HOST=0.0.0.0 ss-url)
SS_URL=$(ss-url)

cat<<EOF | sudo tee "${ENV_FILE}" > /dev/null
SS_URL=${SS_URL_LOCAL}
EOF

sudo qrencode -o "${SANS_EXT}.png" <<< "ss://$(echo -n ${SS_URL} | base64 -w 0)"


install-systemd-service.sh shadowsocks <<EOF
[Unit]
Description=shadowsocks service
StartLimitInterval=0

[Service]
ExecStart=$(which go-shadowsocks2) -s "\${SS_URL}"
User=$(whoami)
Restart=always
RestartSec=60
SyslogIdentifier=shadowsocks
EnvironmentFile=${ENV_FILE}

[Install]
WantedBy=multi-user.target
EOF


