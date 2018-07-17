#!/bin/bash -x

set -euo pipefail

which go

go get github.com/shadowsocks/shadowsocks-go/cmd/shadowsocks-server

SAMPLE_CONFIG=${HOME}/.shadow-sample-config.json

cat <<EOF > ${SAMPLE_CONFIG}
{
    "server": "***REMOVED***",
    "server_port":***REMOVED***,
    "local_address":"127.0.0.1",
    "local_port":1080,
    "password":"***REMOVED***",
    "timeout":120,
    "method": "aes-256-cfb"
}
EOF

echo "run: ss-local -c ${SAMPLE_CONFIG}"
