#!/bin/bash -x

set -euo pipefail

which go

go get github.com/shadowsocks/shadowsocks-go/cmd/shadowsocks-server
