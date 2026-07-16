#!/bin/bash -x

set -euo pipefail

PORT=${1} && shift

RESP=$(echo -e 'HTTP/1.1 200 OK\r\nContent-Type: application/json; charset=utf-8\r\nContent-Length: 11\r\nConnection: close\r\n\r\nhello world')

while true; do
    nc -lp "${PORT}" <<< "${RESP}"
done

