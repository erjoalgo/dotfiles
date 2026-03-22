#!/bin/bash -x

set -euo pipefail

WS_URL=${WS_URL:-ws://k1c.arpa:9999}

while getopts "c:bBsSmMh" OPT; do
    case ${OPT} in
    c) # as in --connect from wscat
        WS_URL=${OPTARG}
        ;;
    b)
        REQUEST='{"method":"set","params":{"fanCase":1}}' # back fan on
        ;;
    B)
        REQUEST='{"method":"set","params":{"fanCase":0}}' # back fan off
        ;;
    s)
        REQUEST='{"method":"set","params":{"fanAuxiliary":1}}' # side fan on
        ;;
    S)
        REQUEST='{"method":"set","params":{"fanAuxiliary":0}}' # side fan off
        ;;
    m)
        REQUEST='{"method":"set","params":{"fan":1}}' # model fan on
        ;;
    M)
        REQUEST='{"method":"set","params":{"fan":0}}' # model fan off
        ;;
    # g)
    #     GCODE=${OPTARG}
    #     exit ${LINENO}
        # ;;
    h)
        less "$0"
        exit 0
        ;;
    *)
        echo "unrecognized flag: ${OPT}" && exit ${LINENO}
        ;;
    esac
done
shift $((OPTIND -1))

cat <<EOF > /dev/null
GET ws://k1c.arpa:9999/ HTTP/1.1
Host: k1c.arpa:9999
Connection: Upgrade
Pragma: no-cache
Cache-Control: no-cache
User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/145.0.0.0 Safari/537.36
Upgrade: websocket
Origin: http://k1c.arpa
Sec-WebSocket-Version: 13
Accept-Encoding: gzip, deflate
Accept-Language: en-US,en;q=0.9
Sec-WebSocket-Key: ToLQIXpBcO28APJ+otL9/w==
Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits
EOF

test -n "${REQUEST:-}"
wscat -c "${WS_URL}"  -x "${REQUEST}"
exit 0

# Send a message and wait for a response
echo "${REQUEST}" | \
    wscat -c "${WS_URL}" | \
    while read LINE; do
        echo "Received: $LINE"
        # Exit after receiving a specific response or based on time
        if [[ "$line" == *"expected-response"* ]]; then
            pkill -P $$ wscat
            break
        fi
    done
