#!/bin/bash -x

set -euo pipefail

WS_URL=${WS_URL:-ws://k1c.arpa:9999}

while getopts "c:bBsSmMhlL" OPT; do
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
    l)
        REQUEST='{"method":"set","params":{"lightSw":1}}' # LED on
        ;;
    L)
        REQUEST='{"method":"set","params":{"lightSw":0}}' # LED off
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

python <<EOF
import asyncio
import websockets

async def send_message_and_exit(websocket_uri, message):
    try:
        # The 'async with' statement automatically handles opening and closing the connection
        async with websockets.connect(websocket_uri) as websocket:
            print(f"Sending message: '{message}'")
            await websocket.send(message)
            print("Message sent. Closing connection.")
            # The connection closes automatically upon exiting the 'async with' block

            # Optional: You can receive a response if the server sends one before closing
            # try:
            #     response = await asyncio.wait_for(websocket.recv(), timeout=1.0)
            #     print(f"Received response: {response}")
            # except asyncio.TimeoutError:
            #     print("No response received within timeout.")

    except ConnectionRefusedError:
        print(f"Connection refused. Is the server running at {websocket_uri}?")
    except Exception as e:
        print(f"An error occurred: {e}")

# Configuration
WS_URL = "${WS_URL}" # Replace with your WebSocket server URL
MESSAGE_TO_SEND = '${REQUEST}'

# Run the asynchronous function
if __name__ == "__main__":
    asyncio.run(send_message_and_exit(WS_URL, MESSAGE_TO_SEND))

EOF


