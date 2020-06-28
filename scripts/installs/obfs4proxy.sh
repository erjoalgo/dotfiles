#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y tor obfs4proxy

which insert-text-block

while true; do
    PORT=$(python -c "import random; print(random.randint(1025, 65535))")
    if ! exec 6<>/dev/tcp/localhost/${PORT}; then
        break
    fi
done

sudo insert-text-block \
     '# 7c822fec-0e8e-4d13-a28c-2bfe74f4ad29-tor-obfs-bridge' \
     /etc/tor/torrc <<EOF
#Bridge config
RunAsDaemon 1
ORPort ${PORT}
BridgeRelay 1
ServerTransportPlugin obfs4 exec /usr/bin/obfs4proxy
ExtORPort auto

#Set your bridge nickname and contact info
ContactInfo <your-contact-info>
Nickname pickanickname

PublishServerDescriptor 0
EOF

sudo service tor restart

echo "listening on port ${PORT}"
