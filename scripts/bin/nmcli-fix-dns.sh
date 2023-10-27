#!/bin/bash -x

set -euo pipefail

# active wifi connection
CONN=$(nmcli c show  --active |  \
           grep wifi |  \
           grep -Po '[a-z0-9-]{36}')

LINES=$(nmcli c show "${CONN}" |  \
            grep IP4.DNS | cut -d: -f2 | tr -d ' ' |  \
            sed 's/^/nameserver /g')

sudo insert-text-block -b \
     '# baefa02e-1a3a-43ea-ae92-0b69e138fd2e-nmcli-use-current-connection-dns' \
     /etc/resolv.conf <<< "${LINES}"

