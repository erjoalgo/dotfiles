#!/bin/bash

set -euo pipefail

# ip link | grep -Po '^wlan[0-9]|wlp[^:]*'

find /sys/class/net -mindepth 1 -maxdepth 1 |  \
    xargs -L 1 basename |  \
    grep -Po '^(wlan|wlp|wlx).*'
