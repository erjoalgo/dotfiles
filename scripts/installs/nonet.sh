#!/bin/bash -x

set -euo pipefail

USERNAME=nonet

id -u ${USERNAME} || sudo useradd ${USERNAME} -m
sudo iptables -A OUTPUT -p all -m owner --uid-owner ${USERNAME} -j DROP
