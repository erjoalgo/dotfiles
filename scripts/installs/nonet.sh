#!/bin/bash -x

set -euo pipefail

USERNAME=nonet

sudo useradd ${USERNAME} -m
sudo iptables -A OUTPUT -p all -m owner --uid-owner ${USERNAME} -j DROP
