#!/bin/bash -x

set -euo pipefail

sudo udevadm control --log-priority=debug
sudo journalctl -f
