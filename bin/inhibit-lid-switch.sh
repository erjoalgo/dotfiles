#!/bin/bash -x

set -euo pipefail

TIMESPEC=${*}

systemd-inhibit --what=handle-lid-switch sleep ${TIMESPEC}
