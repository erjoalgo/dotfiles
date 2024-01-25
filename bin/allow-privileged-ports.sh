#!/bin/bash -x

set -euo pipefail

EXE=${1} && shift
sudo setcap 'cap_net_bind_service=+ep' ${EXE}
