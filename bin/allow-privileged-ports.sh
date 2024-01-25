#!/bin/bash -x

set -euo pipefail

EXE=${1} && shift
setcap 'cap_net_bind_service=+ep' ${EXE}
