#!/bin/bash -x

set -euo pipefail

HOST=${1} && shift

ssh-keyscan ${HOST} | ssh-keygen -l -f -
