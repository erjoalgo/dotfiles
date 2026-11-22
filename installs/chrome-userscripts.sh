#!/bin/bash

set -euo pipefail

cd "$(dirname "$0")"
./install-chrome-local-extension.sh ../inits/userscripts
