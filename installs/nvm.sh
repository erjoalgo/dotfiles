#!/bin/bash -x

set -euo pipefail

git-clone-maybe.sh https://github.com/nvm-sh/nvm

~/git/nvm/install.sh
