#!/bin/bash -x

set -euo pipefail

install-xdg-open-handler.sh  \
    -m inode/directory -c emacsclient-wrapper.sh -b  \
    "# 89fbb55a-4a89-4b19-8e2f-5d14697e9a2f-open-dirs-with-emacs"
