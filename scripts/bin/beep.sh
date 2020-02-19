#!/bin/bash

set -euo pipefail

sudo env -u SUDO_GID -u SUDO_COMMAND -u SUDO_USER -u SUDO_UID beep ${*}
