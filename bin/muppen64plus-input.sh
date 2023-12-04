#!/bin/bash -x

set -euo pipefail

/usr/bin/sg input mupen64plus ${*}
