#!/bin/bash -x

set -euo pipefail

curl -s https://www.iplocation.net/ |  \
    grep -Po 'city.*?country.*?,' |  \
    head -1
