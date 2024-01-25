#!/bin/bash -x

set -euo pipefail

sudo sed -i  \
     's/rights="none" pattern="PDF"/rights="read | write" pattern="PDF"/'  \
     /etc/ImageMagick-*/policy.xml
