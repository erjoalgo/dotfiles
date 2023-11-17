#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y python-pip
pip install chromeurl


chromeurl --install native
sudo $(which chromeurl) --install extension
