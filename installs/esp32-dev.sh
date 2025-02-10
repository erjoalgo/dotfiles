#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y git wget flex bison gperf python3 python3-pip \
     python3-venv cmake ninja-build ccache libffi-dev \
     libssl-dev dfu-util libusb-1.0-0

cd $(git-clone-maybe.sh "https://github.com/espressif/esp-idf")

./install.sh esp32

pip install esptool

# sudo apt-get install -y python3-argcomplete

# insert-text-block '# 7f462ab8-1c0f-41a0-a53d-c7081fcd2006-esp-completion' \
#                   ${HOME}/.profile-env <<"EOF"
# eval "$(register-python-argcomplete esptool.py espsecure.py espefuse.py)"
# EOF
