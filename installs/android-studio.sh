#!/bin/bash -x

set -euo pipefail

URL="https://dl.google.com/dl/android/studio"
URL+="/ide-zips/3.4.1.0/android-studio-ide-183.5522156-linux.tar.gz"

BASE=$(basename "${URL}")
LOCAL=${HOME}/Downloads/${BASE}
test -e "${LOCAL}" || curl "${URL}" -o "${LOCAL}"

SRC=${HOME}/src/android-studio
test -d "${SRC}" || tar -C "$(dirname ${SRC})" -axvf "${LOCAL}"
cd "${SRC}"

BIN_D="${SRC}/bin"
test -d "${BIN_D}"

PROFILE_ENV=${PROFILE_ENV:-${HOME}/.profile-env}

insert-text-block '# ea5c84ba-cd6c-4e0e-9bd5-da72a6bd845e-android-studio-bin-dir'  \
                  ${PROFILE_ENV} << EOF
export PATH=\$PATH:${BIN_D}
# may not exist until after running studio.sh
export ANDROID_HOME=${HOME}/Android/Sdk/
EOF

echo "in a new terminal, run studio.sh"
