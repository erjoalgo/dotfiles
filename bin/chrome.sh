#!/bin/bash -x

set -euo pipefail

if ! EXE=$(which chromium-browser chrome google-chrome chromium | sort | head -1); then
    command -v "${EXE}"
fi

ENABLE_FEATURES="AsyncDns"
CURRENT_PERIOD=$(redshift-period.sh)

if test "${CURRENT_PERIOD}" != Daytime; then
    # ENABLE_FEATURES+=,WebContentsForceDark
    true
fi

USERSCRIPTS=$(realpath "$(dirname "$(realpath "$0")")/../inits/userscripts")
chrome-extension-fetch-vendor.sh "${USERSCRIPTS}" || true

ARGS=( \
       --high-dpi-support=1  \
           --force-device-scale-factor=2 \
           --disable-pings \
           --enable-gpu-rasterization \
           --enable-remote-extensions \
           --media-router=0 \
           --no-default-browser-check \
           --show-component-extension-options \
           --load-extension="${USERSCRIPTS}" \
           --restart \
           --flag-switches-begin \
           --enable-features="${ENABLE_FEATURES}" \
           --flag-switches-end
)

exec "${EXE}" "${ARGS[@]}" "${@}"
