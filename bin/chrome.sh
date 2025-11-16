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

ARGS=( \
       --high-dpi-support=1  \
           --force-device-scale-factor=2 \
           --disable-pings \
           --enable-gpu-rasterization \
           --enable-remote-extensions \
           --force-device-scale-factor=2 \
           --high-dpi-support=1 \
           --load-extension=/usr/share/chromium/extensions/acofndgbcimipbpeoplfjcapdbebbmca.json,/usr/share/chromium/extensions/chemannjcbmbebomonaldbcjkmobopno.json,/usr/share/chromium/extensions/dbepggeogbaibhgnhhndojpepiihcmeb.json,/usr/share/chromium/extensions/eibefbdcoojolecpoehkpmgfaeapngjk.json,/usr/share/chromium/extensions/fimgfedafeadlieiabdeeaodndnlbhid.json,/usr/share/chromium/extensions/lfpjkncokllnfokkgpkobnkbkmelfefj.json,/usr/share/chromium/extensions/nigigpmchbpkjjgncmpiggfnikllldlh.json,/usr/share/chromium/extensions/related.json \
           --media-router=0 \
           --no-default-browser-check \
           --show-component-extension-options \
           --restart \
           --flag-switches-begin \
           --enable-features=${ENABLE_FEATURES} \
           --flag-switches-end
)

${EXE} ${ARGS[@]} ${*}
