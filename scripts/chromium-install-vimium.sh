#!/bin/bash -x

set -euo pipefail

# https://chrome.google.com/webstore/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb
# https://developer.chrome.com/extensions/external_extensions

TOP=${HOME}/.config/chromium/Default/Extensions
FN=${TOP}/dbepggeogbaibhgnhhndojpepiihcmeb.json
cat <<EOF > ${FN}
{
    "external_update_url":
    "https://chrome.google.com/webstore/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb"
}
EOF
sudo chmod a+r ${FN}
