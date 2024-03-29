#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y blender cura

pip install git+https://github.com/erjoalgo/octoprint-cli

INI=${HOME}/.config/octoprint-cli.ini

if ! test -e "${INI}"; then
    read -p"enter octorpint server URL, including scheme and basic auth: " SERVER_ADDRESS
    x-www-browser "${SERVER_ADDRESS}"
    read -p"enter octorpint API key: " API_KEY
    insert-text-block '# 0754c16b-b5aa-447d-81d6-b4fa758a56bc-octoprint-cli-config' \
                      "${INI}" <<EOF
[server]
;Set OctoPrint server address and x-api-key
ServerAddress = ${SERVER_ADDRESS}
ApiKey = ${API_KEY}

[preferences]
;Set if the program uses colored or formatted text, this setting is turned off on windows due to cmd and powershell limitations
FormattedText = true
;Set if the program should check for updates
UpdateCheck = true

[printer]
;Set maximum temperature that printer can be set to
MaxExtruderTemp = 250
MaxBedTemp = 85
EOF
fi

octoprint-cli files list

pip install trimesh

# creality slicer
# https://file2-cdn.creality.com/file/1946156fb58ee2564ae71424e44c05a3/Creality_Print-v3.11.1-Ubutu-x86_64-Release.AppImage?spm=..page_1995737.download_support_two_1.1

pip install git+https://github.com/erjoalgo/pystl
