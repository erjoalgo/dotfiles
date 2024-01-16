#!/bin/bash -x

set -euo pipefail

REPO=${HOME}/git/firetv-remote

test -d "${REPO}" || git clone ssh://git@github.com/erjoalgo/firetv-remote "${REPO}"
cd "${REPO}"
git checkout insignia
git pull --ff-only
cd ${REPO}
npm install

install-systemd-service.sh -u firetv <<EOF
[Unit]
Description=firetv remote
After=default.target

[Service]
ExecStart=$(which npm) run start
SyslogIdentifier=firetv-remote
Restart=always
Type=simple
Environment=PATH=$PATH:$(dirname $(which npm))
WorkingDirectory=${REPO}

[Install]
WantedBy=default.target
EOF

systemctl --user restart firetv

# Local Variables:
# mode: sh-mode
# End:
