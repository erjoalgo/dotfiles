#!/bin/bash -x

set -euo pipefail
while killall pulseaudio; do sleep 1; done
/etc/init.d/alsa-utils restart
sudo alsactl restore
# this tends to return non-zero exit on success...
sudo alsactl init || true

speaker-test -Ddefault -c2 -l1
