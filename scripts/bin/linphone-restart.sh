#!/bin/bash -x

set -euo pipefail

while pidof linphone; do
    sudo pkill -9 linphone
    sleep 1
done

linphonecsh init -c ~/.linphonerc
