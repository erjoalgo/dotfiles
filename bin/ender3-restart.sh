#!/bin/bash -x

set -euo pipefail


hass-cli service call automation.trigger  \
         --arguments entity_id=automation.ender_3_coolby_restart_after_delay &

ssh coolby.local sudo shutdown now
