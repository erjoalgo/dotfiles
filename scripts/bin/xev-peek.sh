#!/bin/bash -x

set -euo pipefail

{ xev | grep keycode; } &
sleep 1;
pkill xev
