#!/bin/bash -x

set -euo pipefail

ANDROID_DEFAULT_DEVICE=${ANDROID_DEFAULT_DEVICE:-$(emulator -list-avds | head -1)}
emulator-avd "${ANDROID_DEFAULT_DEVICE}"
