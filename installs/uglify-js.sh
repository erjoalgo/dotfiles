#!/bin/bash -x

set -euo pipefail


npm config set prefix '~/.local/'
npm install -g uglify-js
