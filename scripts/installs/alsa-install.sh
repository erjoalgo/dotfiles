#!/bin/bash -x

if ! command -v alsamixer; then
    sudo apt-get install -y alsa-base alsa-utils
fi

if ! command -v mpg321; then
    sudo apt-get install -y mpg321
fi
