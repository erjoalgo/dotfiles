#!/bin/bash -x

if ! command -v alsamixer; then
    sudo apt-get install -y alsa-tools alsa-utils
fi

if ! command -v mpg321; then
    sudo apt-get install -y mpg321
fi
