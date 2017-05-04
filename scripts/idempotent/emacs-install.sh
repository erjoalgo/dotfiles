#!/bin/bash -x

if ! command -v emacs; then
    sudo apt-get install -y emacs emacs24-el
fi
