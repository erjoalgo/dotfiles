#!/bin/bash

emacs &
$(which chromium-browser chromium chrome google-chrome) &
x-terminal-emulator &

command -v keynav  && keynav &
