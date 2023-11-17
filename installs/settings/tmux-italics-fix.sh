#!/bin/bash

# http://stackoverflow.com/questions/10535432/tmux-man-page-search-highlighting
# based on Christy's answer
# might not work if screen-256color doesn't exist

mkdir $HOME/.terminfo

screen_terminfo="screen-256color"

# infocmp "$screen_terminfo" | sed \
infocmp | sed \
  -e 's/^screen[^|]*|[^,]*,/screen-256color|screen with italics support,/' \
  -e 's/%?%p1%t;3%/%?%p1%t;7%/' \
  -e 's/smso=[^,]*,/smso=\\E[7m,/' \
  -e 's/rmso=[^,]*,/rmso=\\E[27m,/' \
  -e '$s/$/ sitm=\\E[3m, ritm=\\E[23m,/' > /tmp/screen.terminfo

tic /tmp/screen.terminfo

echo 'set -g default-terminal "screen-256color"' > $HOME/.tmux.conf
