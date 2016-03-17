${HOME}/repos/stumpwm/xmodmap/xmodmap_init.sh

xmodmap ${HOME}/repos/stumpwm/xmodmap/per-keyboard/$(hostname)
xmodmap ${HOME}/repos/stumpwm/xmodmap/per-keyboard/$(hostname)

emacs &
x-terminal-emulator &
firefox &

${HOME}/.my_startups.sh &
${HOME}/programs/stumpwm/stumpwm
