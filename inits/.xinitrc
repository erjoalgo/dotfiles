${HOME}/repos/stumpwm/xmodmap/xmodmap_init.sh

#TODO fix to avoid loading twice
xmodmap ${HOME}/repos/stumpwm/xmodmap/per-keyboard/$(hostname)
xmodmap ${HOME}/repos/stumpwm/xmodmap/per-keyboard/$(hostname)

${HOME}/.my_startups.sh &
exec ${HOME}/programs/stumpwm/stumpwm
