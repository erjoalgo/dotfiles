${HOME}/repos/stumpwm/xmodmap/xmodmap_init.sh

#remember original settings
[ -e /tmp/xmodmap.pke ] || xmodmap -pke > /tmp/xmodmap.pke

#TODO fix to avoid loading twice
xmodmap ${HOME}/repos/stumpwm/xmodmap/$(hostname)
xmodmap ${HOME}/repos/stumpwm/xmodmap/$(hostname)

${HOME}/.my_startups.sh &
exec ${HOME}/programs/stumpwm/stumpwm
