#remember original settings
[ -e /tmp/xmodmap.pke ] || xmodmap -pke > /tmp/xmodmap.pke

#TODO fix to avoid loading twice
xmodmap ${HOME}/.xmodmap/$(hostname) &>> /tmp/xmodmap-errs
echo ================  >> /tmp/xmodmap-errs
xmodmap ${HOME}/.xmodmap/$(hostname) &>> /tmp/xmodmap-errs

${HOME}/.my-startups.sh &
exec ${HOME}/programs/stumpwm/stumpwm
