#remember original settings
[ -e /tmp/xmodmap.pke ] || xmodmap -pke > /tmp/xmodmap.pke
xmodmap /tmp/xmodmap.pke #prevents corruption when repeated xmodmap $(hostaname). TODO fix

#TODO fix need to load twice
xmodmap ${HOME}/.xmodmap/$(hostname) 
xmodmap ${HOME}/.xmodmap/$(hostname) 

${HOME}/.my-startups.sh &
exec ${HOME}/programs/stumpwm/stumpwm
