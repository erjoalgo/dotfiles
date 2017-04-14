#remember original settings
[ -e /tmp/xmodmap.pke ] || xmodmap -pke > /tmp/xmodmap.pke
xmodmap /tmp/xmodmap.pke #prevents corruption when repeated xmodmap $(hostaname). TODO fix

XMODMAP_FILE="${HOME}/.xmodmap/$(hostname)"
if ! test -f XMODMAP_BASE; then
    XMODMAP_FILE="${HOME}/.xmodmap/default"
fi

#TODO fix need to load twice
xmodmap "${XMODMAP_FILE}"
xmodmap "${XMODMAP_FILE}"

${HOME}/.my-startups.sh &
exec ${HOME}/bin/stumpwm
