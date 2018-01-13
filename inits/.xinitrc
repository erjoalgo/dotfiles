#remember original settings
[ -e /tmp/xmodmap.pke ] || xmodmap -pke > /tmp/xmodmap.pke
xmodmap /tmp/xmodmap.pke #prevents corruption when repeated xmodmap $(hostaname). TODO fix

XMODMAP_FILE="${HOME}/.xmodmap/$(hostname)"

if ! test -f "${XMODMAP_FILE}"; then
    XMODMAP_FILE="${HOME}/.xmodmap/default"
fi

#TODO fix need to load twice
xmodmap "${XMODMAP_FILE}"
xmodmap "${XMODMAP_FILE}"

SCRIPTSD=${HOME}/.stumpwmrc.d/scripts
for SCRIPT in $(find "${SCRIPTSD}/on-startup" -type f); do
    ${SCRIPT}
done

/usr/libexec/notification-daemon &

exec stumpwm
