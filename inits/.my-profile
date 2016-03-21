TZ='America/Mexico_City'; export TZ
TZ='America/New_York'; export TZ
TZ='America/Los_Angeles'; export TZ

if [ -z "$DISPLAY" ] ; then
    tty=$(tty)
    if [ ${tty} = /dev/tty1 ] ; then
	xinit
    fi
fi


# Local Variables:
# mode: shell-script
# End:
