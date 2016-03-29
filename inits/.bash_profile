# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    BASHRC="$HOME/.my-bashrc"
    if [ -f "${BASHRC}" ]; then
	source "${BASHRC}"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

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
