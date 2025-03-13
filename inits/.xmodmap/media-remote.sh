#!/bin/bash -x

# based on:
# https://superuser.com/questions/760602/how-to-remap-keys-under-linux-for-a-specific-keyboard-only

set -euo pipefail

export DISPLAY=${DISPLAY:-:$(ls /tmp/.X11-unix | tr -d 'X')}
export XAUTHORITY=${XAUTHORITY:-${HOME}/.Xauthority}

REMOTE_ID=$(xinput list | grep -P '2[.]4G Mouse[ \t]*id=.*keyboard' | grep -Po '(?<=id=)[0-9]+')

REMOTE_NAME="media_remote"
# SYMBOLS_FILE=/tmp/xkb/symbols/custom
SYMBOLS_FILE=/usr/share/X11/xkb/symbols/custom
[ "$REMOTE_ID" ] || exit

# remap the following keys, only for my custom vintage atari joystick connected
# through an old USB keyboard:
#
# keypad 5 -> keypad 6
# . -> keypad 2
# [ -> keypad 8
# left shift -> left control

mkdir -p /tmp/xkb/symbols
# This is a name for the file, it could be anything you
# want. For us, we'll name it "custom". This is important
# later.
#
# The KP_* come from /usr/include/X11/keysymdef.h
# Also note the name, "remote" is there in the stanza
# definition.

# insert-text-block '# 229c021f-6b95-473b-b824-c9c50365682d-media-remote-mappings'
sudo tee "${SYMBOLS_FILE}"  <<EOF
xkb_symbols "${REMOTE_NAME}" {
    // symbols on the left come from looking up keycodes in https://gist.github.com/zoqaeski/3880640
    // symbols on the right are based on /usr/include/X11/keysymdef.h
    // the bottom key sometimes returns any of these codes. make them all space
    key <ESC>  { [ space ] };
    key <LFSH>  { [ space ] };
    key <FK05>  { [ space ] };

    // left, right for scrolling a video
    key <PGUP> { [ KP_Left ]  };
    key <PGDN> { [ KP_Right ]  };

    // the top key maps to "b", map it to "f" for full-screen
    key <AB05> { [ f ]  };

    // volume up/down
    key <MUTE> { [ XF86AudioRaiseVolume ]  };
    key <TAB> { [ XF86AudioLowerVolume ]  };
};
EOF

# (1) We list our current definition
# (2) Modify it to have a keyboard mapping using the name
#     we used above, in this case it's the "remote" definition
#     described in the file named "custom" which we specify in
#     this world as "custom(remote)".
# (3) Now we take that as input back into our definition of the
#     keyboard. This includes the file we just made, read in last,
#     so as to override any prior definitions.  Importantly we
#     need to include the directory of the place we placed the file
#     to be considered when reading things in.
#
# Also notice that we aren't including exactly the
# directory we specified above. In this case, it will be looking
# for a directory structure similar to /usr/share/X11/xkb
#
# What we provided was a "symbols" file. That's why above we put
# the file into a "symbols" directory, which is not being included
# below.
ORIGINAL=/tmp/xkbmap-original-${REMOTE_ID}
if ! test -e "${ORIGINAL}"; then
   setxkbmap -device $REMOTE_ID -print > "${ORIGINAL}"
fi

cat "${ORIGINAL}"
NEW=/tmp/xkbmap-new-${REMOTE_ID}
SYMBOLS_SPEC="$(basename ${SYMBOLS_FILE})(${REMOTE_NAME})"
setxkbmap -device $REMOTE_ID -print \
 | sed "s/\(xkb_symbols.*\)\"/\1+${SYMBOLS_SPEC}\"/"  \
       > "${NEW}"

cat "${NEW}"

diff "${ORIGINAL}" "${NEW}" || true

xkbcomp -I$(dirname "${SYMBOLS_FILE}") -i $REMOTE_ID -synch - $DISPLAY < "${NEW}"
