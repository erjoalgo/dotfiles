#!/bin/bash -x
AUTOLOGIN_CONF="/etc/systemd/system/getty@tty1.service.d/autologin.conf"

if test ! -d $(dirname "${AUTOLOGIN_CONF}"); then
    sudo mkdir $(dirname "${AUTOLOGIN_CONF}")
fi

if test ! -f "${AUTOLOGIN_CONF}"; then
    cat <<EOF | sudo tee "${AUTOLOGIN_CONF}"
[Service]
ExecStart=
ExecStart=-/sbin/agetty --autologin "${USER}" %I
EOF
fi
