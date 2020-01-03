#!/bin/bash -x

set -euo pipefail

DIR=${HOME}/git/ekiga

test -d ${DIR} || git clone https://gitlab.gnome.org/GNOME/ekiga ${DIR}
cd ${DIR}

sudo apt-get install -y libtool intltool libglib2.0-dev \
     gtk+-3.0 gnome-icon-theme clutter-gtk-1.0  \
     gstreamer1.0-plugins-base libboost-signals-dev \
     libxml2-dev libgudev-1.0-dev libebook1.2-dev \
     libldap2-dev libsasl2-dev libresolv-wrapper libdbus-glib-1-dev \
     libavahi-client-dev libavahi-glib-dev gnome-doc-utils \
     # libpt-dev

# ./autogen.sh || true
# export GSTREAMER_PLUGINS_BASE_LIBS=/usr/lib/x86_64-linux-gnu/gstreamer-1.0
# from https://clang.debian.net/logs/2013-01-28/thoggen_0.7.1-1_unstable_clang.log
export GSTREAMER_PLUGINS_BASE_LIBS="-pthread -lgstreamer-0.10 -lgobject-2.0 -lgmodule-2.0 -lgthread-2.0 -lrt -lxml2 -lglib-2.0"
export GSTREAMER_PLUGINS_BASE_CFLAGS="-pthread -I/usr/include/gstreamer-0.10 -I/usr/include/glib-2.0 -I/usr/lib/x86_64-linux-gnu/glib-2.0/include -I/usr/include/libxml2"
./configure

make
sudo make install
