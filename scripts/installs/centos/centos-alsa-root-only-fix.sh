#!/bin/bash -x


sudo $(which insert-text-block) "# 23fdc4cd-826f-481a-a4fd-03b9a12054a7-centos-alsa-root-only-fix" \
     /etc/rc.d/rc.local <<EOF
chmod -R a+rw /dev/snd/*
EOF
