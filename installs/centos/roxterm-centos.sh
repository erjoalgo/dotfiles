#!/bin/bash -x

set -euo pipefail

if ! test -f /etc/yum.repos.d/gf.repo; then
    cd /tmp
    wget http://mirror.ghettoforge.org/distributions/gf/gf-release-latest.gf.el7.noarch.rpm
    sudo rpm -Uvh gf-release*rpm
fi

sudo yum --enablerepo=gf-plus install -y roxterm

