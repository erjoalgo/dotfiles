#!/bin/bash -x

set -euo pipefail

GOROOT=/usr/local/go
if ! command -v go || test ${UPGRADE_GO} = true; then
    # GO_URL=https://storage.googleapis.com/golang/go1.7.5.linux-amd64.tar.gz
    GO_URL=https://dl.google.com/go/go1.13.5.linux-amd64.tar.gz
    mkdir -p ~/src && cd ~/src
    test -f $(basename ${GO_URL}) || wget "${GO_URL}"
    FNAME=$(basename ${GO_URL})
    DNAME=$(basename ${FNAME} .tar.gz)

    test -d ${GOROOT} ||  \
	sudo tar -C $(dirname ${GOROOT}) -xzf ${FNAME}

    test -d ${GOROOT}

    PROFILE_FILE=/etc/profile.d/go-env.sh
    which insert-text-block
    sudo insert-text-block \
	 '# c0b15b6c-e5fc-495a-b1be-f02308cee38d-add-go-tools-to-path'  \
		      ${PROFILE_FILE} <<EOF
export PATH=\${PATH}:${GOROOT}/bin
EOF
    source ${PROFILE_FILE}
    go version
fi

# add fallback-gopath if user's GOPATH not set

GOPATH=/usr/share/gopath
sudo $(which insert-text-block) '# 387b6046-a715-11e7-b87e-2be468b96d0a-set-default-gopath'  \
		  /etc/bash.bashrc <<EOF
export GOPATH=$GOPATH
export PATH=\$PATH:\$GOPATH/bin
EOF

GOPATH=${HOME}/src/go
mkdir -p ${GOPATH}
insert-text-block '# a7095f2e-b531-4f1e-9fd6-ea4e404f19f2-add-user-gopath'  \
		  ~/.profile-env <<EOF
export GOPATH=$GOPATH
export PATH=\$PATH:\$GOPATH/bin:${GOROOT}/bin
EOF

sudo $(which insert-text-block) '# fba1e4c6-a726-11e7-b4e2-23bbc233d273-set-default-gopath'  \
		  /etc/environment <<< "GOPATH=$GOPATH"
