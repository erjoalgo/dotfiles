#!/bin/bash -x

set -euo pipefail

GOROOT=/usr/local/go
GO_URL=https://go.dev/dl/go1.21.0.linux-amd64.tar.gz

if ! command -v go || test ${UPGRADE_GO:-} = true; then
    mkdir -p ~/src && cd ~/src
    if test -f $(basename ${GO_URL}); then
        rm $(basename ${GO_URL})
    fi
    wget "${GO_URL}"
    FNAME=$(basename ${GO_URL})
    DNAME=$(basename ${FNAME} .tar.gz)

    if test ${UPGRADE_GO:-} = true && command -v go && test -d ${GOROOT}; then
        VERSION=$(go version | cut -f3 -d' ')
        export GOROOT_BOOTSTRAP=${GOROOT}-${VERSION}
        sudo mv ${GOROOT} ${GOROOT_BOOTSTRAP}
    fi

    test -d ${GOROOT} || sudo tar -C $(dirname ${GOROOT}) -xzf ${FNAME}

    test -d ${GOROOT}

    PROFILE_FILE=/etc/profile.d/go-env.sh
    which insert-text-block
    sudo insert-text-block \
	 '# c0b15b6c-e5fc-495a-b1be-f02308cee38d-add-go-tools-to-path'  \
	 ${PROFILE_FILE} <<EOF
export PATH=\${PATH}:${GOROOT}/bin
EOF
    source ${PROFILE_FILE}
    if test -e ${GOROOT}/src/all.bash; then
        pushd .
        cd ${GOROOT}/src
        ./all.bash
        popd
    fi
    go version
fi

# add fallback-gopath if user's GOPATH not set

GOPATH=/usr/share/gopath
sudo $(which insert-text-block) \
     '# 387b6046-a715-11e7-b87e-2be468b96d0a-set-default-gopath'  \
     /etc/bash.bashrc <<EOF
export GOPATH=$GOPATH
export PATH=\$PATH:\$GOPATH/bin
EOF

GOPATH=${HOME}/src/go
mkdir -p ${GOPATH}
insert-text-block \
    '# a7095f2e-b531-4f1e-9fd6-ea4e404f19f2-add-user-gopath'  \
    ~/.profile-env <<EOF
export GOPATH=$GOPATH
export PATH=\$PATH:\$GOPATH/bin:${GOROOT}/bin
EOF

sudo $(which insert-text-block)  \
     '# fba1e4c6-a726-11e7-b4e2-23bbc233d273-set-default-gopath'  \
     /etc/environment <<< "GOPATH=$GOPATH"
