#!/bin/bash -x

set -euo pipefail

if ! command -v go; then
    GO_URL=https://storage.googleapis.com/golang/go1.7.5.linux-amd64.tar.gz
    mkdir -p ~/src && cd ~/src
    test -f $(basename ${GO_URL}) || wget "${GO_URL}"
    FNAME=$(basename ${GO_URL})
    INSTALL_DIR=/usr/local
    DNAME=$(basename ${FNAME} .tar.gz)

    test -d ${INSTALL_DIR}/go ||  \
	sudo tar -C ${INSTALL_DIR} -xzf ${FNAME}

    test -d ${INSTALL_DIR}/go

    PROFILE_FILE=/etc/profile.d/go-env.sh
    which insert-text-block
    sudo $(which insert-text-block) \
	 '# c0b15b6c-e5fc-495a-b1be-f02308cee38d-add-go-tools-to-path'  \
		      ${PROFILE_FILE} <<EOF
    export PATH=\${PATH}:${INSTALL_DIR}/go/bin
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

sudo $(which insert-text-block) '# fba1e4c6-a726-11e7-b4e2-23bbc233d273-set-default-gopath'  \
		  /etc/environment <<< "GOPATH=$GOPATH"

for REPO in\
    golang.org/x/tools/cmd/goimports \
    github.com/golang/lint \
    ; do
    go get ${REPO}
done
