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

    export PATH=${PATH}:${INSTALL_DIR}/go/bin
    go version
fi

for REPO in\
    github.com/github/hub \
    golang.org/x/tools/cmd/goimports \
    github.com/golang/lint \
    ; do
    go get ${REPO}
done
