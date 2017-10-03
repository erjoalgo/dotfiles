#!/bin/bash -x


if ! command -v go; then
    GO_URL=https://storage.googleapis.com/golang/go1.7.5.linux-amd64.tar.gz
    cd ~/Downloads || exit ${LINENO}
    if ! test -f $(basename ${GO_URL}); then
	wget "${GO_URL}" || exit ${LINENO}
    fi
    FNAME=$(basename ${GO_URL})
    INSTALL_DIR=/usr/local
    DNAME=$(basename ${FNAME} .tar.gz)

    test -d ${INSTALL_DIR}/go ||  \
	sudo tar -C ${INSTALL_DIR} -xzf ${FNAME} || exit ${LINENO}

    test -d ${INSTALL_DIR}/go || exit ${LINENO}

    export PATH=${PATH}:${INSTALL_DIR}/go/bin
    go version || exit ${LINENO}
fi

for REPO in\
    github.com/github/hub \
    golang.org/x/tools/cmd/goimports \
    github.com/golang/lint \
    ; do
    go get ${REPO}
done
