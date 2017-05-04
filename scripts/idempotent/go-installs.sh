#!/bin/bash -x

if ! command -v go; then
    echo "go isn't installed" && exit "${LINENO}"
fi

for REPO in\
    github.com/github/hub \
    golang.org/x/tools/cmd/goimports \
    github.com/golang/lint \
    ; do
    go get ${REPO}
done
