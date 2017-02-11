#!/bin/bash -x

if ! command -v go; then
    echo "go isn't installed" && exit "${LINENO}"
fi

for REPO in\
    github.com/github/hub\
	; do
    go get ${REPO}
done
