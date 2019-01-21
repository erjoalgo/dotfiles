#!/bin/bash -x

set -euo pipefail

for REPO in\
    golang.org/x/tools/cmd/goimports \
    github.com/golang/lint \
    ; do
    ${GOEXE} get ${REPO}
done
