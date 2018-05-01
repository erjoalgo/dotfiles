#!/bin/bash -x

set -euo pipefail
QL_SETUP="${HOME}/quicklisp/setup.lisp"
if ! test -f "${QL_SETUP}"; then
    cd /tmp
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
    # gpg --verify quicklisp.lisp.asc quicklisp.lisp
    sbcl --load quicklisp.lisp --non-interactive \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql:add-to-init-file)'
    rm quicklisp.lisp{,.asc}
fi
