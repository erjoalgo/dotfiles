#!/bin/bash -x

set -euo pipefail

if command -v gnutls; then
    echo "gnutls already installed at $(which gnutls)"
    gnutls -h
    exit 0
fi

install-from-source  \
    -u https://ftp.gnu.org/gnu/nettle/nettle-3.4.tar.gz \
    -g 28C67298

install-from-source  \
    -u https://ftp.gnu.org/gnu/libtasn1/libtasn1-4.13.tar.gz \
    -g 9013B842

install-from-source  \
    -u https://github.com/p11-glue/p11-kit/releases/download/0.23.14/p11-kit-0.23.14.tar.gz \
    -g D7E69871

install-from-source  \
    -u https://www.gnupg.org/ftp/gcrypt/gnutls/v3.5/gnutls-3.5.19.tar.xz
