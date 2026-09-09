#!/bin/bash -x
#
# install_go.sh - Install the latest stable version of Go on Debian (amd64/arm64)
#
set -euo pipefail

INSTALL_DIR="/usr/local"
GO_DIR="${INSTALL_DIR}/go"
PROFILE_SNIPPET="/etc/profile.d/go.sh"

# --- Detect architecture ---
ARCH_RAW="$(uname -m)"
case "${ARCH_RAW}" in
    x86_64)
        GOARCH="amd64"
        ;;
    aarch64|arm64)
        GOARCH="arm64"
        ;;
    armv6l|armv7l)
        GOARCH="armv6l"
        ;;
    i386|i686)
        GOARCH="386"
        ;;
    *)
        echo "Unsupported architecture: ${ARCH_RAW}" >&2
        exit 1
        ;;
esac

# --- Ensure prerequisites ---
sudo apt-get update
sudo apt-get install -y curl ca-certificates tar

# --- Determine latest stable version from Go's own version endpoint ---
echo "Checking for the latest stable Go release..."
LATEST_VERSION="$(curl -fsSL https://go.dev/VERSION?m=text | head -n1)"

if [[ -z "${LATEST_VERSION}" ]]; then
    echo "Could not determine the latest Go version." >&2
    exit 1
fi

if command -v go && go version | grep "${LATEST_VERSION}"; then
    echo "go is already at the latest version"
    exit 0
fi

TARBALL="${LATEST_VERSION}.linux-${GOARCH}.tar.gz"
DOWNLOAD_URL="https://go.dev/dl/${TARBALL}"
TMP_DIR="/tmp/go-tarball"
DEST="${TMP_DIR}/${TARBALL}"

echo "Latest version: ${LATEST_VERSION} (${GOARCH})"

if ! test -e "${DEST}"; then
    echo "Downloading ${DOWNLOAD_URL} ..."
    trap 'sudo rm -rf "${TMP_DIR}"' EXIT

    mkdir $(dirname "${DEST}")
    curl -fsSL -o "${DEST}" "${DOWNLOAD_URL}"

    # --- Remove any previous Go installation and extract the new one ---
    if [[ -d "${GO_DIR}" ]]; then
        echo "Removing existing Go installation at ${GO_DIR} ..."
        sudo rm -rf "${GO_DIR}"
    fi
fi

echo "Extracting to ${INSTALL_DIR} ..."
sudo tar -C "${INSTALL_DIR}" -xzf "${DEST}"

for PROFILE in "${PROFILE_SNIPPET}" ${HOME}/.profile-env; do
    sudo insert-text-block \
         '# e24ca404-206d-44ce-a4f7-5dc198d08e48-go-path'  \
         "${PROFILE}" <<'EOF'
export PATH=$PATH:/usr/local/go/bin
EOF
done

sudo chmod 644 "${PROFILE_SNIPPET}"

echo "Go installed successfully."
source "${PROFILE_SNIPPET}"
go version

echo
echo "Open a new shell, or run 'source /etc/profile.d/go.sh', to pick up the updated PATH."
