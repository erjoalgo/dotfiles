#!/bin/bash -x

set -euo pipefail

if ! grep '^docker' /etc/group; then
    sudo groupadd docker
fi
sudo usermod -aG docker ${USER}

sudo apt-get update

sudo apt-get install -y \
     ca-certificates \
     curl \
     gnupg \
     lsb-release

sudo mkdir -p /etc/apt/keyrings

curl -fsSL https://download.docker.com/linux/debian/gpg |  \
    sudo gpg --dearmor --yes -o /etc/apt/keyrings/docker.gpg

sudo insert-text-block \
     '# 5f356d7c-7dfc-4d6b-a113-bae0a11f3622-add-docker-repo' \
     /etc/apt/sources.list.d/docker.list <<EOF
deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/debian $(lsb_release -cs) stable
EOF


sudo apt-get update

sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-compose-plugin

sudo -u ${USER} docker run hello-world
