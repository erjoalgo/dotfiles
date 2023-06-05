#!/bin/bash -x

set -euo pipefail


sudo apt install -y docker.io
sudo setfacl --modify user:$(whoami):rw /var/run/docker.sock
