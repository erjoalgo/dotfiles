#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y wine
sudo dpkg --add-architecture i386 
sudo apt-get update
sudo apt-get install -y wine32:i386
