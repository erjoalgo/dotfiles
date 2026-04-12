#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y blender cura

pip install trimesh

# creality slicer
# https://file2-cdn.creality.com/file/1946156fb58ee2564ae71424e44c05a3/Creality_Print-v3.11.1-Ubutu-x86_64-Release.AppImage?spm=..page_1995737.download_support_two_1.1

pip install git+https://github.com/erjoalgo/pystl
