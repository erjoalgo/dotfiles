#!/bin/bash -x

set -euo pipefail

sudo apt install -y hipcc cmake libamdhip64-dev

# autodetects your GPU, but can be manually specified with -DAMDGPU_TARGETS=<list-of-gfxids>
CXX=hipcc cmake -S. -Bbuild
make -C build
./build/ex

# Bookworm uses clang++-15
# Trixie uses clang++-17
# autodetects your GPU, but can be manually specified with -DCMAKE_HIP_ARCHITECTURES=<list-of-gfxids>
HIPCXX=clang++-15 cmake -S. -Bbuild
make -C build
./build/ex
