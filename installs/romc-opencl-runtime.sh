#!/bin/bash -x

set -euo pipefail

# WIP WIP

cd ${HOME}/git

git clone https://github.com/ROCm-Developer-Tools/ROCclr.git || true
git clone -b master-next https://github.com/RadeonOpenCompute/ROCm-OpenCL-Runtime.git || true

# Set the environment variables
export ROCclr_DIR="$(readlink -f ROCclr)"
export OPENCL_DIR="$(readlink -f ROCm-OpenCL-Runtime)"

cd "$ROCclr_DIR"
mkdir -p build;
cd build
pwd
cmake -DOPENCL_DIR="$OPENCL_DIR" -DCMAKE_INSTALL_PREFIX=/opt/rocm/rocclr ..
make -j$(nproc)

# if ! test -d /etc/OpenCL/vendors/; then
#     git clone -b master-next https://github.com/RadeonOpenCompute/ROCm-OpenCL-Runtime.git
#     sudo cp api/opencl/config/amdocl64.icd /etc/OpenCL/vendors/
# fi


cd "$OPENCL_DIR"
mkdir -p build; cd build
cmake -DUSE_COMGR_LIBRARY=ON -DCMAKE_PREFIX_PATH="$ROCclr_DIR/build;/opt/rocm/" ..
make -j$(nproc)
# Note: For release build, add "-DCMAKE_BUILD_TYPE=Release" to the cmake command line.
