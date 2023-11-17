#!/bin/bash -x
TOP=${1}
find ${TOP} -type f -printf '%s - %p\n' | sort -n 
