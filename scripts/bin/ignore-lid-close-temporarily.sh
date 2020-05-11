#!/bin/bash -x

TIMESPEC=${*}

ignore-lid-close.sh ignore

at ${TIMESPEC} <<EOF
ignore-lid-close.sh respect
EOF
