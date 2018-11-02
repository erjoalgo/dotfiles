#!/bin/bash -x

curl -s checkip.dyndns.org \
    | grep -oi "current ip address: [^<]*" \
    | grep -o "[0-9.]*"
