#!/bin/bash

curl -Ls ipecho.erjoalgo.com --fail-with-body ||  \
    curl -Ls checkip.dyndns.org \
        | grep -oi "current ip address: [^<]*" \
        | grep -o "[0-9.]*"
