#!/bin/bash

curl -s ipecho.erjoalgo.com --fail-with-body ||  \
    curl checkip.dyndns.org \
        | grep -oi "current ip address: [^<]*" \
        | grep -o "[0-9.]*"
