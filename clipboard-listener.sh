#!/bin/bash

#listen for incoming text on a given port
#and paste it to the clipboard
#use to transfer snippets/urls between machines
PORT="${1:-1234}"
echo "listening on port ${PORT}"
while true; do
    nc -lp "${PORT}" | xsel -ib;
    echo "got  $(xsel -o)"
done
