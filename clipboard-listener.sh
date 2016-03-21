#!/bin/bash

#listen for incoming text on a given port
#and paste it to the clipboard
#use to transfer snippets/urls between machines
PORT="${1:-1234}"
echo "listening on port ${PORT}"
while true; do
    MESSAGE=$(nc -lp "${PORT}")
    
    echo "got  "${MESSAGE}""
    
    # echo "${MESSAGE}" | xsel -ib;
    
    echo "${MESSAGE}" | xsel -ip;
    # echo "${MESSAGE}" | xsel -ib;
    # echo "${MESSAGE}" | xsel -is;
    echo "last clipboard:  $(xsel -o)"
done
