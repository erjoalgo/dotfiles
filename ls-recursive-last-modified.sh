#!/bin/bash

# https://superuser.com/questions/416308/how-to-list-files-recursively-and-sort-them-by-modification-time
find . -type f -exec stat --printf "%Y\t%y %N\n" '{}' \;  \
    | sort -n  | cut -f2-
