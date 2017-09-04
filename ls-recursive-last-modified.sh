#!/bin/bash

find . -type f -exec stat --printf "%Y\t%y %N\n" '{}' \;  \
    | sort -rn  | cut -f2-
