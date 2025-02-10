#!/bin/bash

set -euo pipefail

# from https://stackoverflow.com/a/7448828/1941755

find $1 -type f -exec stat --format '%Y :%y %n' "{}" \; |  \
    sort -nr |  \
    cut -d: -f2- |  \
    head -1 |  \
    cut -f4- -d' '
