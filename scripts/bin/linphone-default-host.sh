#!/bin/bash

set -euo pipefail

grep -Po '(?<=^domain=)[a-z0-9.]+' < ${HOME}/.linphonerc |  \
    head -1
