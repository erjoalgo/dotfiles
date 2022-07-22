#!/bin/bash

set -euo pipefail

grep -Po '(?<=^domain=|^reg_proxy=<sip:)[a-z0-9.]+' < ${HOME}/.linphonerc* |  \
    head -1
