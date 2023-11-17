#!/bin/bash -x

set -euo pipefail

sudo wget -O /etc/yum.repos.d/cloudfoundry-cli.repo \
     https://packages.cloudfoundry.org/fedora/cloudfoundry-cli.repo

sudo yum install cf-cli
