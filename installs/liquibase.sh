#!/bin/bash -x

set -euo pipefail

while getopts "ha:l" OPT; do
    case ${OPT} in
    l)
        LIQUIBASE_TAR=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

test -z "${LIQUIBASE_TAR:-}"

mkdir -p ~/src/liquibase
cd ~/src/liquibase
if ! test -e "README.md"; then
    tar -C . -avxf "${LIQUIBASE_TAR}"
fi

sudo apt-get install default-jre

sudo ln -sf $(pwd)/liquibase /usr/bin/


