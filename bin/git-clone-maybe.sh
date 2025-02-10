#!/bin/bash -x

set -euo pipefail

while getopts "h" OPT; do
    case ${OPT} in
    h)
        less "$0"
        exit 0
        ;;
    *)
        echo "unrecognized flag: ${OPT}" && exit ${LINENO}
        ;;
    esac
done
shift $((OPTIND -1))

URL=${1} && shift

BASE=$(basename "${URL}")
DIR="${HOME}/git/${BASE}"

if ! test -d "${DIR}"; then
    git clone "${URL}" "${DIR}" 1>&2
fi

echo "${DIR}"

# git pull --ff-only
