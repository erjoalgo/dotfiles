#!/bin/bash -x

set -euo pipefail

while getopts "h:" OPT; do
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

CLJ=${1} && shift

REPO=${HOME}/git/scad-clj-cli

function jar-path {
    echo ${REPO}/target/*-standalone.jar
}
JAR=$(jar-path)

if ! test -d "${REPO}"; then
    git clone https://github.com/leshy/scad-clj-cli "${REPO}"
fi

if ! test -e "${JAR}"; then
    cd "${REPO}"
    lein uberjar
    JAR=$(jar-path)
    test -e "${JAR}"
fi


BASE="${CLJ%.*}"
SCAD="${BASE}.scad"
STL="${BASE}.stl"

java -jar "${JAR}" "${CLJ}" > "${SCAD}"

openscad.sh -o "${STL}" "${SCAD}"
