#!/bin/bash -x

set -euo pipefail

# default URL
URL="https://dl.google.com/dl/android/studio"
URL+="/ide-zips/3.4.1.0/android-studio-ide-183.5522156-linux.tar.gz"

while getopts "u:z:d:rh" OPT; do
    case ${OPT} in
        u)
            URL=${OPTARG}
            ;;
        z)
            ARCHIVE_FILE=${OPTARG}
            ;;
        d)
            EXTRACTED_DIRECTORY=${OPTARG}
            ;;
        r)
            REPLACE=true
            ;;
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


function fetch-archive-file {
    if ! test -e "${ARCHIVE_FILE:-}" 1>&2; then
        ARCHIVE_FILE=${HOME}/Downloads/$(basename "${URL}")
        curl "${URL}" -o "${ARCHIVE_FILE}" 1>&2
    else
        echo "WARN: skipping archive download"
    fi
    echo "${ARCHIVE_FILE}"
}

function extract-directory {
    DEST=${1} && shift
    if test "${REPLACE:-}" = true; then
        if test -d "${DEST}"; then
            echo "WARN: removing old extracted directory"
        fi
        rm -rf "${DEST}"
    fi
    if ! test -d "${DEST:-}"; then
        ARCHIVE_FILE=$(fetch-archive-file)
        test -e "${ARCHIVE_FILE}"
        DEST=${DEST:-${HOME}/Downloads/$(basename "${ARCHIVE_FILE}")}
        tar -C "$(dirname ${SRC})" -axvf "${DEST}"
    fi
}

SRC=${HOME}/src/android-studio
if test -d "${SRC}" -a "${REPLACE:-}" != true; then
    echo "WARN: skipping replace of ${SRC}"
else
    rm -rf "${SRC}"
    if test -d "${EXTRACTED_DIRECTORY:-}"; then
        mv "${EXTRACTED_DIRECTORY}/android-studio" "${SRC}"
    else
        extract-directory "${SRC}"
    fi
fi

cd "${SRC}"

BIN_D="${SRC}/bin"
test -d "${BIN_D}"

PROFILE_ENV=${PROFILE_ENV:-${HOME}/.profile-env}

insert-text-block '# ea5c84ba-cd6c-4e0e-9bd5-da72a6bd845e-android-studio-bin-dir'  \
                  ${PROFILE_ENV} << EOF
export PATH=\$PATH:${BIN_D}
# may not exist until after running studio.sh
export ANDROID_HOME=${HOME}/Android/Sdk/
EOF

echo "in a new terminal, run studio.sh"
