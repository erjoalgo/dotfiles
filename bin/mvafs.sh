#!/bin/bash -x

set -euo pipefail

THIS_CELL=$(tr -d '\n' < /etc/openafs/ThisCell)
AFS_HOME="/afs/${THIS_CELL}/home/${USER}"
AFS_DIRS_LOG="${HOME}/git/dotfiles/inits/afs-dirs"

while getopts "hd:a:b" OPT; do
    case ${OPT} in
    a)
        ALL=${OPTARG}
        ;;
    b)
        AFS_HOME="${HOME}/mnts/backup/"
        test -e "${AFS_HOME}/hello"
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

if test -z "${ALL:-}"; then
    DIRNAME=$(realpath ${1}) && shift
fi

function mvafs {
    DIRNAME=$(realpath ${1}) && shift
    test -e "${AFS_HOME}"
    touch "${AFS_HOME}/.hola"

    RELPATH=$(realpath -s --relative-to="${HOME}" "${DIRNAME}")
    if grep -F '../'  <<< "${RELPATH}"; then
        echo "path must be relative to home directory"
        return 1
    fi

    DEST="${AFS_HOME}/${RELPATH}"
    mkdir -p $(dirname "${DEST}")
    if test -e "${DIRNAME}" -a ! -L "${DIRNAME}"; then
        if test -e "${DEST}"; then
            rsync -arv --remove-source-files "${DIRNAME}" $(dirname "${DEST}")
            if ! rmdir "${DIRNAME}"; then
                echo "unable to merge all files from ${DIRNAME} into ${DEST}"
                exit ${LINENO}
            fi
        else
            mv -n "${DIRNAME}" "${DEST}"
        fi
    fi
    ln -sf "${DEST}" $(dirname "${DIRNAME}")
}

if test -n "${DIRNAME:-}"; then
    mvafs "${DIRNAME}"
    if ! grep -F "${DIRNAME}" "${AFS_DIRS_LOG}"; then
        echo "${DIRNAME}" >> "${AFS_DIRS_LOG}"
    fi
elif test -n "${ALL:-}"; then
    for DIRNAME in $(cat "${AFS_DIRS_LOG}"); do
        mvafs "${DIRNAME}"
    done
else
    echo "missing action flag" && exit ${LINENO}
fi


