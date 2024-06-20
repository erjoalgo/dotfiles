#!/bin/bash -x

set -euo pipefail

AFS_DIRS_FILE="${HOME}/git/dotfiles/inits/afs-dirs"

while getopts "hd:a:" OPT; do
    case ${OPT} in
    d)
        # directory or filename
        DIRNAME=$(realpath ${OPTARG})
        ;;
    a)
        ALL=${OPTARG}
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

function mvafs {
    DIRNAME=$(realpath ${1}) && shift
    THIS_CELL=$(tr -d '\n' < /etc/openafs/ThisCell)
    AFS_HOME="/afs/${THIS_CELL}/home/${USER}"
    test -e "${AFS_HOME}"
    touch "${AFS_HOME}/.hola"

    RELPATH=$(realpath -s --relative-to="${HOME}" "${DIRNAME}")
    if grep -F '../'  <<< "${RELPATH}"; then
        echo "path must be relative to home directory"
        return 1
    fi

    DEST="${AFS_HOME}/${RELPATH}"
    mkdir -p "${DEST}"
    if test -e "${DIRNAME}" -a ! -L "${DIRNAME}"; then
        if test -e "${DEST}"; then
            rsync -arv --remove-source-files "${DIRNAME}" "${DEST}"
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
    if ! grep -F "${DIRNAME}" "${AFS_DIRS_FILE}"; then
        echo "${DIRNAME}" >> "${AFS_DIRS_FILE}"
    fi
elif test -n "${ALL:-}"; then
    for DIRNAME in $(cat "${AFS_DIRS_FILE}"); do
        mvafs "${DIRNAME}"
    done
else
    echo "missing action flag" && exit ${LINENO}
fi


