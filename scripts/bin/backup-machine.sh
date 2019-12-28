#!/bin/bash

set -euo pipefail

function usage  {
    echo "usage: backup-machine.sh -d OUTPUT_DIRECTORY"
}

FINALIZE=""
while getopts "d:fh" OPT; do
    case ${OPT} in
    d)
        OUTPUT_DIRECTORY=${OPTARG}
        ;;
    f)
        FINALIZE=true
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if test -z "${OUTPUT_DIRECTORY:-}"; then
    usage
    exit $LINENO
fi

mkdir -p "${OUTPUT_DIRECTORY}"
cd "${OUTPUT_DIRECTORY}"

for DIR in /var/{log,mail,www,backups} /etc /lost+found/; do
    if test -d "${DIR}"; then
        echo "backing up ${DIR}"
        OUT="system/$(tr / ! <<< ${DIR})"
        mkdir -p $(dirname "${OUT}")
        sudo rsync -ra "${DIR}" "${OUT}" || true
    fi
done

sudo apt-get install -y sysv-rc-conf
for CMD in \
    "uname -r" \
        "uname -m" \
        "lsb_release -a" \
        "sysv-rc-conf --list" \
        "mount" \
        "fdisk --list" \
        "find /" \
        "dpkg --get-selections" \
        "netstat -tulpn" \
        "service --status-all" \
    ; do
    echo "persisting output of '${CMD}'"
    OUT="system/commands/$(sed 's|[ /]|-|g' <<< ${CMD}).txt"
    mkdir -p $(dirname "${OUT}")
    test -s "${OUT}" || sudo ${CMD} > "${OUT}"
done

# service-specific backups
if command -v pg_dump > /dev/null; then
    OUT=pg_dump.sql.gz
    if ! test -s ${OUT}; then
        echo "backing up postgres db"
        sudo -upostgres pg_dumpall | gzip > ${OUT}
    fi
fi

if command -v mysqldump > /dev/null; then
    OUT=mysqldump.sql.gz
    if ! test -s ${OUT}; then
        echo "backing up mysql db"
        sudo mysqldump --all-databases | gzip > "${OUT}"
    fi
fi

# TODO back up mysql

for HOMEDIR in $(find /home -maxdepth 1 -mindepth 1 -type d) /root; do
    echo "backing up parts of ${HOMEDIR}"
    for BASENAME in  \
        .authinfo \
            .auto-save \
            .backups \
            .bash_history \
            .cache \
            .config \
            .el \
            .eld \
            .emacs.auto-save \
            .emacs-dirlocals-ro \
            .gitconfig \
            .gnupg \
            .gnus-gmail \
            .lesshst \
            .pki \
            .sbcl_history \
            .sbcl_history* \
            .slime-history.eld \
            .ssh \
            .tmp \
            .tmp \
            .vim \
            .viminfo \
            .wget-hsts \
            .xsessionrc \
            custom-backup \
        ; do
        IN="${HOMEDIR}/${BASENAME}"
        if sudo test -L "${IN}"; then
            echo "skipping ${IN}, which is a symlink"
        elif sudo test -e "${IN}"; then
            OUT="$(pwd)/home/$(basename ${HOMEDIR})/${BASENAME}"
            mkdir -p $(dirname "${OUT}")
            if ! test -e "${OUT}"; then
                # output doesn't exist
                sudo mv "${IN}" "${OUT}"
            elif ! sudo test -L "${IN}"; then
                echo "${IN} is not a symlink pointing to ${OUT}"
                if sudo test -f "${IN}"; then
                    sudo cat "${IN}" | sudo tee -a "${OUT}"
                else
                    exit $LINENO
                fi
            fi
            # make sure the symink is pointing to the current backup
            sudo ln -fs "${OUT}" "${IN}"
        fi
    done
done

if test "${FINALIZE}" = true; then
    # ensure the vacate.sh script approves before finalizing migration
    vacate.sh
    IN=$(pwd)
    OUT="${IN}.tar.gz"
    sudo tar -czf "${OUT}" "${IN}"
    md5sum "${OUT}"
    read -p"verify md5sum in backup destination: "
fi
