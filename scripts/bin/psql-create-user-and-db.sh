#!/bin/bash -x

set -euo pipefail

while getopts "d:u:t:Dhb" OPT; do
    case ${OPT} in
    d)
        DBNAME=${OPTARG}
        ;;
    u)
        USER=${OPTARG}
        ;;
    t)
        HOST=${OPTARG}
        ;;
    D)
        DROP=true
        ;;
    b)
        BATCH=true
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done

HOST=${HOST:-localhost}

test -n "${DBNAME}" -a -n "${USER}"

if grep '-' <<< "${DBNAME}"; then
    echo "dbname cannot have hyphens" && exit ${LINENO}
fi

DROP=${DROP:-}

if test -n "${DROP}"; then
    # todo not implemented
    exit 1
else
    if test -z "${BATCH:-}"; then
        read -p "enter psql password for ${USER}: " -s PASS
    else
        PASS=$(cat)
    fi

    sudo -u postgres psql <<EOF
CREATE USER ${USER} WITH PASSWORD '${PASS}';
CREATE DATABASE ${DBNAME} OWNER ${USER};

GRANT ALL PRIVILEGES ON DATABASE "${DBNAME}" to ${USER};

-- ALTER USER ${USER} CREATEDB;
EOF

fi
