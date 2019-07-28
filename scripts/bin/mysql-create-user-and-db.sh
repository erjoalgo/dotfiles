#!/bin/bash -x

set -euo pipefail

while getopts "d:u:h" OPT; do
    case ${OPT} in
    d)
        DBNAME=${OPTARG}
        ;;
    u)
	DBUSER=${OPTARG}
	;;
    t)
	HOST=${OPTARG}
	;;
    p)
	DROP=true
	;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

DBUSER=${DBUSER:-${DBNAME:-}}
HOST=${HOST:-localhost}
DROP=${DROP:-}


if ! test -n "${DBNAME:-}" -a -n "${DBUSER:-}" -a -n "${HOST:}"; then
    echo "missing arguments"
    exit ${LINENO}
fi

if grep '-' <<< "${DBNAME}"; then
    echo "dbname cannot include hyphens" && exit ${LINENO}
fi

if test -n "${DROP}"; then
    sudo mysql -uroot -p <<EOF
GRANT USAGE ON *.* TO '${DBUSER}'@'localhost';
drop USER '${USER}'@'localhost';
drop database if exists ${DBNAME};
FLUSH PRIVILEGES;
EOF
    # make sure we can't log in
    # mysql -u${USER} -p${PASS} < /dev/null && test 0 -ne $?
    echo successful drop
else
    if test -z "${PASS:-}"; then
	read -sp "enter mysql password for ${DBUSER}: " PASS
    fi

    sudo mysql <<EOF
-- CREATE DATABASE ${DBNAME};
CREATE USER '${DBUSER}'@'localhost' IDENTIFIED BY '${PASS}';
GRANT ALL PRIVILEGES ON ${DBNAME}.* TO '${DBUSER}'@'localhost';
FLUSH PRIVILEGES;
EOF

    sudo mysql -u${DBUSER} -p < /dev/null
    cat <<EOF
dbname: ${DBNAME}
username: ${DBUSER}
EOF
fi
# Local Variables:
# compile-command: "./mysql-create-user-and-db.sh piwigokorea"
# End:
