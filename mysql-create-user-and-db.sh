#!/bin/bash

DBNAME=${1} && shift
USER=${1:-${DBNAME}} && shift
HOST=${1:-localhost} && shift


echo 'enter mysql password for root' -n
read -s ADMINPASS
echo

if ! test -n "${DBNAME}" -a -n "${USER}" -a -n "${PASS}" -a -n "${HOST}"; then
    echo "missing arguments" && exit ${LINENO}
fi

if grep '-' <<< "${DBNAME}"; then
    echo "dbname cannot have hyphens" && exit ${LINENO}
fi

DROP=${DROP:-}

if test -n "${DROP}"; then
    mysql -uroot -p${ADMINPASS} <<EOF
GRANT USAGE ON *.* TO '${USER}'@'localhost';
drop USER '${USER}'@'localhost';
drop database if exists ${DBNAME};
FLUSH PRIVILEGES;
EOF
    # make sure we can't log in
    # mysql -u${USER} -p${PASS} < /dev/null && test 0 -ne $?
    echo successful drop
else
    # cat <<EOF
    echo 'enter mysql password for "${USER}"' -n
    read -s PASS
    echo

    mysql -uroot -p${ADMINPASS} <<EOF
CREATE USER '${USER}'@'localhost' IDENTIFIED BY '${PASS}';
CREATE DATABASE ${DBNAME};
GRANT ALL PRIVILEGES ON ${DBNAME}.* TO '${USER}'@'localhost';
FLUSH PRIVILEGES;
EOF

    mysql -u${USER} -p${PASS} < /dev/null
    cat <<EOF
dbname: ${DBNAME}
username: ${USER}
EOF
fi
# Local Variables:
# compile-command: "./mysql-create-user-and-db.sh piwigokorea"
# End:
