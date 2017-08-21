#!/bin/bash -x

DBNAME=${1} && shift
USER=${1:-${DBNAME}} && shift
HOST=${1:-localhost} && shift


echo 'enter password for "${USER}"' -n
read -s PASS
echo

if ! test -n "${DBNAME}" -a -n "${USER}" -a -n "${PASS}" -a -n "${HOST}"; then
    echo "missing arguments" && exit ${LINENO}
fi

if grep '-' <<< "${DBNAME}"; then
    echo "dbname cannot have hyphens" && exit ${LINENO}
fi

if test -n "${DROP}"; then
    mysql -uroot -pcontra <<EOF
drop USER '${USER}'@'localhost';
drop database ${DBNAME};
FLUSH PRIVILEGES;
EOF
fi



# cat <<EOF
mysql -uroot -pcontra <<EOF
CREATE USER '${USER}'@'localhost' IDENTIFIED BY '${PASS}';
CREATE DATABASE ${DBNAME};
GRANT ALL PRIVILEGES ON ${DBNAME}.* TO '${USER}'@'localhost';
FLUSH PRIVILEGES;
EOF

# Local Variables:
# compile-command: "./mysql-create-user-and-db.sh piwigokorea"
# End:
