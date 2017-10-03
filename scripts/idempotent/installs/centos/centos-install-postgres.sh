#!/bin/bash
sudo yum -y install postgresql-server postgresql-contrib

sudo sed -i 's/ident\|peer/md5/g' /var/lib/pgsql/data/pg_hba.conf || exit ${LINENO}

sudo systemctl stop postgresql
sudo systemctl disable postgresql
sudo systemctl enable postgresql
sudo systemctl start postgresql


