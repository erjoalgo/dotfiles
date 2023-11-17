#!/bin/bash -x

set -euo pipefail

WEBDAV_PORT=${WEBDAV_PORT:-81}
WEBDAV_ROOT=${WEBDAV_ROOT:-/var/www/webdav}
WEBDAV_LOCK=${WEBDAV_LOCK:-/var/www/DavLock}

# https://www.digitalocean.com/community/tutorials/
# how-to-configure-webdav-access-with-apache-on-ubuntu-14-04

sudo apt-get install -y apache2

sudo mkdir -p "${WEBDAV_ROOT}"
sudo chown -R www-data:www-data "$(dirname ${WEBDAV_LOCK})"

sudo a2enmod dav
sudo a2enmod dav_fs

DEFAULT_CONF=/etc/apache2/sites-enabled/000-default.conf
if test -e ${DEFAULT_CONF}; then
    sudo unlink ${DEFAULT_CONF}
fi

PORTS_CONF=/etc/apache2/ports.conf
if test -e "${PORTS_CONF}"; then
    sudo mv "${PORTS_CONF}" "${PORTS_CONF}.bak"
fi

sudo insert-text-block  \
    "# 4d8f7b7f-c77a-4a81-b11e-bb4a9e84b7d8-apache-nginx-ports-conflict" \
    "${PORTS_CONF}" <<EOF
# If you just change the port or add more ports here, you will likely also
# have to change the VirtualHost statement in
# /etc/apache2/sites-enabled/000-default.conf
Listen ${WEBDAV_PORT}
# <IfModule ssl_module>
# 	Listen 443
# </IfModule>
# <IfModule mod_gnutls.c>
# 	Listen 443
# </IfModule>
# vim: syntax=apache ts=4 sw=4 sts=4 sr noet
EOF

sudo insert-text-block '# 4b56218f-2a94-4089-b556-177861092b1c-webdav-host' \
     /etc/apache2/sites-enabled/webdav.conf <<EOF
DavLockDB ${WEBDAV_LOCK}
<VirtualHost *:${WEBDAV_PORT}>

DocumentRoot ${WEBDAV_ROOT}

# Available loglevels: trace8, ..., trace1, debug, info, notice, warn,
# error, crit, alert, emerg.
# It is also possible to configure the loglevel for particular
# modules, e.g.
#LogLevel info ssl:warn

ErrorLog \${APACHE_LOG_DIR}/error.log
CustomLog \${APACHE_LOG_DIR}/access.log combined

<Directory ${WEBDAV_ROOT}>
DAV On
</Directory>
</VirtualHost>

EOF

sudo service apache2 restart
