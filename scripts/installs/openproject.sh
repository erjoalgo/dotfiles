#!/bin/bash -x

set -euo pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"


sudo apt-get install -y postgresql postgresql-client
sudo apt-get install -y libpq-dev

./ruby.sh

DB_USER=${DB_NAME:-openproject}
DB_NAME=${DB_NAME:-openproject_dev}
DB_NAME_TEST=${DB_NAME_TEST:-openproject_test}
if test -z "${DB_PASS:-}"; then
  read -sp"enter postgres password: " DB_PASS
fi

# psql-create-user-and-db.sh -u ${DB_USER}
# 604 ponderosa
# # sudo -upostgres createdb -O ${DB_USER} openproject_test

sudo -upostgres psql<<EOF
create user ${DB_USER} password '${DB_PASS}';
CREATE DATABASE ${DB_NAME} OWNER ${DB_USER};
CREATE DATABASE ${DB_NAME_TEST} OWNER ${DB_USER};
EOF

sudo -upostgres psql<<EOF
GRANT ALL PRIVILEGES ON DATABASE "${DB_NAME}" to ${DB_USER};
GRANT ALL PRIVILEGES ON DATABASE "${DB_NAME_TEST}" to ${DB_USER};
EOF

# sudo -upostgres createdb -O "${DB_USER}" "${DB_NAME}"
# sudo -upostgres createdb -O "${DB_USER}" "${DB_NAME_TEST}"

# if ! command -v node; then
#   # ./npm-install.sh
# fi

PROFILE_ENV=${HOME}/.profile-env
source  "${PROFILE_ENV}"

# https://github.com/nodenv/nodenv/issues/170
eval "$(nodenv init -)"

# https://github.com/rbenv/rbenv/issues/1300
eval "$(rbenv init -)"

ruby --version
# ruby 2.7.2p137 (2020-10-01 revision 5445e04352) [x86_64-linux]
bundler --version
# Bundler version 2.0.2
node --version
# v12.6.1
npm --version
# 6.14.4

OPENPROJECT_SRC=${OPENPROJECT_SRC:-${HOME}/git/openproject}

test -d "${OPENPROJECT_SRC}" ||  \
  git clone https://github.com/opf/openproject.git "${OPENPROJECT_SRC}"

cd "${OPENPROJECT_SRC}"

rbenv install -s $(cat .ruby-version)
# Install gem dependencies
# If you get errors here, you're likely missing a development dependency for your distribution
bundle install

# Install node_modules
npm install

insert-text-block  \
  '# 9ab02fe5-3433-4be1-8b9a-6d38737701b3-openproject-database'  \
  config/database.yml <<EOF
default: &default
  adapter: postgresql
  encoding: unicode
  host: localhost
  username: ${DB_USER}
  password: ${DB_PASS}

development:
  <<: *default
  database: ${DB_NAME}

test:
  <<: *default
  database: ${DB_NAME_TEST}
EOF

RAILS_ENV=development bin/rails db:migrate
RAILS_ENV=development bin/rails db:seed

gem install foreman

foreman start -f Procfile.dev
