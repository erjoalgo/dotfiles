#!/bin/bash -x

set -euo pipefail

CONF_DIR=${HOME}/.config/common-lisp/source-registry.conf.d/
mkdir -p ${CONF_DIR}

LINK_FARM="${HOME}/.asd-link-farm/"
mkdir -p ${LINK_FARM}

insert-text-block ';; 8fa697e3-0a47-4d17-be60-83cef7b8e3ce-add-default-asdf-link-farm' \
                  ${CONF_DIR}/42-asd-link-farm.conf<<EOF

;; (:directory "${LINK_FARM}") this doesn't work

(:tree "${LINK_FARM}")

;; Local Variables:
;; mode: lisp
;; End:
EOF
