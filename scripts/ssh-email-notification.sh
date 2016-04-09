#!/bin/sh
if ! [ "$PAM_TYPE" != "open_session" ]; then
    SUBJECT="${PAM_SERVICE} login on $(hostname -s) for  ${PAM_USER}"
    cat << EOF | mutt -s "${SUBJECT}"erjoalgo@gmail.com
User: ${PAM_USER}
Remote Host: ${PAM_RHOST}
Service: ${PAM_SERVICE}
TTY: {{PAM_TTY}
Date: $(date)
Server: $(uname -a)
EOF
fi
exit 0
