#!/bin/bash -x

# EMAIL=${1:-erjoalgo@gmail.com}
EMAILS=${*}
test -n "${EMAILS}" || exit ${LINENO}

if test "$PAM_TYPE" = "open_session" ; then
   SUBJECT="$PAM_SERVICE login on $(hostname -s) for account $PAM_USER"
   cat <<EOF | mutt -s "${SUBJECT}" ${EMAILS}
        User: $PAM_USER
        Remote Host: $PAM_RHOST
        Service: $PAM_SERVICE
        TTY: $PAM_TTY
        Date: $(date)
        Server: $(uname -a)
EOF
fi
# add the following line to /etc/pam.d/sshd
# session optional pam_exec.so /usr/local/bin/send-mail-on-ssh-login.sh erjoalgo@gmail.com
