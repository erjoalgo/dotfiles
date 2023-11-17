#!/bin/bash -x

set -euo pipefail

BARE_TOP=$(realpath ${1}) && shift
EMAILS=${*}

test -d ${BARE_TOP}

POST_RECEVE_HOOK=${BARE_TOP}/hooks/post-receive

# test -e ${POST_RECEVE_HOOK}
touch ${POST_RECEVE_HOOK}
chmod +x ${POST_RECEVE_HOOK}

which mutt

insert-text-block '# 2aa53a0f-71aa-4092-a377-436ed09dacb5-send-post-receive-email-notications'  \
                  ${POST_RECEVE_HOOK}<<EOF
SUBJECT=\$(git log --pretty=oneline --abbrev-commit)
BODY=\$(git log -1)
SUBJECT=\$(git log -1 --pretty="%an commited: %s")
echo "\${BODY}" | mutt -s "\${SUBJECT}" -- ${EMAILS}
if test 0 = \$?; then
    echo "email notification probably sent to ${EMAILS}"
else
    echo "failed to send email notification to ${EMAILS}"
fi
EOF

# Local Variables:
# compile-command: "./git-bare-repo-add-post-receive-email-notification.sh /tmp/test/"
# End:
