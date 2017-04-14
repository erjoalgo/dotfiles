#!/bin/bash -x

KEY_NAME="${1}"
shift
if test -z "${KEY_NAME}"; then
    echo "usage git-hub-post-ssh-key.sh <KEY_NAME>"
    exit ${LINENO}
fi

GITHUB_API="https://api.github.com"
IDRSAPUB="${HOME}/.ssh/id_rsa.pub"

read  -p "enter github username: " GITHUB_USERNAME
read -p "enter github password: " -s GITHUB_PASSWD

while getopts "g:s:" OPT; do
    case ${OPT} in
	g)
	    GITHUB_API="${OPTARG}"
	    ;;
	s)
	    IDRSAPUB="${OPTARG}"
	    ;;
    esac
done

cat ${IDRSAPUB} || exit ${LINENO}

DATA=$(cat <<EOF
{
   "title": "${KEY_NAME}",
   "key": "$(cat ${IDRSAPUB})"
}
EOF
	    )
curl "${GITHUB_API}/user/keys"\
     -H "Content-Type: application/json" -i \
     -u "${GITHUB_USERNAME}:${GITHUB_PASSWD}" \
     --data "${DATA}" -i
     # -H "Authorization: token ${GITHUB_PASSWD}"
