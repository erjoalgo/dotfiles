#!/bin/bash -x

KEY_NAME="${1}"
shift
if test -z "${KEY_NAME}"; then
    set +x
    echo "usage git-hub-post-ssh-key.sh <KEY_NAME> [-g GITHUB_API_URL]"
    echo "	[-s PATH_TO_ID_RSA_PUB] [-u GITHUB_USER] [-u GITHUB_TOKEN]"
    exit ${LINENO}
fi

GITHUB_API="https://api.github.com"
IDRSAPUB="${HOME}/.ssh/id_rsa.pub"

while getopts "g:s:hu:t:" OPT; do
    case ${OPT} in
	g)
	    GITHUB_API="${OPTARG}"
	    ;;
	s)
	    IDRSAPUB="${OPTARG}"
	    ;;
	t)
	    GITHUB_TOKEN="${OPTARG}"
	    ;;
	u)
	    GITHUB_USERNAME="${OPTARG}"
	    ;;
	h)
	    less $0
	    exit 0
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

if test -n  "${GITHUB_TOKEN}"; then
    curl "${GITHUB_API}/user/keys" \
	-H "Content-Type: application/json" \
	-H "Authorization: token ${GITHUB_TOKEN}" \
	--data "${DATA}" -i
else
    if test -z "${GITHUB_USERNAME}"; then
	read  -p "enter github username: " GITHUB_USERNAME
    fi
    
    curl "${GITHUB_API}/user/keys" \
	-H "Content-Type: application/json" \
	-u "${GITHUB_USERNAME}" \
	--data "${DATA}" -i
fi
