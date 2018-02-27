#!/bin/bash -x

usage()	{
    set +x
    echo "usage: git-hub-post-ssh-key.sh [-k <KEY_NAME>] [-g GITHUB_API_URL]"
    echo "	[-s PATH_TO_ID_RSA_PUB] [-u GITHUB_USER] [-t GITHUB_TOKEN]"
}

GITHUB_API="https://api.github.com"
IDRSAPUB="${HOME}/.ssh/id_rsa.pub"
KEY_NAME=autogen-$(hostname)

while getopts "g:s:hu:t:k:" OPT; do
    case ${OPT} in
        k)
            KEY_NAME="${OPTARG}"
            ;;
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
            usage
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
