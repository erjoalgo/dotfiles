#!/bin/bash -x

REPO_NAME="${1}" && shift
REPO_NAME=${REPO_NAME:-$(basename $(pwd))}

GITHUB_API="https://api.github.com"

while getopts "g:s:hu:t:d:p" OPT; do
    case ${OPT} in
	g)
	    GITHUB_API="${OPTARG}"
	    ;;
	p)
	    PRIVATE=true
	    ;;
	d)
	    DESCRIPTION="${OPTARG}"
	    ;;
	t)
	    PRIVATE="${OPTARG}"
	    ;;
	u)
	    GITHUB_USERNAME="${OPTARG}"
	    ;;
	t)
	    GITHUB_TOKEN="${OPTARG}"
	    ;;
	h)
	    less $0
	    exit 0
	    ;;
    esac
done

PRIVATE=${PRIVATE:-false}

# Accept: application/vnd.github.v3+json

DATA=$(cat <<EOF
{
  "name": "${REPO_NAME}",
  "description": "${DESCRIPTION}",
  "private": ${PRIVATE},
  "has_issues": true,
  "has_projects": true,
  "has_wiki": true
}
EOF
)

if test -z "${DESCRIPTION}"; then
    read  -p "enter description for ${REPO_NAME}: " DESCRIPTION
fi

if test -n  "${GITHUB_TOKEN}"; then
    AUTH_OPT_KEY="-H"
    AUTH_OPT_VAL="Authorization: token ${GITHUB_TOKEN}"
else
    if test -z "${GITHUB_USERNAME}"; then
	read  -p "enter github username: " GITHUB_USERNAME
    fi
    AUTH_OPT_KEY="-u"
    AUTH_OPT_VAL="${GITHUB_USERNAME}"
fi


RESP=$(curl -s "${GITHUB_API}/user/repos" \
     -H "Content-Type: application/json" \
     -H "Accept: application/vnd.github.v3+json" \
     ${AUTH_OPT_KEY} "${AUTH_OPT_VAL}" \
     --data "${DATA}")

python -c "import json, sys; print(sys.stdin)" <<< "${RESP}"
URL=$(python -c "import json, sys; print(json.load(sys.stdin)['html_url'])" <<< "${RESP}")
cat<<EOF
${RESP}
EOF

git init
git remote add origin "${URL}"
