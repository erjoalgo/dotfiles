#!/bin/bash -x

set -euo pipefail

REPO_NAME=${1:-$(basename $(pwd))}

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

test -n "${DESCRIPTION:-}" || \
    read  -p "enter description for ${REPO_NAME}: " DESCRIPTION

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


if test -n "${GITHUB_TOKEN:-}"; then
    AUTH_OPT_KEY_VAL="-H 'Authorization: token ${GITHUB_TOKEN}'"
elif test -e ${HOME}/.netrc && grep -F "$(basename ${GITHUB_API})" ${HOME}/.netrc; then
    echo "using netrc..." >&2
    AUTH_OPT_KEY_VAL="-n"
else
    if test -z "${GITHUB_USERNAME:-}"; then
	read  -p "enter github username: " GITHUB_USERNAME
    fi
    AUTH_OPT_KEY_VAL="-u ${GITHUB_USERNAME}"
fi

RESP=$(curl -s "${GITHUB_API}/user/repos" \
     -H "Content-Type: application/json" \
     -H "Accept: application/vnd.github.v3+json" \
     ${AUTH_OPT_KEY_VAL} \
     --data "${DATA}")

python -c "import json, sys; print(sys.stdin)" <<< "${RESP}"
URL=$(python -c "import json, sys; print(json.load(sys.stdin)['html_url'])" <<< "${RESP}")
cat<<EOF
${RESP}
EOF

git init
git remote add origin "${URL}"
