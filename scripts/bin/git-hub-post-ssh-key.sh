#!/bin/bash -x

usage()	{
    set +x
    echo "usage: git-hub-post-ssh-key.sh [-k <KEY_NAME>] [-g GITHUB_API_URL]"
    echo "	[-s PATH_TO_ID_RSA_PUB] [-u GITHUB_USER] [-t GITHUB_TOKEN]"
}

github-fingerprints() {
    cat <<EOF
These are GitHub's public key fingerprints (in hexadecimal format):

16:27:ac:a5:76:28:2d:36:63:1b:56:4d:eb:df:a6:48 (RSA)
ad:1c:08:a4:40:e3:6f:9c:f5:66:26:5d:4b:33:5d:8c (DSA)
These are the SHA256 hashes shown in OpenSSH 6.8 and newer (in base64 format):

SHA256:nThbg6kXUpJWGl7E1IGOCspRomTxdCARLviKw6E5SY8 (RSA)
SHA256:br9IjFspm1vxR3iA35FWE+4VTyz1hYVLIE2t1/CeyWQ (DSA)
EOF
}

GITHUB_API="https://api.github.com"
IDRSAPUB="${HOME}/.ssh/id_rsa.pub"
KEY_NAME=autogen-$(hostname)

while getopts "g:s:hu:t:k:p" OPT; do
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
        p)
            github-fingerprints
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
