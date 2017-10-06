#!/bin/bash -x
set -euo pipefail

LINE=${1} && shift
FILE=${1} && shift

UUID=$(grep -o '[a-fA-Z0-9-]*' <<< "${LINE}")

while getopts "bh" OPT; do
    case ${OPT} in
	b)
	    BEGINING_APPEND=true
	    ;;
	h)
	    less $0
	    exit 0
	    ;;
    esac
done

BEGINING_APPEND=${BEGINING_APPEND:-}

# https://serverfault.com/questions/137829/
# sed '\:// START TEXT:,\:// END TEXT:d' file

# sed '/<function type="class">/!b;N;N;/<function type="class">\s*\n\s*<arg name="class.name">com.mycompany.name.UnLockIssueFunction<\/arg>\s*\n\s*<\/function>/d' file

# sed "/^.*${UUID}.*/!b;N;N;/^.*${UUID}.*/d" test
# sed "\:${UUID},\:${UUID}:d" test

REMOVE_BLOCK_SCRIPT=$(cat <<EOF
)

BLOCK=$({
    echo "${LINE}"
    cat /dev/stdin
    echo "${LINE}"
})
    
{
    test -n "${BEGINING_APPEND}" && echo "${BLOCK}"
    python -c "${REMOVE_BLOCK_SCRIPT}" "${UUID}" < ${FILE}
    test -z "${BEGINING_APPEND}" && echo "${BLOCK}"
} > /tmp/${UUID}

mv /tmp/${UUID} ${FILE}.new


# Local Variables:
# mode: sh
# compile-command: "./text-block '# 07b37c2c-a63f-11e7-94e3-cff9e57fa83f' test -b  <<< \"$(echo -e 'PENDEJO\\nCABRÓN')\""
# End:
