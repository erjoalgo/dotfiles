#!/bin/bash -x

set -euo pipefail

cd $(git rev-parse --show-toplevel)
pwd
PRE_PUSH=$(pwd)/.githooks/pre-push
mkdir -p $(dirname "${PRE_PUSH}")
if test -e "${PRE_PUSH}"; then
    echo "warn: not overwritting existing ${PRE_PUSH}"
fi

cat <<EOF > "${PRE_PUSH}"
#!/bin/bash -x

set -euo pipefail

echo "this is the pre-push hook"

EOF

chmod +x "${PRE_PUSH}"
