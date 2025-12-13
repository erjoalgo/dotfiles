#!/bin/bash -x

set -euo pipefail

cd $(git rev-parse --show-toplevel)
pwd
PRE_PUSH=.githooks/pre-push
mkdir -p $(dirname "${PRE_PUSH}")
cat <<EOF > "${PRE_PUSH}"
#!/bin/bash -x

set -euo pipefail

echo "this is the pre-push hook"
exit 0
EOF

chmod +x "$(pwd)/${PRE_PUSH}"
