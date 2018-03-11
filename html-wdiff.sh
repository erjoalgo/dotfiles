#!/bin/bash -x

set -euo pipefail

cat <<EOF
<!DOCTYPE html>
<html>
<head>
  <title>${1}</title>
  <meta charset="UTF-8">
</head>
<body>
EOF
wdiff \
    --start-delete '<font size="3" color="red">' \
    --end-delete '</font>' \
    --start-insert  '<font size="3" color="green">' \
    --end-insert '</font>' \
    -n "$1" "$2" \
     | sed -e 's/^$\|[.] *$/<br>/g'

cat <<EOF
</body>
</html>
EOF
