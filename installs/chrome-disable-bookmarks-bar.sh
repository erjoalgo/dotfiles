#!/bin/bash -x

set -euo pipefail

chrome-add-policy.sh -b bookmarks.json <<EOF
{
  "BookmarkBarEnabled": false
}
EOF
