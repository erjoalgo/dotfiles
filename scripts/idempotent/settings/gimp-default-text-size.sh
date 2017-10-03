#!/bin/bash -x

set -euo pipefail

insert-text-block '# 2b024c6e-f750-4cb0-97a8-6bab3d9b05ba-gimp-set-default-text-size' \
		  "${HOME}/.gimp-2.8/tool-options/gimp-text-tool" <<EOF
# GIMP gimp-text-tool options
(palette "Basic")
(font "Gilgongo")
(font-size 60.000000)
# end of gimp-text-tool options
EOF
