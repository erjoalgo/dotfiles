#!/bin/bash

FN="${HOME}/.gimp-2.8/tool-options/gimp-text-tool"
test -f "${FN}" || exit ${LINENO}
grep "(font-size " "${FN}" && exit 0

cat <<EOF >> "${FN}"
# GIMP gimp-text-tool options

(palette "Basic")
(font "Gilgongo")
(font-size 60.000000)

# end of gimp-text-tool options
EOF
