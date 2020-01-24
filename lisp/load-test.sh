#!/bin/bash -x

set -euo pipefail

sbcl --non-interactive \
     --load ${HOME}/src/stumpwm/stumpwm.asd \
     --eval "(ql:quickload :stumpwm)" \
     --load ~/.sbclrc \
     --load erjoalgo-stumpwmrc.asd  \
     --eval "(ql:quickload :erjoalgo-stumpwmrc)" \
      # |& grep Unha -A10
