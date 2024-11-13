#!/usr/bin/env bash

set -xeuo pipefail

# Set save-runtime-options to true, values of runtime options
# –dynamic-space-size and –control-stack-size that were used to start
# SBCL are stored in the standalone executable, and restored when the
# executable is run. This also inhibits normal runtime option
# processing, causing all command line arguments to be passed to the
# toplevel.

sbcl --load dlgo.asd \
     --eval '(ql:quickload :dlgo)' \
     --eval '(asdf:make :dlgo)' \
     --eval "(sb-ext:save-lisp-and-die \"dlgo-cli\"
              :executable t
              :compression t
              :save-runtime-options t
              :toplevel #'dlgo.tui:start-game)"
#     --eval '(quit)'
     # Add this line for compressed binary.

mv dlgo-cli ./bin
