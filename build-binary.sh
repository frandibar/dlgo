#!/usr/bin/env bash

set -euo pipefail

sbcl --load dlgo.asd \
     --eval '(ql:quickload :dlgo)' \
     --eval '(asdf:make :dlgo)' \
     --eval "(sb-ext:save-lisp-and-die \"./bin/dlgo-cli\"
              :executable t
              :compression t
              :save-runtime-options t
              :toplevel #'dlgo.tui:start-game)"
#     --eval '(quit)'
     # Add this line for compressed binary.
