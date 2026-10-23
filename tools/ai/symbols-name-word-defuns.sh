#!/usr/bin/env bash
# symbols-name-word-defuns.sh REV BOUNDARY-FILE: fails when a defun named in BOUNDARY-FILE differs from REV.
cd "$(dirname "$0")/../.." || exit 2
(ulimit -v 8388608; exec timeout 280 "${GNU_EMACS:-/usr/local/bin/emacs}" -Q --batch -l tools/ai/symbols-name-word-defuns.el -- "$@")
