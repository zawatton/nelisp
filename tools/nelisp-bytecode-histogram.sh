#!/usr/bin/env bash
# Regenerate the census with host Emacs; optional arguments replace the corpus.
set -euo pipefail
tool_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
export NELISP_BYTECODE_HISTOGRAM_OUTPUT="$tool_dir/nelisp-bytecode-opcode-histogram.txt"
exec "${EMACS:-emacs}" -Q --batch -L "$tool_dir" \
  -l nelisp-bytecode-histogram \
  --eval '(let ((files (or command-line-args-left
                          (nelisp-bytecode-source-paths))))
            (setq command-line-args-left nil)
            (nelisp-bytecode-write-histogram
             files (getenv "NELISP_BYTECODE_HISTOGRAM_OUTPUT")))' "$@"
