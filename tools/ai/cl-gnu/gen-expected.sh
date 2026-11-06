#!/usr/bin/env bash
# Regenerate expected-*.txt from host GNU Emacs (31.1) under the 8 GiB cap.
# Usage: gen-expected.sh   (EMACS overrides the emacs binary)
here=$(cd "$(dirname "$0")" && pwd) || exit 1
emacs=${EMACS:-emacs}
for c in pcase defstruct generic setf; do
  ( ulimit -v 8388608; CLGNU_CORPUS="$here/corpus-$c.el" timeout 120 "$emacs" --batch -Q -l "$here/run-corpus.el" 2>/dev/null ) \
    | grep -E '^[0-9][0-9] ' > "$here/expected-$c.txt" || exit 1
done
wc -l "$here"/expected-*.txt
