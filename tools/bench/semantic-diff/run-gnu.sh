#!/bin/sh
# Run cases.el on GNU Emacs 31.1; writes gnu.out next to this script.
D=$(cd "$(dirname "$0")" && pwd)
(ulimit -v 8388608; ulimit -c 0; cd "$D" && timeout 300 /usr/local/bin/emacs-31.1 -Q --batch -l "$D/cases.el" >"$D/gnu.out" 2>"$D/gnu.err")
echo "exit=$?"
