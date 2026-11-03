#!/bin/sh
# Byte-compile the six kernels (kernels.el, copied verbatim from source-package.el) and run kernel-cases.el on GNU Emacs 31.1.
D=$(cd "$(dirname "$0")" && pwd)
(ulimit -v 8388608; ulimit -c 0; cd "$D" && timeout 300 /usr/local/bin/emacs-31.1 -Q --batch -l "$D/gnu-main.el" >"$D/gnu.out" 2>"$D/gnu.err")
echo "exit=$?"
