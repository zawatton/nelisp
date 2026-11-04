#!/bin/bash
# Doc 238 B0.2: the frozen build21 image bumps the function epoch on each listed
# operation, the guard checks recorded on it match build18, and the B0-less
# build20 image fails the same probe (negative control).
D=$(cd "$(dirname "$0")" && pwd); B=$HOME/.cache/tmp/nelisp-sbcl-impl-builds
I=$B/build-on21/nelisp-dynamic; NEG=$B/build-on20/nelisp-dynamic
[ "$(sha256sum "$I" | cut -d' ' -f1)" = "$(cat "$D/b21/image.sha256")" ] || exit 1
W=$(mktemp -d) || exit 1; trap 'rm -rf "$W"' EXIT
( ulimit -v 8388608; ulimit -c 0; cd "$W" && timeout 120 "$I" --load "$D/epoch-probe.el" < /dev/null > "$W/on.out" 2>&1 )
( ulimit -v 8388608; ulimit -c 0; cd "$W" && timeout 120 "$NEG" --load "$D/epoch-probe.el" < /dev/null > "$W/neg.out" 2>&1 )
grep -q '^PROBE OK' "$W/on.out" || exit 1
for op in fset fset-redefine defalias defun fmakunbound advice-add advice-remove load-redefine; do
  grep -q "^OK   $op: " "$W/on.out" || exit 1; done
grep -q '^PROBE OK' "$W/neg.out" && exit 1
C=$D/b21/checks.out
for k in 'HASH-CUSTOM equal=t' 'LOGAND-FSET equal=t' 'LOGAND-RESTORED equal=t'; do grep -q "^$k" "$C" || exit 1; done
[ "$(grep -c '^SUBR-RESTORED .* equal=t' "$C")" -eq 4 ] || exit 1
diff <(sed 1d "$C" | sed 's/elapsed=[0-9.]*//') <(sed 1d "$D/b21/checks-b18.out" | sed 's/elapsed=[0-9.]*//') > /dev/null
