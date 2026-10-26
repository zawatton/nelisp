#!/bin/sh
# Usage: check-blread.sh NELISP-BINARY [--mutate]
# Runs the GNU-compiled buffer-local read corpus (blread-src.el) on GNU Emacs and on NELISP-BINARY; the outputs must
# be identical.  --mutate corrupts the NeLisp output first and requires the comparison to FAIL (negative control).
d=$(cd "$(dirname "$0")" && pwd); t=$(mktemp -d) || exit 1
sh "$d/blread-gen.sh" "$d/blread-bc.el" 2>/dev/null
( ulimit -v 8388608; cd "$t" && timeout 60 emacs -Q --batch -l "$d/blread-run.el" > gnu.out 2>/dev/null )
( ulimit -v 8388608; cd "$t" && timeout 120 "$1" --load "$d/blread-run.el" -- > nl.out 2> nl.err < /dev/null )
if [ "$2" = "--mutate" ]; then sed -i "s/^(plain 1)$/(plain 2)/" "$t/nl.out"; fi
if cmp -s "$t/gnu.out" "$t/nl.out" && [ "$(wc -l < "$t/gnu.out")" -ge 30 ]; then r=0; else r=1; fi
if [ "$2" = "--mutate" ]; then [ $r = 1 ] && r=0 || r=1; fi
[ $r = 0 ] || { diff "$t/gnu.out" "$t/nl.out" | head -20; }
rm -rf "$t"; exit $r
