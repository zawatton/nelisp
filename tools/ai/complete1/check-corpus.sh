#!/bin/bash
# usage: check-corpus.sh BINARY [slow.el]  -- exit 0 iff BINARY's output equals GNU Emacs 31.1's on corpus.el
d=$(cd "$(dirname "$0")" && pwd)
t=$(mktemp -d)
/usr/local/bin/emacs -Q --batch -l "$d/run.el" > "$t/gnu.txt" 2>/dev/null || exit 2
"$d/run-nl.sh" "$1" "$t/nl.txt" ${2:-}
grep -v '^nil$' "$t/nl.txt" > "$t/nl2.txt"
if diff "$t/gnu.txt" "$t/nl2.txt" > "$t/diff.txt"; then rm -rf "$t"; exit 0; fi
head -20 "$t/diff.txt"; exit 1
