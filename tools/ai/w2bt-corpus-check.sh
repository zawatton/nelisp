#!/bin/bash
# usage: w2bt-corpus-check.sh BINARY  -- run the bytecode backtrace corpus and diff against GNU's output.
here=$(cd "$(dirname "$0")/../.." && pwd)
bin=${1:?binary}; ulimit -v 8388608
tmp=$(mktemp -d "$here/target/tmp/w2bt.XXXXXX") || exit 1
"$here/test/w2bt-backtrace-corpus-gen.sh" "$tmp/c.el" >/dev/null 2>&1 || exit 1
timeout 120 /usr/local/bin/emacs --batch -Q -l "$tmp/c.el" > "$tmp/gnu.txt" 2>&1
(cd "$tmp" && timeout 300 "$bin" --load c.el > nl.txt 2>&1)
diff "$tmp/gnu.txt" "$tmp/nl.txt" && [ -s "$tmp/nl.txt" ] && echo "w2bt corpus PASS" ; rc=$?; rm -rf "$tmp"; exit $rc
