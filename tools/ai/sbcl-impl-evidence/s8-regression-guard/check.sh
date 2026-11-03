#!/bin/sh
# S8.1: the final image/tree adds no gate failure over the frozen12 baseline.
# The gates themselves are red before this work; each comparison is explicit.
cd "$(dirname "$0")" || exit 1
# 1. standalone regression guard: same 17 failing checks as the recorded r3 ON image.
grep -q 'GATE-COUNT: 142 cases, 17 failures' new-b7.txt || exit 1
grep -q 'GATE-COUNT: 142 cases, 17 failures' old-r3.txt || exit 1
test -z "$(comm -3 new-b7.fail old-r3.fail)" || exit 1
# 2. compile: the same 24 erroring files as the merged-tree baseline taken before any edit.
head -24 s0-compile-error-files.txt | sort > .base; sort compile-error-files.txt > .now
cmp -s .base .now || { rm -f .base .now; exit 1; }; rm -f .base .now
# 3. ns-inventory: every finding present only in the final tree names a file from
#    the shared checkout's pre-existing uncommitted overlay, none from this work.
test -s ns-findings-final-only.txt || exit 1
while IFS= read -r line; do
  ok=0
  for f in $(printf '%s\n' "$line" | grep -o '"[^"]*\.el"' | tr -d '"'); do
    grep -qx "$f" shared-overlay-files.txt && ok=1
  done
  [ "$ok" = 1 ] || { echo "unattributed: $line" | cut -c1-160; exit 1; }
done < ns-findings-final-only.txt
echo PASS
