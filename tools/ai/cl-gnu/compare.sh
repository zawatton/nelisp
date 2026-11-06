#!/usr/bin/env bash
# Usage: CLGNU_BIN=BINARY compare.sh [-v]
# Run every corpus on BINARY and diff against the GNU 31.1 expected files.
# Prints "corpus matched/total"; exit 0 iff every corpus is identical.
# -v also prints the differing lines.  CLGNU_ONLY="pcase setf" limits the corpora.
here=$(cd "$(dirname "$0")" && pwd) || exit 1
[ -x "${CLGNU_BIN:-}" ] || { echo "CLGNU_BIN is not an executable" >&2; exit 2; }
rc=0
for c in ${CLGNU_ONLY:-pcase defstruct generic setf}; do
  out=$(mktemp) || exit 1
  ( ulimit -v 8388608; ulimit -c 0; cd "$here/../../.." && CLGNU_CORPUS="$here/corpus-$c.el" \
    timeout "${CLGNU_TIMEOUT:-120}" "$CLGNU_BIN" --load "$here/run-corpus.el" -- "$here/corpus-$c.el" 2>/dev/null ) \
    | grep -aE '^[0-9][0-9] ' > "$out"
  total=$(wc -l < "$here/expected-$c.txt")
  matched=$(comm -12 <(sort "$here/expected-$c.txt") <(sort "$out") | wc -l)
  echo "$c $matched/$total"
  if ! cmp -s "$here/expected-$c.txt" "$out"; then
    rc=1
    [ "$1" = "-v" ] && diff "$here/expected-$c.txt" "$out" | grep -a '^[<>]'
  fi
  rm -f "$out"
done
exit $rc
