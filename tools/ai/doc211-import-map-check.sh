#!/usr/bin/env bash
export LC_ALL=C
set -euo pipefail
repo=${1:-${DOC211_SOURCE:-$(git rev-parse --show-toplevel 2>/dev/null)/../nel-lib-d211s4-src}}
map=${DOC211_MAP:-$(dirname "$0")/doc211-import-map.tsv}
here=$(cd "$(dirname "$0")" && pwd)
target=${DOC211_TARGET:-$(git -C "$here/../.." rev-parse --show-toplevel)}
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
git -C "$repo" ls-files >"$tmp/tracked"
tail -n +2 "$map" | cut -f1 >"$tmp/mapped"
comm -23 <(LC_ALL=C sort "$tmp/tracked") <(LC_ALL=C sort "$tmp/mapped") >"$tmp/missing"
comm -13 <(LC_ALL=C sort "$tmp/tracked") <(LC_ALL=C sort "$tmp/mapped") >"$tmp/extra"
bad=$(awk -F '\t' 'NR>1 && $2 ~ /^DROP:/ && $1 !~ /^(packages\/|build\/|target\/|vendor\/nelisp(\/|$)|vendor\/emacs-lisp[^/]*(\/|$))/ && !($1 == "bin/build-nelisp" && $2 == "DROP: superseded by in-tree standalone-reader target") {print $1 "\t" $2}' "$map")
drop_rows=$(awk -F '\t' 'NR>1 && $1 == "bin/build-nelisp" && $2 == "DROP: superseded by in-tree standalone-reader target" {n++} END {print n+0}' "$map")
if (( drop_rows > 0 )); then
  if (( drop_rows != 1 )) || ! grep -Eq '^standalone-reader:[[:space:]]*$' "$target/Makefile" \
     || ! make -s -n -C "$target" standalone-reader >/dev/null 2>&1; then
    bad+=$'\n''bin/build-nelisp (supersession target missing or unrunnable)'
  fi
fi
awk -F '\t' 'NR>1 && $2 !~ /^DROP:/ {print $2}' "$map" | sort | uniq -d >"$tmp/duplicate-destinations"
if [[ -s $tmp/missing || -s $tmp/extra || -n $bad || -s $tmp/duplicate-destinations ]]; then
  echo "unmapped=$(wc -l <"$tmp/missing") extra=$(wc -l <"$tmp/extra") illegal-drops=$(printf '%s\n' "$bad" | sed '/^$/d' | wc -l) duplicate-destinations=$(wc -l <"$tmp/duplicate-destinations")"
  cat "$tmp/missing" "$tmp/extra"; [[ -z $bad ]] || printf '%s\n' "$bad"; exit 1
fi
echo "PASS tracked=$(wc -l <"$tmp/tracked") unmapped=0 illegal-drops=0"
