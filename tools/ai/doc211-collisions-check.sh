#!/usr/bin/env bash
export LC_ALL=C
set -euo pipefail
target=${1:-$(git rev-parse --show-toplevel)}
source=${2:-${DOC211_SOURCE:-$target/../nel-lib-d211s4-src}}
map=${DOC211_MAP:-$(dirname "$0")/doc211-import-map.tsv}
decisions=${DOC211_COLLISIONS:-$(dirname "$0")/doc211-collisions.tsv}
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
base_ref=${DOC211_COLLISION_BASE:-}
if [[ -z $base_ref ]]; then
  if git -C "$target" show-ref --verify --quiet refs/tags/pre-doc211; then
    base_ref=refs/tags/pre-doc211
  else
    base_ref=HEAD
  fi
fi
if ! base=$(git -C "$target" rev-parse --verify --quiet --end-of-options "${base_ref}^{commit}"); then
  echo "FAIL invalid collision baseline ref: $base_ref" >&2
  exit 1
fi
git -C "$target" ls-tree -r --name-only "$base" >"$tmp/target"
awk -F '\t' 'NR>1 && $2 !~ /^DROP:/ {print $1 "\t" $2}' "$map" >"$tmp/map"
while IFS=$'\t' read -r src dest; do
  if grep -Fxq "$dest" "$tmp/target"; then printf '%s\t%s\n' "$src" "$dest"; fi
done <"$tmp/map" | LC_ALL=C sort -u >"$tmp/found"
awk -F '\t' 'NR>1 {print $1 "\t" $2}' "$decisions" | LC_ALL=C sort -u >"$tmp/decided"
comm -23 "$tmp/found" "$tmp/decided" >"$tmp/missing"
if [[ -s $tmp/missing ]]; then echo "FAIL undecided collisions:"; cat "$tmp/missing"; exit 1; fi
for src in src/nelisp-coding.el src/nelisp-text-buffer.el src/nelisp-regex.el test/nelisp-coding-test.el test/fixtures/ bin/nemacs; do
  grep -Fq "${src}"$'\t' "$decisions" || { echo "FAIL missing design collision: $src"; exit 1; }
done
grep -Fq $'\tdocs/design/nemacs/' "$decisions" || { echo 'FAIL design-doc namespace decision absent'; exit 1; }
echo "PASS mapped-path-collisions=$(wc -l <"$tmp/found") design-cases=7 baseline=$base"
