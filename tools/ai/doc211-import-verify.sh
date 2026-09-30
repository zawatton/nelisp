#!/usr/bin/env bash
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
target=${1:-$(git -C "$here/../.." rev-parse --show-toplevel)}
source=${2:-${DOC211_SOURCE:-$target/../nel-lib-d211s4-src}}
mode=${3:-all}
map=${DOC211_MAP:-$here/doc211-import-map.tsv}
evidence=${DOC211_EVIDENCE:-$target/target/doc211-s4-evidence.tsv}
mkdir -p "$(dirname "$evidence")"
if [[ $mode == repeat ]]; then
  filter=${GIT_FILTER_REPO:-$HOME/.local/bin/git-filter-repo}
  tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
  python3 - "$map" "$tmp/paths" <<'PY'
import csv,sys
with open(sys.argv[2],'w') as out:
 for r in csv.DictReader(open(sys.argv[1]),delimiter='\t'):
  d=r['destination']
  if d.startswith('DROP:'): continue
  out.write(f"literal:{r['source']}\n")
  if d!=r['source']: out.write(f"literal:{r['source']}==>{d}\n")
PY
  for n in a b; do
    git clone -q --no-local "$source" "$tmp/$n"
    (cd "$tmp/$n" && "$filter" --force --paths-from-file "$tmp/paths" >/dev/null)
  done
  a=$(git -C "$tmp/a" rev-parse HEAD); b=$(git -C "$tmp/b" rev-parse HEAD)
  [[ $a == "$b" ]] || { echo "FAIL deterministic rewrite $a != $b"; exit 1; }
  # Add one kept-file commit to a third input and prove the old rewritten history remains stable.
  git clone -q --no-local "$source" "$tmp/new"
  f=$(awk -F '\t' 'NR>1 && $2 !~ /^DROP:/ {print $1; exit}' "$map")
  printf '\nDoc 211 repeatability probe.\n' >>"$tmp/new/$f"
  git -C "$tmp/new" add -- "$f"
  GIT_AUTHOR_NAME='Doc 211 Probe' GIT_AUTHOR_EMAIL='probe@localhost' GIT_COMMITTER_NAME='Doc 211 Probe' GIT_COMMITTER_EMAIL='probe@localhost' GIT_AUTHOR_DATE='2001-01-01T00:00:00Z' GIT_COMMITTER_DATE='2001-01-01T00:00:00Z' git -C "$tmp/new" commit -q -m 'S4 repeatability probe'
  (cd "$tmp/new" && "$filter" --force --paths-from-file "$tmp/paths" >/dev/null)
  n=$(git -C "$tmp/new" rev-parse HEAD)
  git -C "$tmp/new" merge-base --is-ancestor "$a" "$n" || { echo 'FAIL old rewritten source tip not preserved after new commit'; exit 1; }
  delta=$(git -C "$tmp/new" rev-list --count "$n" ^"$a")
  [[ $delta == 1 ]] || { echo "FAIL new import added $delta commits; expected 1"; exit 1; }
  printf 'repeat-identical\t%s\nrepeat-new-tip\t%s\nrepeat-new-commits\t%s\n' "$a" "$n" "$delta" >"$evidence"
  echo "PASS repeat-identical=$a new-commits=$delta"
  exit 0
fi
fail=0
if [[ $mode == all || $mode == sha ]]; then
  count=0
  while IFS=$'\t' read -r src dst; do
    [[ $src == source || $dst == DROP:* ]] && continue
    [[ -f $target/$dst && -f $source/$src ]] || { echo "FAIL missing moved file $src -> $dst"; fail=1; continue; }
    a=$(sha256sum "$source/$src" | cut -d' ' -f1); b=$(sha256sum "$target/$dst" | cut -d' ' -f1)
    [[ $a == "$b" ]] || { echo "FAIL sha256 $src -> $dst"; fail=1; }
    count=$((count+1))
  done <"$map"
  echo "sha256-checked=$count"
fi
if [[ $mode == all || $mode == history ]]; then
  evidence_dir=$(dirname "$evidence")
  cmap="$target/.git/doc211-import/commit-map"
  [[ -s $cmap ]] || { echo "FAIL missing filter-repo commit map $cmap"; exit 1; }
  count=0
  while IFS=$'\t' read -r src dst; do
    [[ $src == source || $dst == DROP:* ]] && continue
    sc=$(git -C "$source" rev-list --count --no-merges HEAD -- "$src")
    tc=$(git -C "$target" rev-list --count --no-merges HEAD -- "$dst")
    [[ $sc == "$tc" ]] || { echo "FAIL path commit count $src ($sc) -> $dst ($tc)"; fail=1; }
    count=$((count+1))
  done <"$map"
  printf 'history-paths\t%s\n' "$count" >"$evidence"
  samples=(src/nelisp-coding.el src/nelisp-text-buffer.el src/nelisp-regex.el src/emacs-buffer.el src/emacs-keymap.el)
  for src in "${samples[@]}"; do
    dst=$(awk -F '\t' -v s="$src" '$1==s {print $2}' "$map")
    old=$(git -C "$source" log --follow --format=%H -- "$src" | tail -1)
    mapped=$(awk -v h="$old" '$1==h {print $2}' "$cmap")
    log=$(git -C "$target" log --follow --format=%H -- "$dst")
    [[ -n $mapped && "$log" == *"$mapped"* ]] || { echo "FAIL follow $src original=$old rewritten=$mapped"; fail=1; }
  done
  echo "history-paths=$count follow-samples=${#samples[@]}"
fi
if [[ $mode == ert || $mode == all ]]; then
  emacs_bin=${EMACS:-emacs}
  command -v "$emacs_bin" >/dev/null
  # Load the path-mapped test sources on both trees and compare registered ERT totals.
  tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
  python3 - "$map" "$source" "$target" "$tmp" <<'PY'
import csv,sys
mapping,src,tgt,out=sys.argv[1:]
rows=list(csv.DictReader(open(mapping),delimiter='\t'))
tests=[r for r in rows if r['source'].startswith('test/') and r['source'].endswith('-test.el') and not r['destination'].startswith('DROP:')]
for side,root,key in [('source',src,'source'),('target',tgt,'destination')]:
 with open(f'{out}/{side}.el','w') as f:
  f.write("(require 'ert)\n")
  f.write(f"(add-to-list 'load-path {root+'/src'!r})\n")
  import glob,os
  for d in glob.glob(root+'/packages/*/src')+glob.glob(root+'/packages/*/lazy'):
   f.write(f"(add-to-list 'load-path {d!r} t)\n")
  f.write(f"(add-to-list 'load-path {root+'/test'!r} t)\n")
  for r in tests:
   f.write(f"(load {root+'/'+r[key]!r} nil t)\n")
  f.write('(princ (format "ERT-COUNT %d\\n" (length (ert-select-tests t t))))\n')
  f.write('(ert-run-tests-batch-and-exit t)\n')
PY
  for side in source target; do
    if ! "$emacs_bin" --batch -Q -l "$tmp/$side.el" >"$tmp/$side.out" 2>&1; then
      echo "FAIL host ERT suite $side; see $tmp/$side.out"; tail -30 "$tmp/$side.out"; fail=1
    fi
    grep '^ERT-COUNT ' "$tmp/$side.out" | tail -1 | awk '{print $2}' >"$tmp/$side.count" || true
  done
  sc=$(cat "$tmp/source.count" 2>/dev/null || true); tc=$(cat "$tmp/target.count" 2>/dev/null || true)
  if [[ -z $sc || $sc != "$tc" ]]; then echo "FAIL ERT registered counts source=${sc:-missing} target=${tc:-missing}"; fail=1
  else echo "ERT-COUNT source=$sc target=$tc"; fi
fi
[[ $fail == 0 ]] || exit 1
echo PASS
