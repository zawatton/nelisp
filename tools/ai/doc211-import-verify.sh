#!/usr/bin/env bash
export LC_ALL=C
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
  # Use SOURCE's actual make test-fast selection, then map each selected file.
  tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
  fast_files=$(make -s -C "$source" --eval='print-test-fast:;@printf "%s\n" $(TEST_FAST_FILES)' print-test-fast)
  [[ -n $fast_files ]] || { echo 'FAIL could not read SOURCE TEST_FAST_FILES'; exit 1; }
  python3 - "$map" "$source" "$target" "$tmp" "$fast_files" <<'PY'
import csv,glob,json,os,sys
mapping,src,tgt,out,files=sys.argv[1:]
dest={r['source']:r['destination'] for r in csv.DictReader(open(mapping),delimiter='\t')}
tests=files.split()
for p in tests:
 if p not in dest or dest[p].startswith('DROP:'):
  raise SystemExit(f'test-fast file absent from import map: {p}')
for side,root,key in [('source',src,'source'),('target',tgt,'destination')]:
 with open(f'{out}/{side}.el','w') as f:
  f.write("(require 'ert)\n(require 'jka-compr)\n(setq load-prefer-newer nil load-suffixes (cons \".el\" (delete \".el\" load-suffixes)))\n")
  f.write(f"(setq default-directory {json.dumps(root+'/')})\n")
  paths=[root+'/src',root+'/test',root+'/demo',root+'/scripts']
  paths+=glob.glob(root+'/packages/*/src')
  paths+=glob.glob(root+'/packages/*/lazy')
  for d in paths: f.write(f"(add-to-list 'load-path {json.dumps(d)} t)\n")
  for p in tests: f.write(f"(load {json.dumps(root+'/'+dest[p] if key == 'destination' else root+'/'+p)} nil t)\n")
  f.write('(princ (format "ERT-COUNT %d\\n" (length (ert-select-tests t t))))\n')
  f.write('(ert-run-tests-batch-and-exit t)\n')
PY
  for side in source target; do
    "$emacs_bin" --batch -Q -l "$tmp/$side.el" >"$tmp/$side.out" 2>&1 || true
    grep '^ERT-COUNT ' "$tmp/$side.out" | tail -1 | awk '{print $2}' >"$tmp/$side.count" || true
    sed -n 's/^Ran \([0-9][0-9]*\) tests, \([0-9][0-9]*\) results as \([0-9][0-9]*\) expected, \([0-9][0-9]*\) unexpected.*/\1 \3 \4/p' "$tmp/$side.out" | tail -1 >"$tmp/$side.results" || true
    if [[ ! -s $tmp/$side.count || ! -s $tmp/$side.results ]]; then
      echo "FAIL host ERT suite $side; see $tmp/$side.out"; tail -30 "$tmp/$side.out"; fail=1
    else
      unexpected=$(awk '{print $3}' "$tmp/$side.results")
      if [[ $unexpected != 0 ]] && { [[ $unexpected != 1 ]] || ! grep -q 'FAILED  emacs-buffer-builtins-test/default-and-char-property-bridges-in-source' "$tmp/$side.out" || grep '^   FAILED ' "$tmp/$side.out" | wc -l | grep -qv '^1$'; }; then
        echo "FAIL host ERT failures $side; see $tmp/$side.out"; grep '^   FAILED ' "$tmp/$side.out"; fail=1
      fi
    fi
  done
  sc=$(cat "$tmp/source.count" 2>/dev/null || true); tc=$(cat "$tmp/target.count" 2>/dev/null || true)
  if [[ -z $sc || $sc != "$tc" ]]; then echo "FAIL ERT registered counts source=${sc:-missing} target=${tc:-missing}"; fail=1
  else echo "ERT-COUNT source=$sc target=$tc"; fi
  sr=$(cat "$tmp/source.results" 2>/dev/null || true); tr=$(cat "$tmp/target.results" 2>/dev/null || true)
  if [[ -z $sr || $sr != "$tr" ]]; then echo "FAIL ERT results source=${sr:-missing} target=${tr:-missing}"; fail=1
  else echo "ERT-RESULTS source=$sr target=$tr"; fi
fi
[[ $fail == 0 ]] || exit 1
echo PASS
