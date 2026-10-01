#!/usr/bin/env bash
export LC_ALL=C
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
target=${1:-$(git -C "$here/../.." rev-parse --show-toplevel)}
source=${2:-${DOC211_SOURCE:-$target/../nel-lib-d211s4-src}}
mode=${3:-all}
map=${DOC211_MAP:-$here/doc211-import-map.tsv}
evidence=${DOC211_EVIDENCE:-$target/target/doc211-s4-evidence.tsv}
if [[ $mode == repeat-evidence ]]; then
  repeat_evidence=${DOC211_IMPORT_REPEAT_EVIDENCE:-$target/target/progress/doc211-import-repeat-evidence.json}
  python3 - "$target" "$source" "$repeat_evidence" <<'PY'
import hashlib,json,pathlib,re,subprocess,sys
target,source,artifact=map(pathlib.Path,sys.argv[1:])
try:
 d=json.loads(artifact.read_text()); a=d['assertions']
 required=set('current_seed_import_no_new_history current_seed_import_noop deterministic_import_merge dirty_first_import_head_unchanged dirty_first_import_rejected dirty_staged_entry_retained fresh_clone_noop_without_git_metadata fresh_clone_recovers_map_from_import_merge fresh_clone_starts_without_git_metadata fresh_import_stores_current_map incompatible_history_map_rejected incompatible_map_commit_map_unchanged incompatible_map_head_unchanged incompatible_map_metadata_restored incompatible_map_no_merge_state incompatible_map_refs_unchanged incremental_first_parent_previous_merge incremental_merge_two_parents incremental_old_source_ancestor incremental_rewritten_delta_one incremental_source_commit_one merge_has_two_parents moved_bridge_change_present moved_bridge_first_parent_previous_merge moved_bridge_io_path_present moved_bridge_no_stays_resurrection moved_bridge_source_commit_one moved_bridge_source_delta_one one_rewritten_delta_commit one_source_commit prior_rewritten_tip_ancestor recovered_original_history_map repeat_no_duplicate_merge same_source_delta_commit source_tip_is_second_parent updated_source_copy_identical updated_source_import_deterministic'.split())
 if len(required)!=37 or set(a)!=required or any(type(v) is not bool or v is not True for v in a.values()): raise ValueError('required 37 assertions must all be present and bool true')
 top_true=set('bridge_first_parent_is_prior_merge bridge_io_path_present bridge_marker_present bridge_stays_path_absent current_seed_no_new_history current_seed_noop current_seed_succeeded delta_parent_is_second_parent deterministic_equal dirty_index_entry_retained dirty_index_head_unchanged dirty_index_rejected fresh_a_history_map_is_current fresh_b_history_map_is_current fresh_clone_has_no_import_metadata fresh_clone_map_recovered_from_merge fresh_clone_noop incompatible_map_commit_map_unchanged incompatible_map_head_unchanged incompatible_map_metadata_restored incompatible_map_no_merge_state incompatible_map_refs_unchanged incompatible_map_rejected incremental_first_parent_is_prior_merge incremental_old_source_ancestor old_rewritten_tip_is_ancestor recovered_history_map_matches_merge_tree repeat_head_unchanged repeat_succeeded repeat_unchanged'.split())
 if not top_true.issubset(d) or any(type(d[k]) is not bool or d[k] is not True for k in top_true): raise ValueError('missing or false top-level outcome status')
 if any(type(v) is bool and v is not True for v in d.values()): raise ValueError('false top-level producer verdict')
 pin=d['frozen_source_pin']; seed=d['seed_head']
 if not re.fullmatch(r'[0-9a-f]{40}',pin) or subprocess.check_output(['git','-C',str(source),'rev-parse','HEAD'],text=True).strip()!=pin or subprocess.check_output(['git','-C',str(source),'rev-parse','refs/tags/pre-doc211-final^{commit}'],text=True,stderr=subprocess.DEVNULL).strip()!=pin: raise ValueError('source HEAD/tag pin mismatch')
 if subprocess.check_output(['git','-C',str(target),'rev-parse','HEAD'],text=True).strip()!=seed: raise ValueError('target HEAD differs from recorded seed_head')
 paths=['tools/ai/doc211-import.sh','tools/ai/doc211-import-map.tsv','tools/ai/doc211-import-map-check.sh','tools/ai/doc211-collisions-check.sh','tools/ai/doc211-collisions.tsv']
 hashes=d['tested_script_and_map_sha256']
 if set(paths)-set(hashes): raise ValueError('producer artifact lacks required file hashes')
 for rel in paths:
  if hashlib.sha256((target/rel).read_bytes()).hexdigest()!=hashes[rel]: raise ValueError('stale/tampered file hash: '+rel)
 harness=target/'tools/ai/doc211-import-repeat-probe.py'; verifier=target/'tools/ai/doc211-import-verify.sh'
 if 'producer_harness_sha256' not in d or 'verifier_sha256' not in d: raise ValueError('artifact lacks producer/verifier hash bindings')
 if not harness.is_file() or not verifier.is_file(): raise ValueError('producer harness/verifier missing from target')
 if hashlib.sha256(harness.read_bytes()).hexdigest()!=d['producer_harness_sha256']: raise ValueError('stale/tampered producer harness hash')
 if hashlib.sha256(verifier.read_bytes()).hexdigest()!=d['verifier_sha256']: raise ValueError('stale/tampered verifier hash')
 if any('fatal:' in str(v.get('output','')) for v in d.values() if isinstance(v,dict) and 'elapsed_seconds' in v): raise ValueError('successful import output contains fatal:')
 base=subprocess.check_output(['git','-C',str(target),'rev-parse','refs/tags/pre-doc211^{commit}'],text=True,stderr=subprocess.DEVNULL).strip()
 clones=d['source_clone_heads']
 if clones!=[pin,pin,pin]: raise ValueError('source clones do not match frozen pin')
 for name,idx in [('import_a',0),('import_b',1)]:
  rec=d[name]; parents=rec['parents']
  if len(parents)!=2 or parents[0]!=base or parents!=d['import_a']['parents'] or rec['head']!=d['import_a']['head']: raise ValueError(name+' has unexpected merge outcome/parents')
 if d['delta_import']['parents']!=[base,d['delta_rewritten_tip']] or d['delta_import_copy']['parents']!=[base,d['delta_rewritten_tip']] or d['delta_import']['head']!=d['delta_import_copy']['head']: raise ValueError('delta import outcome/parents mismatch')
 if d['delta_source_copy_tip']!=d['delta_source_tip'] or d['delta_source_parent']!=pin or d['delta_source_commit_count']!='1' or d['delta_rewritten_delta_commits']!='1' or d['delta_merge_parent_count']!=2: raise ValueError('delta ancestry/count mismatch')
 if d['repeat']['head']!=d['import_a']['head'] or d['repeat']['parents']!=d['import_a']['parents'] or d['fresh_clone_import']['head']!=d['fresh_clone_head_before'] or d['fresh_clone_import']['parents']!=d['import_a']['parents']: raise ValueError('repeat/fresh-clone no-op or recovery mismatch')
 for key in ['import_a','import_b','delta_import','delta_import_copy','incremental_import','bridge_incremental_import']:
  if 'PASS merge=' not in d[key]['output']: raise ValueError(key+' lacks successful merge output')
 for key in ['repeat','current_seed_import','fresh_clone_import']:
  if 'PASS unchanged source tip=' not in d[key]['output']: raise ValueError(key+' lacks successful no-op output')
 ci=d['current_seed_import']
 if d['current_seed_head_before']!=seed or ci['head']!=seed or ci['parents']!=d['current_seed_parent_before'] or ci['parents']!=subprocess.check_output(['git','-C',str(target),'show','-s','--format=%P','HEAD'],text=True).split(): raise ValueError('current-seed no-op/parent mismatch')
 if d['incremental_import']['parents'][0]!=seed or d['incremental_source_parent']!=pin or d['incremental_source_commit_count']!='1' or d['incremental_rewritten_delta_commits']!='1' or d['incremental_parent_count']!=2: raise ValueError('incremental import ancestry/count mismatch')
 if d['bridge_incremental_import']['parents'][0]!=d['incremental_import']['head'] or d['bridge_source_commit_count']!='1' or d['bridge_rewritten_delta_commits']!='1': raise ValueError('bridge import ancestry/count mismatch')
 merge=d['recovered_original_import_merge']; parents=subprocess.check_output(['git','-C',str(target),'rev-list','--parents','-n1',merge],text=True).split()
 subject=subprocess.check_output(['git','-C',str(target),'show','-s','--format=%s',merge],text=True).strip()
 if len(parents)!=3 or parents[2]!=d['original_import_source_tip'] or subject!='Import nelisp-emacs-lib history (Doc 211 S4)' or not d['recovered_history_map_matches_merge_tree']: raise ValueError('original merge/history-map recovery mismatch')
 print('PASS repeat-evidence assertions=37 source-pin=verified seed-head=verified file-hashes=5')
except Exception as e:
 print('FAIL repeat-evidence: '+str(e),file=sys.stderr); sys.exit(1)
PY
  exit $?
fi
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
    sed -n 's/^Ran \([0-9][0-9]*\) tests, \([0-9][0-9]*\) results as expected, \([0-9][0-9]*\) unexpected.*/\1 \2 \3/p' "$tmp/$side.out" | tail -1 >"$tmp/$side.results" || true
    if [[ ! -s $tmp/$side.count || ! -s $tmp/$side.results ]]; then
      echo "FAIL host ERT suite $side; see $tmp/$side.out"; tail -30 "$tmp/$side.out"; fail=1
    else
      read -r ran expected unexpected <"$tmp/$side.results"
      registered=$(cat "$tmp/$side.count")
      if [[ $registered != "$ran" ]] || (( expected + unexpected != ran )); then
        echo "FAIL host ERT summary counts $side; registered=$registered ran=$ran expected=$expected unexpected=$unexpected"; fail=1
      fi
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
