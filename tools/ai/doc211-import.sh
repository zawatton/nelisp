#!/usr/bin/env bash
export LC_ALL=C
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
target=${1:-$(git -C "$here/../.." rev-parse --show-toplevel)}
source=${2:-${DOC211_SOURCE:-$target/../nel-lib-d211s4-src}}
map=${DOC211_MAP:-$here/doc211-import-map.tsv}
filter=${GIT_FILTER_REPO:-$HOME/.local/bin/git-filter-repo}
command -v "$filter" >/dev/null 2>&1 || [[ -x $filter ]] || { echo "missing git-filter-repo: $filter" >&2; exit 2; }
# These checks always validate the current migration contract, even when an
# existing import must reuse an older map to preserve its rewritten history.
"$here/doc211-import-map-check.sh" "$source"
"$here/doc211-collisions-check.sh" "$target" "$source"
base_ref=${DOC211_BASE_REF:-feat/eln-jit-2026-09-28}
if git -C "$target" show-ref --verify --quiet refs/tags/pre-doc211; then
  base=$(git -C "$target" rev-parse pre-doc211)
elif git -C "$target" show-ref --verify --quiet "refs/heads/$base_ref"; then
  base=$(git -C "$target" rev-parse "$base_ref")
else
  base=$(git -C "$target" rev-parse HEAD)
fi
git -C "$source" tag -f pre-doc211-final "$(git -C "$source" rev-parse HEAD)" >/dev/null
work=$(mktemp -d); trap 'rm -rf "$work"' EXIT
# Find the earliest validated Doc 211 import merge on migration ancestry. Its
# second parent is the source tip whose rewritten genealogy subsequent imports
# must preserve; its first-parent tree identifies the original path map.
branch=doc211-migration
branch_exists=0
original_merge=
original_source_tip=
if git -C "$target" show-ref --verify --quiet "refs/heads/$branch"; then
  branch_exists=1
  migration=$(git -C "$target" rev-parse "refs/heads/$branch")
  mapfile -t merges < <(git -C "$target" rev-list --reverse --topo-order --merges --ancestry-path "$base..$migration")
  for candidate in "${merges[@]}"; do
    subject=$(git -C "$target" show -s --format=%s "$candidate")
    if [[ $subject == 'Import nelisp-emacs-lib history (Doc 211 S4)' ]] \
       && git -C "$target" diff-tree -r --name-only "${candidate}^1" "$candidate" \
            | grep '^nelisp-emacs-lib/' >/dev/null; then
      original_merge=$candidate
      break
    fi
  done
fi
if [[ -n $original_merge ]]; then
  read -ra original_parents <<<"$(git -C "$target" rev-list --parents -n1 "$original_merge")"
  [[ ${#original_parents[@]} == 3 ]] \
    || { echo "FAIL original import merge $original_merge does not have two parents" >&2; exit 1; }
  git -C "$target" merge-base --is-ancestor "$base" "${original_parents[1]}" \
    || { echo 'FAIL pre-import ref is not ancestor of original import first parent' >&2; exit 1; }
  git -C "$target" merge-base --is-ancestor "$original_merge" "$migration" \
    || { echo 'FAIL original import merge is not on migration ancestry path' >&2; exit 1; }
  original_source_tip=${original_parents[2]}
fi
git_dir=$(git -C "$target" rev-parse --absolute-git-dir)
metadata_dir="$git_dir/doc211-import"
history_map_metadata="$metadata_dir/history-map.tsv"
if [[ -f $history_map_metadata ]]; then
  [[ -n $original_source_tip ]] \
    || { echo 'FAIL history-map metadata exists without a validated import merge' >&2; exit 1; }
  [[ -s $history_map_metadata ]] \
    || { echo 'FAIL stored history map is empty' >&2; exit 1; }
  history_map=$history_map_metadata
elif [[ -n $original_merge ]]; then
  history_map="$work/history-map.tsv"
  git -C "$target" show "$original_merge:tools/ai/doc211-import-map.tsv" >"$history_map" \
    || { echo 'FAIL cannot recover original import map from import merge tree' >&2; exit 1; }
  [[ -s $history_map ]] \
    || { echo 'FAIL recovered original import map is empty' >&2; exit 1; }
else
  # A first import uses the current validated map and saves it after success.
  history_map=$map
fi
# A first import must not absorb caller-staged changes when its deterministic
# merge commit records the map used to rewrite source history.
if ((! branch_exists)); then
  git -C "$target" diff --cached --quiet \
    || { echo 'FAIL first import requires a clean target index' >&2; exit 1; }
  git -C "$target" diff --quiet \
    || { echo 'FAIL first import requires a clean tracked worktree' >&2; exit 1; }
fi
# A fresh clone keeps the source clone immutable and gives git-filter-repo stable input.
git clone -q --no-local "$source" "$work/import"
python3 - "$history_map" "$work/paths" <<'PY'
import csv,sys
with open(sys.argv[2],'w') as out:
 for r in csv.DictReader(open(sys.argv[1]),delimiter='\t'):
  d=r['destination']
  if d.startswith('DROP:'): continue
  out.write(f"literal:{r['source']}\n")
  if d!=r['source']: out.write(f"literal:{r['source']}==>{d}\n")
PY
(cd "$work/import" && "$filter" --force --paths-from-file "$work/paths" >/dev/null)
filtered=$(git -C "$work/import" rev-parse HEAD)
git -C "$work/import" branch doc211-filtered "$filtered"
# Bring the target's migration history into the filtered clone for ancestry
# validation without changing target refs or working-tree state.
if (( branch_exists )); then
  git -C "$work/import" fetch -q "$target" "$branch" 2>/dev/null || true
  if [[ -n $original_source_tip ]] \
     && ! git -C "$work/import" merge-base --is-ancestor "$original_source_tip" "$filtered"; then
    echo "FAIL selected history map does not preserve original source tip $original_source_tip" >&2
    exit 1
  fi
  current=$(git -C "$target" rev-parse "refs/heads/$branch")
else
  current=$base
fi
# If the filtered tip is already present, a successful no-op may also persist a
# recovered legacy map. Incompatible/tampered maps have failed above.
if (( branch_exists )) && git -C "$work/import" merge-base --is-ancestor "$filtered" "$current"; then
  if ! git -C "$target" show-ref --verify --quiet refs/tags/pre-doc211; then
    git -C "$target" tag pre-doc211 "$base"
  fi
  mkdir -p "$metadata_dir"
  map_tmp="$history_map_metadata.tmp.$$"
  cp "$history_map" "$map_tmp"
  mv "$map_tmp" "$history_map_metadata"
  cp "$work/import/.git/filter-repo/commit-map" "$metadata_dir/commit-map"
  echo "PASS unchanged source tip=$filtered migration=$current"
  exit 0
fi
if ! git -C "$target" show-ref --verify --quiet refs/tags/pre-doc211; then
  git -C "$target" tag pre-doc211 "$base"
fi
if (( branch_exists )); then
  git -C "$target" checkout -q "$branch"
else
  git -C "$target" checkout -q -b "$branch" "$base"
fi
# Import rewritten source history, then create a deterministic two-parent merge commit.
git -C "$target" fetch -q "$work/import" doc211-filtered
newtip=$(git -C "$target" rev-parse FETCH_HEAD)
git -C "$target" update-ref refs/heads/doc211-import-source "$newtip"
git -C "$target" -c user.name='Doc 211 Import' -c user.email='doc211-import@localhost' merge --no-commit --no-ff --allow-unrelated-histories "$newtip" || true
if [[ -n $(git -C "$target" diff --name-only --diff-filter=U) ]]; then
  echo 'FAIL unresolved merge conflicts; see collision decisions and resolve mappings before retry' >&2
  git -C "$target" merge --abort || true
  exit 1
fi
if [[ -z $original_merge ]]; then
  target_map="$target/tools/ai/doc211-import-map.tsv"
  if ! cmp -s "$history_map" "$target_map"; then
    cp "$history_map" "$target_map"
  fi
  git -C "$target" add -- tools/ai/doc211-import-map.tsv
fi
GIT_AUTHOR_NAME='Doc 211 Import' GIT_AUTHOR_EMAIL='doc211-import@localhost' GIT_COMMITTER_NAME='Doc 211 Import' GIT_COMMITTER_EMAIL='doc211-import@localhost' GIT_AUTHOR_DATE='2000-01-01T00:00:00Z' GIT_COMMITTER_DATE='2000-01-01T00:00:00Z' git -C "$target" commit -q -m "Import nelisp-emacs-lib history (Doc 211 S4)"
git -C "$target" update-ref -d refs/heads/doc211-import-source
mkdir -p "$metadata_dir"
map_tmp="$history_map_metadata.tmp.$$"
cp "$history_map" "$map_tmp"
mv "$map_tmp" "$history_map_metadata"
cp "$work/import/.git/filter-repo/commit-map" "$metadata_dir/commit-map"
echo "PASS merge=$(git -C "$target" rev-parse HEAD) source=$newtip"
