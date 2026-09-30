#!/usr/bin/env bash
export LC_ALL=C
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
target=${1:-$(git -C "$here/../.." rev-parse --show-toplevel)}
source=${2:-${DOC211_SOURCE:-$target/../nel-lib-d211s4-src}}
map=${DOC211_MAP:-$here/doc211-import-map.tsv}
filter=${GIT_FILTER_REPO:-$HOME/.local/bin/git-filter-repo}
command -v "$filter" >/dev/null 2>&1 || [[ -x $filter ]] || { echo "missing git-filter-repo: $filter" >&2; exit 2; }
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
if ! git -C "$target" show-ref --verify --quiet refs/tags/pre-doc211; then
  git -C "$target" tag pre-doc211 "$base"
fi
git -C "$source" tag -f pre-doc211-final "$(git -C "$source" rev-parse HEAD)" >/dev/null
work=$(mktemp -d); trap 'rm -rf "$work"' EXIT
# A fresh clone keeps the source clone immutable and gives git-filter-repo stable input.
git clone -q --no-local "$source" "$work/import"
python3 - "$map" "$work/paths" <<'PY'
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
mkdir -p "$target/.git/doc211-import"
cp "$work/import/.git/filter-repo/commit-map" "$target/.git/doc211-import/commit-map"
git -C "$work/import" branch doc211-filtered "$filtered"
branch=doc211-migration
if git -C "$target" show-ref --verify --quiet "refs/heads/$branch"; then
  git -C "$target" checkout -q "$branch"
  current=$(git -C "$target" rev-parse HEAD)
  # If source tip is already an ancestor, no new migration commit is needed.
  git -C "$work/import" fetch -q "$target" "$branch" 2>/dev/null || true
else
  git -C "$target" checkout -q -b "$branch" "$base"
  current=$base
fi
# Import rewritten source history, then create a deterministic two-parent merge commit.
git -C "$target" fetch -q "$work/import" doc211-filtered
newtip=$(git -C "$target" rev-parse FETCH_HEAD)
if git -C "$target" merge-base --is-ancestor "$newtip" "$current"; then
  echo "PASS unchanged source tip=$newtip migration=$current"
  exit 0
fi
git -C "$target" update-ref refs/heads/doc211-import-source "$newtip"
git -C "$target" -c user.name='Doc 211 Import' -c user.email='doc211-import@localhost' merge --no-commit --no-ff --allow-unrelated-histories "$newtip" || true
if [[ -n $(git -C "$target" diff --name-only --diff-filter=U) ]]; then
  echo 'FAIL unresolved merge conflicts; see collision decisions and resolve mappings before retry' >&2
  git -C "$target" merge --abort || true
  exit 1
fi
GIT_AUTHOR_NAME='Doc 211 Import' GIT_AUTHOR_EMAIL='doc211-import@localhost' GIT_COMMITTER_NAME='Doc 211 Import' GIT_COMMITTER_EMAIL='doc211-import@localhost' GIT_AUTHOR_DATE='2000-01-01T00:00:00Z' GIT_COMMITTER_DATE='2000-01-01T00:00:00Z' git -C "$target" commit -q -m "Import nelisp-emacs-lib history (Doc 211 S4)"
git -C "$target" update-ref -d refs/heads/doc211-import-source
echo "PASS merge=$(git -C "$target" rev-parse HEAD) source=$newtip"
