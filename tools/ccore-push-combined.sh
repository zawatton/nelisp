#!/usr/bin/env bash
# Publish library and runtime work on the combined GitHub branch.
#
# Usage: tools/ccore-push-combined.sh RUNTIME_CHECKOUT [BRANCH] [REMOTE_URL]
#
# Cherry-picks, onto BRANCH (default ccore/20261004), every commit of this
# checkout's current branch and of the runtime checkout's current branch that
# BRANCH does not contain yet (matched by patch id via git cherry), checks that
# the combined tree's library files equal this checkout's HEAD, then pushes.
# Works in a temporary worktree, so this checkout's working tree is untouched.
# DRY_RUN=1 does everything except the push.  BRANCH is restored unless the
# push succeeds.
set -u
rt=$(cd "${1:?runtime checkout}" && pwd)
branch=${2:-ccore/20261004}
url=${3:-https://github.com/zawatton/nelisp.git}
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root" || exit 2
git fetch -q "$rt" HEAD || exit 1
rt_head=$(git rev-parse FETCH_HEAD)
start_tip=$(git rev-parse "$branch")
wt=$(mktemp -d)
pushed=0
cleanup() { git worktree remove --force "$wt" 2>/dev/null; rm -rf "$wt"; git worktree prune
            [ $pushed = 1 ] || git branch -f "$branch" "$start_tip"; }
trap cleanup EXIT
git worktree add -q "$wt" "$branch" || exit 1
# The newest commit of a source branch already on BRANCH is found through the
# "(cherry picked from commit X)" trailers that -x adds; patch ids are not
# reliable because earlier picks were resolved by hand.
published() { # source-head: newest trailer sha that is an ancestor of it
  git log --format=%b "$branch" | sed -n 's/^(cherry picked from commit \([0-9a-f]*\))$/\1/p' |
    while read -r sha; do git merge-base --is-ancestor "$sha" "$1" 2>/dev/null && { echo "$sha"; break; }; done
}
lib_base=${LIB_BASE:-$(published HEAD)}
rt_base=${RT_BASE:-$(published "$rt_head")}
[ -n "$lib_base" ] && [ -n "$rt_base" ] || { echo "cannot find published bases (set LIB_BASE/RT_BASE)"; exit 1; }
lib_picks=$(git rev-list --reverse --no-merges "$lib_base..HEAD")
rt_picks=$(git rev-list --reverse --no-merges "$rt_base..$rt_head")
[ -n "$lib_picks$rt_picks" ] || { echo "nothing to publish"; exit 0; }
for c in $lib_picks $rt_picks; do
  git -C "$wt" cherry-pick -x "$c" > /dev/null 2>&1 || {
    echo "conflict at $(git log --oneline -1 "$c")"; git -C "$wt" cherry-pick --abort; exit 1; }
done
# Every file a library commit touched must equal this checkout's HEAD; files
# that only exist on the combined branch (runtime history) are not compared.
touched=$(for c in $lib_picks; do git diff-tree --no-commit-id --name-only -r "$c"; done | sort -u)
if [ -n "$touched" ]; then
  differ=$(git -C "$wt" diff --name-only HEAD "$(git rev-parse HEAD)" -- $touched)
  [ -z "$differ" ] || { echo "combined tree differs from this checkout: $(echo $differ | cut -c1-200)"; exit 1; }
fi
echo "publishing $(echo $lib_picks $rt_picks | wc -w) commit(s) to $branch"
[ -z "${DRY_RUN:-}" ] || { echo "DRY_RUN: not pushed; would publish $(git -C "$wt" rev-parse --short HEAD)"; exit 0; }
if out=$(git -C "$wt" push "$url" "HEAD:refs/heads/$branch" 2>&1); then pushed=1; fi
echo "$out" | tail -1
[ $pushed = 1 ]
