#!/usr/bin/env bash
# Take a finished codex track's work into the current worktree.
#
# `codex-track.sh' prepares a clone and dispatches; this is the other half.
# It existed only as a sequence typed by hand, and both times it was typed it
# went wrong in a way that looked like something else:
#
#  1. `git apply -3 "$patch" | head -20'.  `head' closed the pipe, `git apply'
#     took SIGPIPE mid-run and rolled back atomically, and the "Applied patch
#     to X cleanly" lines that HAD been printed made it read as applied.  The
#     next twenty minutes were spent looking at a tree that did not contain
#     the change.  Nothing here pipes a command whose exit code matters.
#
#  2. The leftover check was `git diff --stat'.  A `gate-mutation' injection
#     that had been staged was invisible to it, so a tree with a real defect
#     in it read as clean, and two unrelated gates went red an hour later.
#     The check here is `git diff HEAD --name-only' -- staged and unstaged --
#     minus the patch's own file list, so "what the agent changed" and "what
#     else moved" are told apart rather than summed.
#
# It also refuses to apply anything if the destination has uncommitted work,
# because a failed 3-way merge on top of unrelated edits is unrecoverable
# without knowing which hunk came from where.
#
# Usage:
#   tools/ai/codex-harvest.sh NAME BASE_SHA [WORKROOT]
#
#   NAME      the track name given to codex-track.sh
#   BASE_SHA  the base that track was dispatched against
#   WORKROOT  where codex-track.sh put the clone (default $TMPDIR or /tmp)
#
# On success the change is applied and staged, and the file lists are printed.
# Nothing is committed: the diff is for you to read first.
set -u

NAME=${1:?usage: codex-harvest.sh NAME BASE_SHA [WORKROOT]}
BASE=${2:?missing BASE_SHA}
ROOT=${3:-${TMPDIR:-/tmp}}
CLONE="$ROOT/$NAME"
REF="refs/codex/$NAME"

die() { printf 'codex-harvest: %s\n' "$*" >&2; exit 1; }

[ -d "$CLONE" ] || die "no clone at $CLONE"
git rev-parse --git-dir >/dev/null 2>&1 || die "not in a git repository"
git cat-file -e "$BASE^{commit}" 2>/dev/null || die "base $BASE is not a commit here"

# A 3-way apply onto a dirty tree cannot be untangled afterwards.
dirty=$(git status --porcelain | grep -v '^?? ' || true)
if [ -n "$dirty" ]; then
  printf 'codex-harvest: the destination has uncommitted changes:\n%s\n' "$dirty" >&2
  die "commit or stash them first"
fi

# The agent never commits; codex-track.sh's snapshot loop does. Fold anything
# still uncommitted in the clone into one more snapshot so nothing is missed.
if [ -n "$(git -C "$CLONE" status --porcelain 2>/dev/null | grep -v '^?? ' || true)" ]; then
  git -C "$CLONE" add -A >/dev/null 2>&1
  git -C "$CLONE" -c user.name=codex-harvest -c user.email=codex-harvest@local \
      commit -q -m "wip($NAME): harvest snapshot" >/dev/null 2>&1 \
    && printf 'codex-harvest: folded the clone'"'"'s uncommitted work into a snapshot\n'
fi

branch=$(git -C "$CLONE" rev-parse --abbrev-ref HEAD 2>/dev/null)
[ -n "$branch" ] || die "cannot read the clone's branch"
git fetch -q "$CLONE" "refs/heads/$branch:$REF" || die "fetch from the clone failed"

# `.serena/' and `BRIEF-CONTEXT.txt' are the harness's own droppings, never
# the agent's work.
patch=$(mktemp); trap 'rm -f "$patch"' EXIT
git diff "$BASE" "$REF" -- . ':!.serena' ':!BRIEF-CONTEXT.txt' > "$patch" \
  || die "could not diff $BASE..$REF"
if [ ! -s "$patch" ]; then
  printf 'codex-harvest: %s changed nothing against %s\n' "$NAME" "$BASE"
  exit 0
fi

# Never pipe this: a closed pipe rolls the apply back and leaves the success
# lines on screen (failure mode 1 above).
applog=$(mktemp)
if ! git apply -3 "$patch" > "$applog" 2>&1; then
  printf 'codex-harvest: apply FAILED\n' >&2
  cat "$applog" >&2
  rm -f "$applog"
  conflicts=$(git diff --diff-filter=U --name-only || true)
  [ -n "$conflicts" ] && printf 'conflicted:\n%s\n' "$conflicts" >&2
  exit 1
fi
rm -f "$applog"

conflicts=$(git diff --diff-filter=U --name-only || true)
if [ -n "$conflicts" ]; then
  printf 'codex-harvest: 3-way apply left conflicts:\n%s\n' "$conflicts" >&2
  exit 1
fi

# Staged AND unstaged, minus the patch's own files (failure mode 2 above).
changed=$(mktemp); theirs=$(mktemp)
git diff HEAD --name-only | sort > "$changed"
git diff "$BASE" "$REF" --name-only -- . ':!.serena' ':!BRIEF-CONTEXT.txt' | sort > "$theirs"
outside=$(comm -23 "$changed" "$theirs")

printf '\ncodex-harvest: %s applied onto %s\n' "$NAME" "$(git rev-parse --short HEAD)"
printf '  files from the track: %s\n' "$(wc -l < "$theirs" | tr -d ' ')"
git diff "$BASE" "$REF" --stat -- . ':!.serena' ':!BRIEF-CONTEXT.txt' | tail -1

if [ -n "$outside" ]; then
  printf '\ncodex-harvest: FILES CHANGED OUTSIDE THE PATCH -- read these before committing:\n'
  printf '%s\n' "$outside" | sed 's/^/  /'
  printf '  A leftover gate-mutation injection looks exactly like this.  Check\n'
  printf '  each against tools/gate-mutations.txt before assuming it is work.\n'
  rc=2
else
  printf '  nothing changed outside the patch\n'
  rc=0
fi
rm -f "$changed" "$theirs"

printf '\ncodex-harvest: staged, not committed.  Read the diff, then run the gates:\n'
printf '  tools/ai/nelisp-ai.sh check\n'
printf '  make standalone-reader-smokes emacs-parity binary-size-ratchet\n'
exit $rc
