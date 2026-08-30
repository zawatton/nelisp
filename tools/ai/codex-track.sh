#!/usr/bin/env bash
# Dispatch one coordinator-prepared codex track.
#
# Exists because of two failure modes this repo actually hit, both of which
# cost real work rather than being hypothetical:
#
#  1. WORK LOSS.  The convention is that the agent never runs git (its sandbox
#     blocks .git writes) and the coordinator commits afterwards.  That leaves
#     the whole run uncommitted, so when one track's clone was removed under
#     memory pressure mid-run, every edit went with it and had to be recovered
#     by replaying apply_patch payloads out of ~/.codex/sessions/*/rollout-*.jsonl.
#     This script snapshots the work tree into the clone's own git on a timer,
#     from the COORDINATOR side (which can write .git), so a lost sandbox costs
#     at most one snapshot interval.
#
#  2. STALE TREE.  A brief that says both "check out SHA X" and "do not run
#     git" is self-contradictory: the agent cannot do the first, so it reads
#     whatever the tree already held.  That happened, and the agent then
#     reported four mechanisms as "not present" when all four were present in
#     the intended tree.  The fix is structural, not a wording tweak: the
#     coordinator prepares the tree, the brief never mentions checking out, and
#     the agent is handed a BRIEF-CONTEXT.txt it must quote back -- so a stale
#     tree is visible in the REPORT rather than only in whatever the agent
#     happens to grep.
#
# Usage:
#   tools/ai/codex-track.sh NAME BASE_SHA BRIEF_FILE [WORKROOT]
#
# Produces, under WORKROOT (default: $TMPDIR or /tmp):
#   NAME/                  the prepared clone, checked out at BASE_SHA
#   NAME/BRIEF-CONTEXT.txt the starting state the agent must quote back
#   NAME.out               the codex transcript
#   NAME.snapshots.log     one line per snapshot commit
set -u

NAME=${1:?usage: codex-track.sh NAME BASE_SHA BRIEF_FILE [WORKROOT]}
BASE=${2:?missing BASE_SHA}
BRIEF=${3:?missing BRIEF_FILE}
ROOT=${4:-${TMPDIR:-/tmp}}
SNAP_INTERVAL=${CODEX_TRACK_SNAPSHOT_SECONDS:-180}
MODEL=${CODEX_TRACK_MODEL:-gpt-5.6-sol}
EFFORT=${CODEX_TRACK_EFFORT:-xhigh}

SRC=$(git rev-parse --show-toplevel) || { echo "not in a git repo" >&2; exit 1; }
[ -f "$BRIEF" ] || { echo "brief not found: $BRIEF" >&2; exit 1; }

CLONE="$ROOT/$NAME"
rm -rf "$CLONE"
git clone -q --shared -n "$SRC" "$CLONE" || exit 1
git -C "$CLONE" checkout -q "$BASE" || exit 1
git -C "$CLONE" checkout -q -B "codex/$NAME" || exit 1

# --- the starting state the agent must quote back (fixes failure mode 2) ---
{
  printf 'BRIEF-CONTEXT for track: %s\n' "$NAME"
  printf 'This tree is ALREADY PREPARED.  git is NOT available to you.\n'
  printf 'Do not attempt to check anything out; do not run git at all.\n\n'
  printf 'base-sha: %s\n' "$(git -C "$CLONE" rev-parse HEAD)"
  printf 'base-subject: %s\n' "$(git -C "$CLONE" log -1 --format=%s)"
  printf 'tree-digest: %s\n' "$(git -C "$CLONE" rev-parse HEAD^{tree})"
  printf '\nQuote the three values above verbatim in your final report, under a\n'
  printf 'heading "STARTING STATE".  If you cannot, say so loudly -- it means you\n'
  printf 'are not reading the tree this brief describes.\n'
} > "$CLONE/BRIEF-CONTEXT.txt"

echo "[codex-track] $NAME prepared at $(git -C "$CLONE" rev-parse --short HEAD) in $CLONE"

# --- snapshot loop (fixes failure mode 1) ---
SNAPLOG="$ROOT/$NAME.snapshots.log"
: > "$SNAPLOG"
(
  while true; do
    sleep "$SNAP_INTERVAL"
    [ -d "$CLONE" ] || { echo "$(date -Is) clone gone" >> "$SNAPLOG"; exit 0; }
    if [ -n "$(git -C "$CLONE" status --porcelain 2>/dev/null)" ]; then
      git -C "$CLONE" add -A >/dev/null 2>&1
      if git -C "$CLONE" -c user.name=codex-track -c user.email=codex-track@local \
           commit -q -m "wip($NAME): coordinator snapshot" >/dev/null 2>&1; then
        echo "$(date -Is) $(git -C "$CLONE" rev-parse --short HEAD)" >> "$SNAPLOG"
      fi
    fi
  done
) &
SNAP_PID=$!
trap 'kill '"$SNAP_PID"' 2>/dev/null' EXIT

codex exec -C "$CLONE" -m "$MODEL" -c "model_reasoning_effort=$EFFORT" \
  --sandbox workspace-write --skip-git-repo-check < "$BRIEF" > "$ROOT/$NAME.out" 2>&1
rc=$?

kill "$SNAP_PID" 2>/dev/null
# final snapshot so the last edits are never the ones that get lost
if [ -n "$(git -C "$CLONE" status --porcelain 2>/dev/null)" ]; then
  git -C "$CLONE" add -A >/dev/null 2>&1
  git -C "$CLONE" -c user.name=codex-track -c user.email=codex-track@local \
    commit -q -m "wip($NAME): final coordinator snapshot" >/dev/null 2>&1 \
    && echo "$(date -Is) $(git -C "$CLONE" rev-parse --short HEAD) final" >> "$SNAPLOG"
fi

echo "[codex-track] $NAME finished rc=$rc, $(wc -l < "$SNAPLOG") snapshot(s), transcript $ROOT/$NAME.out"
exit $rc
