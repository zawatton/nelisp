#!/usr/bin/env bash
# Run one codex worker per lane with bounded concurrency.
#   ccore-lane-dispatch.sh MODEL EFFORT JOBS LANE_DIR...
# Each LANE_DIR must contain BRIEF.md.  Per lane this writes codex.log,
# LAST.txt (the worker's final message) and STATUS (exit code and the model
# line of the log header, for launch verification).  Workers get no MCP
# servers and may write only inside their lane.
# Environment passed through to workers: NELISP_BIN, LIB, EMACS.
set -u
[ $# -ge 4 ] || { echo "usage: $0 MODEL EFFORT JOBS LANE_DIR..." >&2; exit 2; }
model=$1 effort=$2 jobs=$3; shift 3
run_lane() {
  local lane=$1
  [ -f "$lane/BRIEF.md" ] || { echo "exit=missing-brief" > "$lane/STATUS"; return 1; }
  ( cd "$lane" || exit 1
    codex exec -c 'mcp_servers={}' -c "model_reasoning_effort=\"$CCORE_EFFORT\"" -m "$CCORE_MODEL" \
      --sandbox workspace-write --skip-git-repo-check -C "$lane" -o "$lane/LAST.txt" \
      "$(cat BRIEF.md)" < /dev/null > codex.log 2>&1
    echo "exit=$? $(grep -m1 '^model:' codex.log) $(grep -m1 '^sandbox:' codex.log)" > STATUS )
}
export -f run_lane
export CCORE_MODEL=$model CCORE_EFFORT=$effort
printf '%s\n' "$@" | xargs -P "$jobs" -I{} bash -c 'run_lane "$1"' _ {}
for lane in "$@"; do printf '%s %s\n' "$(basename "$lane")" "$(cat "$lane/STATUS" 2>/dev/null || echo no-status)"; done
