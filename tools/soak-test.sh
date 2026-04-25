#!/usr/bin/env bash
# tools/soak-test.sh — Phase 7.5.3 soak test harness (Doc 32 v2 §2.7 + §7)
#
# Doc 32 v2 §7 establishes a 2-tier soak gate:
#   - blocker (CI、SOAK_DURATION_HOURS=1) — every release-qualification run
#   - post-ship audit (release-audit weekly、SOAK_DURATION_HOURS=24) —
#     RSS growth ceiling per §7 完遂 gate (10MB / 24h)
#
# Wraps `bin/anvil mcp serve` and samples RSS at start / end so we can
# assert on growth without a heavy time-series infra.  GC pause p99
# spike detection is post-ship audit scope (Doc 30 §5.2 integration、
# this harness only catches the coarse leak signal).
#
# Usage:
#   SOAK_DURATION_HOURS=1  ./tools/soak-test.sh   # blocker
#   SOAK_DURATION_HOURS=24 ./tools/soak-test.sh   # post-ship audit
#
# Exit code:
#   0 — RSS growth under the tier threshold
#   1 — RSS growth >= threshold (memory leak suspected) or daemon died
#   2 — environment misconfiguration (anvil binary missing etc.)

set -uo pipefail

DURATION_HOURS="${SOAK_DURATION_HOURS:-1}"
DURATION_SEC=$((DURATION_HOURS * 3600))

# Resolve repo root.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

ANVIL_BIN="${ANVIL_BIN:-$REPO_ROOT/bin/anvil}"
if [ ! -x "$ANVIL_BIN" ]; then
  echo "ERROR: anvil binary not executable: $ANVIL_BIN" >&2
  exit 2
fi

echo "Phase 7.5.3 soak harness"
echo "  duration : ${DURATION_HOURS}h (${DURATION_SEC}s)"
echo "  binary   : $ANVIL_BIN"

# 1. Start anvil daemon.  We launch in MCP serve mode and detach into
#    the background; the trap ensures we always reap the child.
"$ANVIL_BIN" mcp serve >/tmp/nelisp-soak-stdout.log 2>/tmp/nelisp-soak-stderr.log &
ANVIL_PID=$!
trap 'kill "$ANVIL_PID" 2>/dev/null || true; wait "$ANVIL_PID" 2>/dev/null || true' EXIT

# Give the daemon a moment to materialise so the first RSS sample is
# meaningful (otherwise we sample during fork/exec and undercount).
sleep 2

if ! kill -0 "$ANVIL_PID" 2>/dev/null; then
  echo "ERROR: anvil daemon exited during warmup" >&2
  cat /tmp/nelisp-soak-stderr.log >&2 || true
  exit 1
fi

START_RSS=$(ps -o rss= -p "$ANVIL_PID" 2>/dev/null | tr -d ' ')
START_RSS=${START_RSS:-0}
START_TS=$(date +%s)

echo "  start RSS: ${START_RSS} KB"

# 2. Workload loop.  We do not pump real MCP traffic here (CI ARM
#    runners may not have nc on PATH); the soak proves the *idle* leak
#    floor first.  Real MCP traffic loop is post-ship audit scope, see
#    release-audit weekly job (Doc 32 v2 §7).
SAMPLE_INTERVAL=60
NEXT_SAMPLE=$((START_TS + SAMPLE_INTERVAL))
PEAK_RSS="$START_RSS"

while :; do
  NOW=$(date +%s)
  ELAPSED=$((NOW - START_TS))
  if [ "$ELAPSED" -ge "$DURATION_SEC" ]; then
    break
  fi

  # Daemon liveness check + RSS sample.
  if ! kill -0 "$ANVIL_PID" 2>/dev/null; then
    echo "ERROR: anvil daemon exited during soak (elapsed ${ELAPSED}s)" >&2
    cat /tmp/nelisp-soak-stderr.log >&2 || true
    exit 1
  fi

  if [ "$NOW" -ge "$NEXT_SAMPLE" ]; then
    CUR_RSS=$(ps -o rss= -p "$ANVIL_PID" 2>/dev/null | tr -d ' ')
    CUR_RSS=${CUR_RSS:-0}
    if [ "$CUR_RSS" -gt "$PEAK_RSS" ]; then
      PEAK_RSS="$CUR_RSS"
    fi
    NEXT_SAMPLE=$((NEXT_SAMPLE + SAMPLE_INTERVAL))
  fi

  sleep 5
done

# 3. Final RSS sample + growth calculation.
END_RSS=$(ps -o rss= -p "$ANVIL_PID" 2>/dev/null | tr -d ' ')
END_RSS=${END_RSS:-0}
GROWTH_KB=$((END_RSS - START_RSS))
GROWTH_MB=$((GROWTH_KB / 1024))

# 4. Tier threshold (Doc 32 v2 §7 完遂 gate):
#    blocker (1h)        : < 5 MB
#    post-ship audit (24h): < 10 MB
if [ "$DURATION_HOURS" -le 1 ]; then
  THRESHOLD_MB=5
  TIER="blocker"
else
  THRESHOLD_MB=10
  TIER="post-ship audit"
fi

echo ""
echo "Soak result (${TIER} tier):"
echo "  start RSS : ${START_RSS} KB"
echo "  peak RSS  : ${PEAK_RSS} KB"
echo "  end RSS   : ${END_RSS} KB"
echo "  growth    : ${GROWTH_KB} KB (${GROWTH_MB} MB) over ${DURATION_HOURS}h"
echo "  threshold : ${THRESHOLD_MB} MB"

if [ "$GROWTH_MB" -lt "$THRESHOLD_MB" ]; then
  echo "  verdict   : PASS"
  exit 0
else
  echo "  verdict   : FAIL — memory leak suspected"
  exit 1
fi
