#!/usr/bin/env bash
# release-preflight.sh --- reproducible standalone daily-driver release preflight
#
# Doc 11 M8 (Stability Release Gate): one reproducible command that runs the
# release gates for a standalone daily-driver build and reports results in
# pass/fail buckets.  Long diagnostics are opt-in and scripted (this file),
# while the fast `make test' stays the default developer gate.
#
# Stages (each bucketed PASS / FAIL / SKIP):
#   1. fast-gate      : `make test'                  (host ERT, the fast gate)
#   2. standalone-smoke: `make test-nemacs-gui-bridge' (reader-binary smoke)
#   3. soak           : repeated standalone smoke runs (opt-in via SOAK_ITER)
#
# Usage:
#   scripts/release-preflight.sh              # run fast-gate + smoke
#   SOAK_ITER=5 scripts/release-preflight.sh  # also soak: 5 extra smoke runs
#   scripts/release-preflight.sh --dry-run    # list stages, run nothing
#
# Exit status: 0 when every non-skipped stage passes, 1 otherwise.

set -u

DRY_RUN=0
[ "${1:-}" = "--dry-run" ] && DRY_RUN=1

SOAK_ITER="${SOAK_ITER:-0}"
SMOKE_TIMEOUT="${SMOKE_TIMEOUT:-360}"

# Resolve repo root from this script's location (works from any cwd).
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Bucket accumulators.
declare -a STAGE_NAMES=()
declare -a STAGE_RESULTS=()
OVERALL=0

record() { # name result
  STAGE_NAMES+=("$1")
  STAGE_RESULTS+=("$2")
  [ "$2" = "FAIL" ] && OVERALL=1
  return 0
}

run_stage() { # name command...
  local name="$1"; shift
  if [ "$DRY_RUN" = "1" ]; then
    echo "DRY-RUN would run [$name]: $*"
    record "$name" "SKIP"
    return 0
  fi
  echo "=== preflight stage: $name ==="
  if ( cd "$REPO_ROOT" && "$@" ); then
    record "$name" "PASS"
  else
    record "$name" "FAIL"
  fi
}

# Stage 1: fast gate.
run_stage "fast-gate" make test

# Stage 2: standalone reader-binary smoke.
run_stage "standalone-smoke" timeout "$SMOKE_TIMEOUT" make test-nemacs-gui-bridge

# Stage 3: soak (opt-in) -- repeat the smoke to surface intermittent / leak
# failures.  Each iteration is its own bucket so a single flake is visible.
if [ "$SOAK_ITER" -gt 0 ]; then
  i=1
  while [ "$i" -le "$SOAK_ITER" ]; do
    run_stage "soak-$i" timeout "$SMOKE_TIMEOUT" make test-nemacs-gui-bridge
    i=$((i + 1))
  done
fi

# Failure-bucket report.
echo ""
echo "=== release preflight summary ==="
n=${#STAGE_NAMES[@]}
pass=0; fail=0; skip=0
idx=0
while [ "$idx" -lt "$n" ]; do
  printf '  %-20s %s\n' "${STAGE_NAMES[$idx]}" "${STAGE_RESULTS[$idx]}"
  case "${STAGE_RESULTS[$idx]}" in
    PASS) pass=$((pass + 1)) ;;
    FAIL) fail=$((fail + 1)) ;;
    SKIP) skip=$((skip + 1)) ;;
  esac
  idx=$((idx + 1))
done
echo "  ----------------------------------------"
printf '  PASS=%d FAIL=%d SKIP=%d\n' "$pass" "$fail" "$skip"

if [ "$DRY_RUN" = "1" ]; then
  echo "  (dry-run: no stages executed)"
  exit 0
fi

if [ "$OVERALL" = "0" ]; then
  echo "  RESULT: PASS (release preflight green)"
else
  echo "  RESULT: FAIL (one or more stages failed)"
fi
exit "$OVERALL"
