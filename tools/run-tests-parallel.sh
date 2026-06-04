#!/usr/bin/env bash
# run-tests-parallel.sh — super-parallel host ERT runner.
#
# The serial `make test' target loads all ~160 `test/nelisp*-test.el'
# files into ONE `emacs --batch' process and runs every `ert-deftest'
# sequentially.  On a many-core box that wastes the machine: the suite
# is embarrassingly parallel at the *file* level (each test file
# `require's its own deps and ert tests are independent).
#
# This runner shards the SAME file set (and the SAME `-L' load paths)
# the Makefile uses across JOBS worker `emacs' processes, runs them
# concurrently, then aggregates the per-shard ert summaries into one
# pass/fail verdict.  Wall time drops ~= JOBS-fold (minus the fixed
# ~0.1s emacs startup per shard).  The total `Ran N tests' count should
# stay identical to the serial run; the runner enforces the weaker but
# self-contained guarantee that EVERY assigned shard ran to an ert
# summary and exited clean (rc 0, 0 unexpected) — a shard that crashes
# OR silently under-runs (no summary line) fails the verdict.  Confirm
# the total count matches serial `make test' once after changing shards.
#
# Usage:
#   tools/run-tests-parallel.sh            # JOBS = nproc
#   JOBS=8 tools/run-tests-parallel.sh     # explicit shard count
#   tools/run-tests-parallel.sh test/nelisp-eval-test.el ...
#                                          # only the given files (still sharded)
#   EMACS=/path/to/emacs tools/run-tests-parallel.sh
#   SELECTOR='"pattern"' tools/run-tests-parallel.sh
#                                          # ert string/regexp selector per shard
#
# Exit 0 iff every shard reports 0 unexpected results and exits 0.

set -uo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

EMACS="${EMACS:-emacs}"
JOBS="${JOBS:-$(nproc 2>/dev/null || echo 4)}"
OUT_DIR="${OUT_DIR:-target/test-parallel}"
SELECTOR="${SELECTOR:-t}"

# `clean' once up front (mirrors `make test: clean') so no stale .elc
# can shadow a just-edited source while `load-prefer-newer' is on.
find . -name '*.elc' -type f -delete

# --- load paths: keep in lock-step with the Makefile `test' recipe ----------
LOAD_DIRS=(-L lisp -L src -L test -L bench)
for d in $(ls -d packages/*/src 2>/dev/null | sort); do LOAD_DIRS+=(-L "$d"); done
for d in $(ls -d packages/*/test 2>/dev/null | sort); do LOAD_DIRS+=(-L "$d"); done

# --- test file set: same glob as the Makefile, minus the soak test ----------
if [ "$#" -gt 0 ]; then
  mapfile -t FILES < <(printf '%s\n' "$@")
else
  mapfile -t FILES < <(
    { ls test/nelisp*-test.el packages/*/test/nelisp*-test.el 2>/dev/null; } \
      | grep -v 'test/nelisp-worker-soak-test.el' \
      | sort
  )
fi
NFILES="${#FILES[@]}"
if [ "$NFILES" -eq 0 ]; then
  echo "[parallel-test] no test files matched" >&2
  exit 2
fi
# Never spin up more shards than files.
if [ "$JOBS" -gt "$NFILES" ]; then JOBS="$NFILES"; fi

rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"

# --- balance files across shards by descending size (LPT heuristic) ---------
# Heavy files (e.g. the float/stdlib suites) dominate wall time, so greedily
# drop each next-largest file into the currently-lightest shard.
declare -a SHARD_LOAD   # accumulated bytes per shard
declare -a SHARD_FILES  # newline-joined file list per shard
for ((i=0; i<JOBS; i++)); do SHARD_LOAD[i]=0; SHARD_FILES[i]=""; done

while IFS= read -r line; do
  sz="${line%% *}"
  f="${line#* }"
  # pick lightest shard
  min=0
  for ((i=1; i<JOBS; i++)); do
    if (( SHARD_LOAD[i] < SHARD_LOAD[min] )); then min="$i"; fi
  done
  SHARD_LOAD[min]=$(( SHARD_LOAD[min] + sz ))
  SHARD_FILES[min]="${SHARD_FILES[min]}${f}"$'\n'
done < <(
  for f in "${FILES[@]}"; do
    sz=$(stat -c '%s' "$f" 2>/dev/null || stat -f '%z' "$f" 2>/dev/null || echo 0)
    printf '%s %s\n' "$sz" "$f"
  done | sort -rn
)

echo "[parallel-test] $NFILES files across $JOBS shards (EMACS=$EMACS)"
T0=$(date +%s.%N)

# --- launch one emacs --batch per shard -------------------------------------
declare -a PIDS
for ((i=0; i<JOBS; i++)); do
  [ -z "${SHARD_FILES[i]}" ] && continue
  load_args=()
  while IFS= read -r f; do
    [ -n "$f" ] && load_args+=(-l "$f")
  done <<< "${SHARD_FILES[i]}"
  log="$OUT_DIR/shard-$i.log"
  (
    "$EMACS" --batch -Q "${LOAD_DIRS[@]}" \
      --eval '(setq load-prefer-newer t)' \
      -l ert "${load_args[@]}" \
      --eval "(ert-run-tests-batch-and-exit '$SELECTOR)" \
      > "$log" 2>&1
    echo "$?" > "$OUT_DIR/shard-$i.rc"
  ) &
  PIDS+=("$!")
done

wait

T1=$(date +%s.%N)

# --- aggregate --------------------------------------------------------------
total_ran=0 total_unexpected=0 total_expected_fail=0 total_skipped=0 failed_shards=0
for ((i=0; i<JOBS; i++)); do
  [ -z "${SHARD_FILES[i]}" ] && continue
  rc="$(cat "$OUT_DIR/shard-$i.rc" 2>/dev/null || echo 1)"
  log="$OUT_DIR/shard-$i.log"
  # The single-line ert summary carries ran/unexpected/skipped; ert prints
  # "N expected failures" on its OWN line BELOW it, so parse that separately
  # from the whole log (not the Ran line).
  summ="$(grep -E 'Ran [0-9]+ tests' "$log" | tail -1)"
  ran="$(sed -nE 's/.*Ran ([0-9]+) tests.*/\1/p' <<< "$summ")"
  unexp="$(sed -nE 's/.*, ([0-9]+) unexpected.*/\1/p' <<< "$summ")"
  skip="$(sed -nE 's/.*, ([0-9]+) skipped.*/\1/p' <<< "$summ")"
  expf="$(sed -nE 's/^([0-9]+) expected failures$/\1/p' "$log" | tail -1)"
  ran="${ran:-0}"; unexp="${unexp:-0}"; skip="${skip:-0}"; expf="${expf:-0}"
  total_ran=$(( total_ran + ran ))
  total_unexpected=$(( total_unexpected + unexp ))
  total_expected_fail=$(( total_expected_fail + expf ))
  total_skipped=$(( total_skipped + skip ))
  # A non-empty shard that exits 0 but produced NO ert summary line has
  # silently under-run (e.g. a test called `kill-emacs 0', or a SELECTOR
  # matched nothing).  Treat that as a failure so the "every assigned test
  # actually ran" gate the header promises is genuinely enforced — not just
  # the hard-crash (rc != 0) path.
  no_summary=0
  [ -z "$summ" ] && no_summary=1
  if [ "$rc" != "0" ] || [ "$unexp" != "0" ] || [ "$no_summary" = "1" ]; then
    failed_shards=$(( failed_shards + 1 ))
    echo "[parallel-test] SHARD $i FAILED (rc=$rc, unexpected=$unexp, no-summary=$no_summary):"
    grep -E 'FAILED|ERROR|Ran [0-9]+ tests' "$log" | tail -20 | sed 's/^/    /'
  fi
done

WALL=$(awk "BEGIN{printf \"%.1f\", $T1-$T0}")
echo "---------------------------------------------------------------"
printf '[parallel-test] ran %d tests, %d unexpected, %d skipped, %d expected-failures in %ss across %d shards\n' \
  "$total_ran" "$total_unexpected" "$total_skipped" "$total_expected_fail" "$WALL" "$JOBS"

if [ "$failed_shards" -gt 0 ]; then
  echo "[parallel-test] RESULT: FAIL ($failed_shards shard(s) failed)"
  exit 1
fi
echo "[parallel-test] RESULT: PASS"
exit 0
