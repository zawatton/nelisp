#!/usr/bin/env bash
# run-tests-parallel.sh — parallel per-file host ERT runner.
#
# Each test file runs in its OWN `emacs --batch' process, JOBS at a time.
# Loading every test file into one process made the result depend on load
# order: a later file could rely on a library that only an earlier file
# had loaded by path, and test files that redefine functions (or fake
# standalone state) broke later files' load-time checks.  One process per
# file removes both kinds of interference; the cost is one Emacs startup
# plus the file's own `require' graph per file.
#
# The runner fails the verdict for any file that exits non-zero, reports
# unexpected results, or exits without an ert summary line (a crash at
# load time, a `kill-emacs', or a SELECTOR that matched nothing).
#
# Usage:
#   tools/run-tests-parallel.sh            # JOBS = nproc
#   JOBS=8 tools/run-tests-parallel.sh     # explicit process count
#   tools/run-tests-parallel.sh test/nelisp-eval-test.el ...
#                                          # only the given files
#   EMACS=/path/to/emacs tools/run-tests-parallel.sh
#   SELECTOR='"pattern"' tools/run-tests-parallel.sh
#                                          # ert string/regexp selector per file
#   TEST_TIMEOUT=900 tools/run-tests-parallel.sh
#                                          # per-file timeout in seconds (default 600)
#
# Per-file logs go to OUT_DIR (default target/test-parallel).
# Exit 0 iff every file exits 0 with an ert summary and 0 unexpected results.

set -uo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

EMACS="${EMACS:-emacs}"
JOBS="${JOBS:-$(nproc 2>/dev/null || echo 4)}"
OUT_DIR="${OUT_DIR:-target/test-parallel}"
SELECTOR="${SELECTOR:-t}"
TEST_TIMEOUT="${TEST_TIMEOUT:-600}"

# Remove stale untracked .elc once up front (mirrors `make clean') so none
# can shadow a just-edited source while `load-prefer-newer' is on.  Tracked
# .elc files are test fixtures and stay.
tracked=" $(git ls-files -- '*.elc' 2>/dev/null | sed 's|^|./|' | tr '\n' ' ') "
find . -name '*.elc' -type f | while IFS= read -r f; do
  case "$tracked" in *" $f "*) ;; *) rm -f "$f" ;; esac
done

# --- load paths: keep in lock-step with the Makefile `test-one' recipe -------
# scripts/ and tools/ come last so they never shadow a library of the same
# name in lisp/, src/ or a package.
LOAD_DIRS=(-L lisp -L src -L test -L bench)
for d in $(ls -d packages/*/src 2>/dev/null | sort); do LOAD_DIRS+=(-L "$d"); done
for d in $(ls -d packages/*/test 2>/dev/null | sort); do LOAD_DIRS+=(-L "$d"); done
LOAD_DIRS+=(-L scripts -L tools)

# --- test file set: same glob as the Makefile, minus the soak test ----------
if [ "$#" -gt 0 ]; then
  mapfile -t FILES < <(printf '%s\n' "$@")
else
  mapfile -t FILES < <(
    { ls test/nelisp*-test.el packages/*/test/nelisp*-test.el \
         packages/*/test/nl-*-test.el 2>/dev/null; } \
      | grep -v 'test/nelisp-worker-soak-test.el' \
      | sort
  )
fi
NFILES="${#FILES[@]}"
if [ "$NFILES" -eq 0 ]; then
  echo "[parallel-test] no test files matched" >&2
  exit 2
fi
[ "$JOBS" -gt "$NFILES" ] && JOBS="$NFILES"

rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"
RESULTS="$OUT_DIR/results.tsv"
: > "$RESULTS"

echo "[parallel-test] $NFILES files, $JOBS at a time (EMACS=$EMACS)"
T0=$(date +%s.%N)

run_one() {
  local f=$1 log rc summ ran unexp skip expf
  log="$OUT_DIR/$(echo "$f" | tr '/' '_' | sed 's/\.el$//').log"
  timeout "$TEST_TIMEOUT" "$EMACS" --batch -Q "${LOAD_DIRS[@]}" \
    --eval '(setq load-prefer-newer t)' \
    -l ert -l "$f" \
    --eval "(ert-run-tests-batch-and-exit '$SELECTOR)" \
    > "$log" 2>&1
  rc=$?
  # ert prints "N expected failures" on its own line below the summary.
  summ="$(grep -aE '^(Aborted: )?Ran [0-9]+ tests?' "$log" | tail -1)"
  ran="$(sed -nE 's/.*Ran ([0-9]+) tests?.*/\1/p' <<< "$summ")"
  unexp="$(sed -nE 's/.*, ([0-9]+) unexpected.*/\1/p' <<< "$summ")"
  skip="$(sed -nE 's/.*, ([0-9]+) skipped.*/\1/p' <<< "$summ")"
  expf="$(sed -nE 's/^([0-9]+) expected failures?$/\1/p' "$log" | tail -1)"
  # One short printf per file keeps concurrent appends whole.
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$f" "$rc" "${ran:-0}" "${unexp:-0}" \
    "${skip:-0}" "${expf:-0}" "$([ -n "$summ" ] && echo 1 || echo 0)" "$log" >> "$RESULTS"
}

for f in "${FILES[@]}"; do
  while [ "$(jobs -rp | wc -l)" -ge "$JOBS" ]; do
    wait -n
  done
  run_one "$f" &
done
wait

T1=$(date +%s.%N)

# --- aggregate --------------------------------------------------------------
total_ran=0 total_unexpected=0 total_expected_fail=0 total_skipped=0 failed_files=0
while IFS=$'\t' read -r f rc ran unexp skip expf has_summ log; do
  total_ran=$(( total_ran + ran ))
  total_unexpected=$(( total_unexpected + unexp ))
  total_skipped=$(( total_skipped + skip ))
  total_expected_fail=$(( total_expected_fail + expf ))
  if [ "$rc" != "0" ] || [ "$unexp" != "0" ] || [ "$has_summ" = "0" ]; then
    failed_files=$(( failed_files + 1 ))
    if [ "$has_summ" = "0" ]; then
      echo "[parallel-test] ERROR $f (rc=$rc, no ert summary; see $log)"
    else
      echo "[parallel-test] FAIL  $f (rc=$rc, $unexp unexpected of $ran; see $log)"
    fi
  fi
done < <(sort "$RESULTS")

WALL=$(awk "BEGIN{printf \"%.1f\", $T1-$T0}")
echo "---------------------------------------------------------------"
printf '[parallel-test] ran %d tests, %d unexpected, %d skipped, %d expected-failures in %ss across %d files\n' \
  "$total_ran" "$total_unexpected" "$total_skipped" "$total_expected_fail" "$WALL" "$NFILES"

if [ "$failed_files" -gt 0 ]; then
  echo "[parallel-test] RESULT: FAIL ($failed_files of $NFILES file(s) failed)"
  exit 1
fi
echo "[parallel-test] RESULT: PASS"
exit 0
