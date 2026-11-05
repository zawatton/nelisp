#!/usr/bin/env bash
# ert-per-file.sh -- run each ERT test file in its own Emacs process.
#
# Usage: ert-per-file.sh EMACS-COMMAND... -- TEST-FILE...
#
# Each TEST-FILE runs as
#   EMACS-COMMAND... -l TEST-FILE -f ert-run-tests-batch-and-exit
# in a fresh process.  Several test files reload library sources under a
# faked standalone state (fmakunbound, a faked `fboundp'); in a single
# process that state leaks into later files and causes spurious failures,
# so the umbrella `make test' runs files separately.
#
# Environment:
#   TEST_JOBS     parallel processes (default 4)
#   TEST_TIMEOUT  per-file timeout in seconds (default 600)
#   TEST_LOG_DIR  per-file logs (default build/ert-per-file)
#
# Prints one line per file that is not clean, then the totals.  Exits 0
# only when every file exits 0.

cmd=()
while [ $# -gt 0 ] && [ "$1" != "--" ]; do
  cmd+=("$1"); shift
done
if [ $# -eq 0 ] || [ ${#cmd[@]} -eq 0 ]; then
  echo "usage: $0 EMACS-COMMAND... -- TEST-FILE..." >&2
  exit 2
fi
shift

jobs_max=${TEST_JOBS:-4}
timeout_s=${TEST_TIMEOUT:-600}
log_dir=${TEST_LOG_DIR:-build/ert-per-file}
mkdir -p "$log_dir" || exit 2
result_file="$log_dir/results.tsv"
: > "$result_file" || exit 2

run_one() {
  local f=$1 log rc summary
  log="$log_dir/$(basename "$f" .el).log"
  timeout "$timeout_s" "${cmd[@]}" -l "$f" -f ert-run-tests-batch-and-exit \
    > "$log" 2>&1
  rc=$?
  summary=$(grep -aE '^(Aborted: )?Ran [0-9]+ tests?' "$log" | tail -1)
  # One line per file; a single short printf keeps concurrent appends whole.
  printf '%s\t%s\t%s\t%s\n' "$f" "$rc" "$summary" "$log" >> "$result_file"
}

start=$(date +%s)
for f in "$@"; do
  while [ "$(jobs -rp | wc -l)" -ge "$jobs_max" ]; do
    wait -n
  done
  run_one "$f" &
done
wait

awk -F'\t' -v secs=$(( $(date +%s) - start )) '
  {
    files++
    n_ran = n_exp = n_unexp = n_skip = 0
    if (match($3, /Ran [0-9]+/)) n_ran = substr($3, RSTART + 4, RLENGTH - 4) + 0
    if (match($3, /[0-9]+ results? as expected/)) n_exp = substr($3, RSTART) + 0
    if (match($3, /[0-9]+ unexpected/)) n_unexp = substr($3, RSTART) + 0
    if (match($3, /[0-9]+ skipped/)) n_skip = substr($3, RSTART) + 0
    tr += n_ran; te += n_exp; tu += n_unexp; ts += n_skip
    if ($2 != 0) {
      bad++
      if ($3 == "")
        printf "ERROR  %s (exit %s, no ERT summary; see %s)\n", $1, $2, $4
      else
        printf "FAIL   %s (%d unexpected of %d; see %s)\n", $1, n_unexp, n_ran, $4
    }
  }
  END {
    printf "\nFiles: %d, %d not clean.  Ran %d tests, %d as expected, %d unexpected, %d skipped (%d s)\n",
      files, bad, tr, te, tu, ts, secs
  }' <(sort "$result_file")

! awk -F'\t' '$2 != 0 { found = 1 } END { exit !found }' "$result_file"
