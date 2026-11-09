#!/usr/bin/env bash
# Run the whole ERT suite with the per-file runner (one Emacs per file, as
# the CI ert jobs do since e3470413f) and pass iff every file ran to its
# end: a TOTALS line with ran > 0 and error-files=0.  Failing tests do not
# count here (S3 of tools/ai/s1-9-ert-progress.org lists them); a file that
# crashed, timed out or never printed its summary does.
set -u
root=$(git rev-parse --show-toplevel) || exit 2
cd "$root" || exit 2
out=target/s1-9-ert/complete
log=target/s1-9-ert/ert-complete.log
mkdir -p target/s1-9-ert
OUT_DIR=$out ./tools/run-tests-parallel.sh > "$log" 2>&1
rc=$?
totals=$(grep -E '^\[parallel-test\] TOTALS ' "$log" | tail -1)
echo "ert-complete: runner exit $rc; ${totals:-no TOTALS line} (log $log)"
case "$totals" in
  *" ran=0 "*|"") exit 1 ;;
  *" error-files=0 "*) exit 0 ;;
  *) exit 1 ;;
esac
