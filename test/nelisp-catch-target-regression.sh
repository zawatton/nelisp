#!/usr/bin/env bash
# GNU parity and real evaluator-worker coverage for catch target lifetime.
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"
reader=${NELISP_BIN:-$root/target/nelisp}
host=${EMACS:-emacs}
out=${CATCH_REGRESSION_OUT:-target/progress/catch-target-regression}
mkdir -p "$out"

timeout 30 "$host" -Q --batch -l test/nelisp-catch-target-regression.el \
  > "$out/host.out" 2> "$out/host.err"
test ! -s "$out/host.err"
test "$(wc -l < "$out/host.out")" -eq 26
# Multiple CLI actions suppress the documented implicit result print without
# filtering stdout; every explicit regression row still has to match GNU.
timeout 30 "$reader" --load "$root/test/nelisp-catch-target-regression.el" --eval nil \
  > "$out/reader.out" 2> "$out/reader.err"
test ! -s "$out/reader.err"
diff -u "$out/host.out" "$out/reader.out" > "$out/parity.diff"

timeout 30 "$reader" --load "$root/test/nelisp-catch-worker-regression.el" --eval nil \
  > "$out/worker.out" 2> "$out/worker.err"
test ! -s "$out/worker.err"
cat > "$out/worker.expected" <<'EXPECTED'
41
42
43
WORKER-CATCH-PASS
EXPECTED
diff -u "$out/worker.expected" "$out/worker.out" > "$out/worker.diff"

status=0
timeout 30 "$reader" --eval "(throw 'missing (list 7))" \
  > "$out/uncaught.out" 2> "$out/uncaught.err" || status=$?
test "$status" -eq 1
test ! -s "$out/uncaught.out"
rg -q 'no-catch: \(missing \(7\)\)' "$out/uncaught.err"
printf 'CATCH-TARGET-PASS GNU=26 worker=3 parent=1 uncaught=1\n'
