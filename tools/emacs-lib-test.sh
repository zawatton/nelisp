#!/bin/sh
# Run the emacs-lib ERT files on host Emacs, one file per process, and
# compare the set of failing tests with the recorded known reds.
#
# Usage: tools/emacs-lib-test.sh [--update | --compare-only]
#   (default)       exit 0 iff the failing tests are exactly the known reds
#   --update        rewrite the known-reds file from this run
#   --compare-only  skip the run; compare the last run's failing.txt
#
# The files are packages/emacs-lib-*/test/*-test.el, except nelisp*-test.el,
# which the root Makefile's ERT already runs.  The known reds are in
# tools/emacs-lib-test-known-reds.txt, one test name per line.  A new
# failure fails the run, and so does a known red that now passes: remove it
# from the file.  EMACS (default emacs) selects the host Emacs; logs go to
# EMACS_LIB_TEST_LOGS (default build/emacs-lib-test).
set -u
# Byte order for sort/comm, whatever the caller's locale.
LC_ALL=C
export LC_ALL
root=$(cd "$(dirname "$0")/.." && pwd) || exit 2
cd "$root" || exit 2
known=tools/emacs-lib-test-known-reds.txt
logs=${EMACS_LIB_TEST_LOGS:-build/emacs-lib-test}
emacs=${EMACS:-emacs}
mkdir -p "$logs" || exit 2
lp=$(for d in packages/*/src packages/*/test; do printf -- '-L %s ' "$d"; done)
actual="$logs/failing.txt"
files=0 broken=0
if [ "${1:-}" = --compare-only ]; then
    [ -r "$actual" ] || { echo "no previous run in $logs" >&2; exit 2; }
    files=prev
else
: > "$actual"
for f in packages/emacs-lib-*/test/*-test.el; do
    case "${f##*/}" in nelisp*-test.el) continue ;; esac
    files=$((files + 1))
    log="$logs/${f##*/}.log"
    # shellcheck disable=SC2086
    "$emacs" --batch -Q -l json -L lisp -L src -L test $lp -l ert -l "$f" \
        -f ert-run-tests-batch-and-exit </dev/null >"$log" 2>&1
    rc=$?
    if ! grep -aq '^Ran ' "$log"; then
        echo "NO-RESULT $f (rc=$rc, see $log)" >&2
        broken=$((broken + 1))
        continue
    fi
    # The closing summary lists each failing test as "   FAILED  NAME";
    # the progress lines carry an "N/M" counter before the name.
    grep -a '^   FAILED  [^ ]' "$log" | awk 'NF == 2 && $2 !~ /^[0-9]+\/[0-9]+$/ {print $2}' >> "$actual"
done
sort -u -o "$actual" "$actual"
fi
if [ "${1:-}" = --update ]; then
    cp "$actual" "$known"
    echo "wrote $known ($(wc -l < "$known") tests)"
    exit 0
fi
new=$(comm -13 "$known" "$actual")
fixed=$(comm -23 "$known" "$actual")
status=0
for t in $new; do echo "NEW FAILURE $t" >&2; status=1; done
for t in $fixed; do echo "NOW PASSES (remove from $known) $t" >&2; status=1; done
[ "$broken" -gt 0 ] && status=1
echo "emacs-lib-test: files=$files failing=$(wc -l < "$actual") known=$(wc -l < "$known") broken=$broken"
exit $status
