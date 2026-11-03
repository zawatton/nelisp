#!/usr/bin/env bash
# run-with-deadline.sh — run a command under a deadline and SAY how it ended.
#
# WHY
#
# `timeout N cmd' kills the command and leaves nothing behind.  The caller sees
# an empty log and a non-zero status, which reads exactly like a crash.
#
# `magit-load-smoke' was read as a crash for months because of this.  Measured
# 2026-08-12: the load needs 1188 s, the target allowed 900 s, and the recipe
# captured the output of the killed run and grepped it for a PASS line.  No
# error was printed because no error occurred -- the work simply had not
# finished.  Sessions went looking for a memory-exhaustion or printer bug that
# was never there (peak RSS measured at 374 MB).
#
# A deadline is a decision the caller made.  When it fires, the caller should
# say so in its own voice.
#
# USAGE
#
#   run-with-deadline.sh SECONDS [--label NAME] -- CMD...
#
# Prints one summary line to stderr and exits with the command's status
# (124 on deadline, as `timeout' does):
#
#   [magit-load] TIMEOUT after 900s -- the command was still running (deadline, not a crash)
#   [magit-load] SIGNAL 11 (SIGSEGV) after 812s
#   [magit-load] EXIT 1 after 1188s
#   [magit-load] OK after 134s
#
# The elapsed time is printed on every outcome, including success: a target
# that passes today at 890 s against a 900 s deadline is a failure scheduled
# for next week, and the number is the only warning you get.
set -uo pipefail
export LC_ALL=C

LABEL="deadline"
SECS="${1:?usage: run-with-deadline.sh SECONDS [--label NAME] -- CMD...}"
shift
while [ "$#" -gt 0 ]; do
  case "$1" in
    --label) LABEL="$2"; shift 2 ;;
    --) shift; break ;;
    *) break ;;
  esac
done
[ "$#" -gt 0 ] || { echo "run-with-deadline: no command" >&2; exit 2; }

start=$(date +%s)
timeout "$SECS" "$@"
rc=$?
elapsed=$(( $(date +%s) - start ))

case "$rc" in
  0)
    printf '[%s] OK after %ss (deadline %ss)\n' "$LABEL" "$elapsed" "$SECS" >&2
    # A pass that used most of its budget is the last quiet warning before a
    # failure that will look like a crash.
    awk -v e="$elapsed" -v d="${SECS%s}" 'BEGIN{ if (d>0 && e > d*0.8)
      printf "[%s] NOTE: used %d%% of the deadline -- raise it before it fires\n", "'"$LABEL"'", (e*100)/d }' >&2
    ;;
  124)
    printf '[%s] TIMEOUT after %ss -- still running at the deadline, NOT a crash\n' \
      "$LABEL" "$elapsed" >&2
    printf '[%s] the command was killed; any missing output is missing because it never ran\n' \
      "$LABEL" >&2
    ;;
  *)
    if [ "$rc" -gt 128 ]; then
      sig=$((rc - 128))
      name=$(kill -l "$sig" 2>/dev/null || echo "?")
      printf '[%s] SIGNAL %s (SIG%s) after %ss\n' "$LABEL" "$sig" "$name" "$elapsed" >&2
    else
      printf '[%s] EXIT %s after %ss\n' "$LABEL" "$rc" "$elapsed" >&2
    fi
    ;;
esac
exit "$rc"
