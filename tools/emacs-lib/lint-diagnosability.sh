#!/usr/bin/env bash
# lint-diagnosability.sh — flag patterns that make a failure hard to diagnose.
#
# Not a correctness linter.  Every pattern here is legal, compiles, and often
# works; each one cost real diagnosis time in this repo, and each is greppable.
# The point is to stop the pattern SPREADING, not to fail the build on the
# instances that already exist -- so the existing count is baselined and only
# growth is an error.
#
#   lint-diagnosability.sh            # report; non-zero if a rule grew
#   lint-diagnosability.sh --baseline # rewrite the baseline (review the diff!)
set -uo pipefail
export LC_ALL=C
cd "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

BASELINE="tools/diagnosability-baseline.tsv"
MODE="${1:-report}"

# NAME | WHAT IT COSTS | COUNT COMMAND
#
# `bare-timeout': `timeout N cmd' kills the command and leaves nothing.  The
# caller sees an empty log and non-zero status, which reads as a crash.
# magit-load-smoke was read as a crash for months this way: measured
# 2026-08-12 the load needs 1188 s against a 900 s ceiling, so every run died
# silently with no error because no error had occurred.  Use
# tools/run-with-deadline.sh, which says TIMEOUT/SIGNAL/EXIT in the caller's
# own voice and warns when a passing run is near its budget.
count_bare_timeout() {
  { grep -cE '^\s+.*\btimeout \$\(' Makefile 2>/dev/null || true; } | head -1
}

# `guard-mismatch': a probe that tests one name but gates the definition of
# several.  src/nelisp-emacs-magit-bridge.el tests `make-sparse-keymap' and on
# that evidence skips defining `make-keymap', `make-sparse-keymap' AND
# `suppress-keymap'.  A guard must test what it gates.
count_guard_mismatch() {
  { grep -cE '\(unless probe$' src/nelisp-emacs-magit-bridge.el 2>/dev/null || true; } | head -1
}

# `alloc-in-diagnostic': a diagnostic that allocates changes the thing it is
# measuring.  On 2026-08-12 a probe that called `(make-keymap)' at eleven part
# boundaries flipped the failure it was there to observe from a named
# void-function into a silent abort, and a per-miss name printer added 2,778
# lines to one run and slowed it enough to matter.  Diagnostics that fire on a
# NORMAL path must not allocate; ones that fire only on an already-detected
# fault may.  Counted as bridge-level tracers that call an allocating builtin.
count_alloc_in_diagnostic() {
  { grep -cE 'nelisp--write-stdout-bytes.*\((make-keymap|make-vector|make-list|make-hash-table)' \
      src/nelisp-emacs-magit-bridge.el 2>/dev/null || true; } | head -1
}

# `name-keyed-load': `load' changes what it evaluates based on the file's NAME,
# not just its bytes.  src/emacs-load.el keys three behaviours on the basename:
# a `cc-' prefix skips the artifact/native-compile path entirely, a `cc-defs.el'
# suffix rewrites the source text, and a `cc-' prefix injects a function and
# wraps the source in a push/pop pair.  So renaming a file changes both whether
# it is compiled and what text runs.
#
# On 2026-08-13 that property invalidated a whole bisection: part2 of the magit
# bundle was re-serialised into /tmp under a different name to localise a
# memory defect, all 400 forms loaded clean, and only the k=400 control -- the
# same forms, still under the new name -- showed the harness had stopped
# reproducing the defect at all.  Without the control the run would have
# reported a culprit range that meant nothing.
#
# The rule does not forbid name-keyed behaviour; it stops it spreading
# silently.  Each new branch is one more way that "same bytes" stops meaning
# "same evaluation", and every future harness that copies a file to a scratch
# name inherits the risk.
count_name_keyed_load() {
  { grep -cE '\((string-prefix-p|string-suffix-p) "[^"]+" (base|\(file-name)|\(string= \(substring resolved' \
      src/emacs-load.el 2>/dev/null || true; } | head -1
}

# `basename-routing': a shell path chosen by the basename of a binary.
# `nelisp_driver_kind' in bin/nemacs returns standalone-reader only for a file
# literally named `nelisp' (or `nelisp-standalone-reader'); anything else falls
# through to `cli', which silently skips the cold-load route and takes the
# 85-minute artifact route instead -- and that route prints nothing, so it is
# indistinguishable from a hang.  Measured 2026-08-13: copying the binary to
# `nelisp-fix' for an A/B cost 20 minutes before the routing was noticed.
# Route on content or an explicit flag, not on what someone called the file.
count_basename_routing() {
  { grep -cE 'case "\$\(basename' bin/nemacs 2>/dev/null || true; } | head -1
}

RULES="bare-timeout guard-mismatch alloc-in-diagnostic name-keyed-load basename-routing"

declare -A now
for r in $RULES; do now[$r]=$("count_${r//-/_}"); done

if [ "$MODE" = "--baseline" ]; then
  : > "$BASELINE"
  for r in $RULES; do printf '%s\t%s\n' "$r" "${now[$r]}" >> "$BASELINE"; done
  echo "[lint] baseline written to $BASELINE"
  cat "$BASELINE"
  exit 0
fi

[ -r "$BASELINE" ] || { echo "[lint] no baseline; run --baseline first" >&2; exit 2; }

fails=0
printf '%-18s %8s %8s  %s\n' RULE NOW BASE STATUS
while IFS=$'\t' read -r rule base; do
  [ -n "$rule" ] || continue
  n="${now[$rule]:-0}"
  if [ "$n" -gt "$base" ]; then
    status="GREW +$((n - base))"; fails=$((fails + 1))
  elif [ "$n" -lt "$base" ]; then
    status="improved -$((base - n)) (re-baseline)"
  else
    status="held"
  fi
  printf '%-18s %8s %8s  %s\n' "$rule" "$n" "$base" "$status"
done < "$BASELINE"

echo
if [ "$fails" -eq 0 ]; then
  echo "[lint] PASS -- no diagnosability rule grew"
else
  echo "[lint] FAIL -- $fails rule(s) grew.  These patterns are legal and will work;"
  echo "[lint] they cost days when they fail.  See the comment on each rule."
fi
exit $((fails > 0))
