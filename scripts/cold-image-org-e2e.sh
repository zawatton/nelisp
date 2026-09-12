#!/usr/bin/env bash
# cold-image-org-e2e.sh — multi-chunk cold-load end-to-end timing + faithfulness harness
#
# Reproduces, from a checked-in ~60-file Org vendor chain file list, the
# Doc 156 Increments MC-1/MC-2 (multi-chunk arena streaming dump +
# multi-chunk boot-INSTALL) acceptance flow against THIS worktree's
# ./target/nelisp:
#
#   (a) host-side .repl generation for the file chain via nelisp-emacs-lib's
#       scripts/vendor-repl-standalone-replay.el `vendor-repl-standalone--write-input'
#       (READ-ONLY use of that sibling repo -- this script never writes into it:
#       no cache directory is configured, and all generated output goes under
#       a private $WORKDIR),
#   (b) a full .repl replay-load of the chain into this worktree's
#       ./target/nelisp, followed by an in-process arena dump
#       (`nelisp--arena-dump-image-stream') to a multi-chunk-coalesced image,
#   (c) a cold boot of a FRESH process via `--cold-load-from IMG', evaluating
#       the same faithfulness conjuncts the replay-loaded process saw,
#   (d) wall-clock timing for all three phases (median of RUNS repetitions).
#
# Usage:
#   scripts/cold-image-org-e2e.sh [RUNS]
#     RUNS   number of repetitions per phase for the median (default 3).
#
# Env overrides:
#   NELISP_LIB_ROOT   sibling nelisp-emacs-lib checkout (default: the
#                     standard co-location used during this repo's
#                     development, see below)
#   NELISP_E2E_KEEP   when "1", keep $WORKDIR (images, .repl, logs) instead
#                     of deleting it on exit -- useful for post-mortem.
#   E2E_BOOTSTRAP_REPL  override the bootstrap-repl input fed to host-side
#                     .repl generation (default: this worktree's own
#                     $NELISP_LIB_ROOT/build/nemacs-bootstrap.repl, which
#                     already matches nelisp-emacs-lib's gate default).
#   E2E_PRELUDE       override the stdlib prelude fed to host-side .repl
#                     generation (default: this worktree's own
#                     scripts/nelisp-stdlib-prelude.el).  Set to
#                     $NELISP_LIB_ROOT/vendor/nelisp/scripts/nelisp-stdlib-prelude.el
#                     to reproduce nelisp-emacs-lib's
#                     `make diagnose-vendor-repl-replay' gate prelude
#                     instead -- read-only from that checkout, never
#                     written to.  This is the prelude under which org.el's
#                     `define-derived-mode org-mode' has been observed to
#                     actually install org-mode (Doc 156 §7), at the cost
#                     of a much slower replay-load phase; see
#                     E2E_LOAD_TIMEOUT.
#   E2E_LOAD_TIMEOUT  seconds allotted to the replay-load phase's `timeout'
#                     wrapper (default 180).  Raise this when using a
#                     slower prelude/bootstrap-repl input (E2E_PRELUDE).
#
# Requires a host `emacs` (batch mode, GNU Emacs) able to load
# nelisp-emacs-lib/scripts/*.el, and this worktree's ./target/nelisp already
# built (`make standalone-reader`).
#
# NOTE on pkill: this script's own invocation text necessarily contains the
# substring "target/nelisp"; `pkill -f` matches full command lines, so a
# naive `pkill -f '.../target/nelisp'` run from a shell whose own argv
# contains that same literal text will match (and kill) ITSELF.  The
# bracket idiom below (`[/]path/...', built from $NELISP_BIN at runtime)
# avoids that self-match while still matching the real reader process's
# argv.  It is ALSO deliberately scoped to THIS checkout's own binary path
# rather than a bare `target/nelisp', so it never touches a reader running
# from a sibling worktree or from nelisp-emacs-lib's
# vendor/nelisp/target/nelisp mirror.  (An earlier revision hardcoded the
# `wt-coldload' worktree path here, which silently matched nothing once the
# branch merged to main and the script ran from other checkouts.)

set -uo pipefail

# Force a dot-decimal locale: `bc -l`/`printf %f` disagree with the caller's
# locale (e.g. a comma radix character) otherwise, and bash's printf builtin
# then rejects bc's own output as "invalid number".
export LC_ALL=C LC_NUMERIC=C

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

RUNS="${1:-3}"
NELISP_LIB_ROOT="${NELISP_LIB_ROOT:-/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib}"
NELISP_BIN="$REPO_ROOT/target/nelisp"
# Default bootstrap-repl/prelude reproduce this worktree's own environment
# (fast replay, org-mode not installed -- see Doc 156 §7).  E2E_BOOTSTRAP_REPL
# / E2E_PRELUDE opt in to substituting the *gate's* inputs instead (read-only
# from the nelisp-emacs-lib checkout -- e.g.
# "$NELISP_LIB_ROOT/vendor/nelisp/scripts/nelisp-stdlib-prelude.el", the
# vendored prelude copy `make diagnose-vendor-repl-replay' uses by default via
# VENDOR_REPL_PRELUDE/VENDOR_LOAD_PRELUDE), to reproduce the gate's slower but
# org-mode-installing load.  Unset (default): behavior is unchanged.
BOOTSTRAP_REPL="${E2E_BOOTSTRAP_REPL:-$NELISP_LIB_ROOT/build/nemacs-bootstrap.repl}"
PRELUDE="${E2E_PRELUDE:-$REPO_ROOT/scripts/nelisp-stdlib-prelude.el}"
FILE_LIST_REL="$REPO_ROOT/scripts/cold-image-org-e2e-files.txt"
# Replay-load phase timeout: the default (this worktree's own prelude) loads
# the 60-file chain in ~10s; E2E_PRELUDE=<gate prelude> above is known to take
# ~170-250s longer (org.el's `define-derived-mode org-mode' form alone).
# Override via E2E_LOAD_TIMEOUT when using a slower prelude/bootstrap input.
LOAD_TIMEOUT="${E2E_LOAD_TIMEOUT:-180}"

for f in "$NELISP_BIN" "$BOOTSTRAP_REPL" "$PRELUDE" "$FILE_LIST_REL"; do
  if [ ! -r "$f" ]; then
    echo "[cold-image-org-e2e] missing required input: $f" >&2
    exit 1
  fi
done
if [ ! -x "$NELISP_BIN" ]; then
  echo "[cold-image-org-e2e] $NELISP_BIN is not executable -- run 'make standalone-reader' first" >&2
  exit 1
fi

# Self-match-safe pkill pattern for THIS checkout's reader binary: bracket
# the first character so this script's own argv never matches (see NOTE
# above).  $NELISP_BIN is an absolute path starting with "/".
NELISP_BIN_PAT="[${NELISP_BIN:0:1}]${NELISP_BIN:1}"

# `${TMPDIR:-/tmp}', like every other temp path in this repository: this
# harness writes a multi-hundred-MB image plus logs, and /tmp is a small
# tmpfs on some machines.  Hardcoding it made the run fail as
# "No space left on device" inside the cold boot, which then surfaced as a
# SIGSEGV from the binary under test -- a disk problem wearing a runtime
# defect's clothes (observed 2026-09-12 on a full 8 GB /tmp).
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/cold-image-org-e2e.XXXXXX")"
cleanup() {
  pkill -9 -f "$NELISP_BIN_PAT --repl" 2>/dev/null
  pkill -9 -f "$NELISP_BIN_PAT --cold-load-from" 2>/dev/null
  if [ "${NELISP_E2E_KEEP:-0}" != "1" ]; then
    rm -rf "$WORKDIR"
  else
    echo "[cold-image-org-e2e] kept workdir: $WORKDIR"
  fi
}
trap cleanup EXIT

echo "[cold-image-org-e2e] NELISP_BIN=$NELISP_BIN"
echo "[cold-image-org-e2e] NELISP_LIB_ROOT=$NELISP_LIB_ROOT"
echo "[cold-image-org-e2e] BOOTSTRAP_REPL=$BOOTSTRAP_REPL"
echo "[cold-image-org-e2e] PRELUDE=$PRELUDE"
echo "[cold-image-org-e2e] file count=$(wc -l < "$FILE_LIST_REL") RUNS=$RUNS WORKDIR=$WORKDIR LOAD_TIMEOUT=${LOAD_TIMEOUT}s"

# --- (a) host-side .repl generation (read-only use of nelisp-emacs-lib) ---
cat > "$WORKDIR/genrepl.el" <<EOF
(setq vendor-repl-standalone-repo-root "$NELISP_LIB_ROOT")
(setq vendor-repl-standalone-bootstrap-repl "$BOOTSTRAP_REPL")
(setq vendor-repl-standalone-prelude "$PRELUDE")
(let* ((rels (with-temp-buffer
               (insert-file-contents "$FILE_LIST_REL")
               (split-string (buffer-string) "[\n\r]+" t)))
       (files (mapcar (lambda (r) (expand-file-name r "$NELISP_LIB_ROOT")) rels)))
  (message "[cold-image-org-e2e] file-count=%d" (length files))
  (vendor-repl-standalone--write-input files "$WORKDIR/marker.scratch" "$WORKDIR/raw.repl"))
EOF

T0=$(date +%s%3N)
emacs -Q --batch -L "$NELISP_LIB_ROOT/scripts" \
  -l vendor-repl-standalone-replay -l "$WORKDIR/genrepl.el" \
  > "$WORKDIR/genrepl.log" 2>&1
GEN_RC=$?
T1=$(date +%s%3N)
if [ "$GEN_RC" != "0" ] || [ ! -s "$WORKDIR/raw.repl" ]; then
  echo "[cold-image-org-e2e] FAILED to generate .repl (see $WORKDIR/genrepl.log)" >&2
  cat "$WORKDIR/genrepl.log" >&2
  exit 1
fi
echo "[cold-image-org-e2e] host .repl generation: $(( (T1 - T0) ))ms, $(wc -l < "$WORKDIR/raw.repl") lines"

# Strip the library's default proof/quit tail (everything from the
# `vendor-repl-standalone-marker-file' setq onward) -- we append our own
# single-line probe/dump/timing tail below instead of relying on
# `vendor-repl-standalone-proof-form' (which is pretty-printed and designed
# for a single truthy gate, not for capturing 6 distinct conjunct values).
TAIL_LINE=$(grep -a -n 'vendor-repl-standalone-marker-file' "$WORKDIR/raw.repl" | head -1 | cut -d: -f1)
if [ -z "$TAIL_LINE" ]; then
  echo "[cold-image-org-e2e] could not find proof-tail anchor in generated .repl" >&2
  exit 1
fi
head -n "$((TAIL_LINE - 1))" "$WORKDIR/raw.repl" > "$WORKDIR/load-only.repl"

# --- workaround for a nelisp-emacs-lib .repl-generation defect (found while
# diagnosing why `org-element-parse-buffer' silently returned nil/(error nil)
# on this cold-loaded image; see FINDINGS.md).  The generated replay stream's
# own foundational bootstrap section (nemacs-bootstrap.repl's
# `src/emacs-backquote.el' chunk, replayed via this .repl) installs a
# BODY-LESS `(defmacro backquote (form))' -- and, from replaying this
# worktree's own scripts/nelisp-stdlib-prelude.el through the SAME
# generator, two more identical body-less copies -- before ANY of the 60
# org vendor files are replayed.  Root cause (verified against host Emacs):
# host Emacs's own `backquote' is `(macro . #[byte-code ...])', a compiled
# byte-code object with no Lisp-readable body, so whatever host-side
# machinery in nelisp-emacs-lib serializes "the body of this defmacro" for
# replay cannot recover one and silently emits an empty body instead of
# skipping the (re)definition or preserving semantics.  A body-less
# `defmacro' legitimately expands to nil on ANY Lisp implementation
# (including real Emacs), so this is not a bug in this reader's
# reader/evaluator/GC: replaying that hollow form simply clobbers this
# reader's own correctly-behaving, natively-prelude-baked `backquote' (and
# its real-Emacs-style `\=`' alias) with a permanent always-nil stand-in for
# the rest of the chain.  Every macro that expands via backquote/`,@' syntax
# breaks silently from that point on -- notably org-macs.el's
# `org-with-wide-buffer' and `org-no-read-only', both load-bearing for
# `org-element-org-data-parser' / `org-element-parse-buffer'.
#
# `nelisp--bq-expand' (the actual quasiquote-expansion logic `backquote'
# delegates to) is unaffected -- it is a plain function, never itself named
# "backquote", so it survives the clobber intact and is already loaded and
# correct by this point in the chain.  Re-asserting the two one-line
# `defmacro's below, once, right after the vendor chain and before this
# worktree's own arena dump, is therefore sufficient to restore correct
# backquote/,@ semantics for the image that gets dumped and cold-loaded --
# with no change to this reader's own reader/evaluator/GC and no edit to
# nelisp-emacs-lib (whose replay generator owns the actual defect and is out
# of scope here; see FINDINGS.md for the full repro/diagnosis).
cat >> "$WORKDIR/load-only.repl" <<'BQFIXEOF'
(defmacro backquote (form) (nelisp--bq-expand form))
(defmacro \` (form) (nelisp--bq-expand form))
BQFIXEOF

# --- faithfulness conjuncts, shared verbatim between the replay probe and
# the cold-boot probe so the two columns are directly comparable.
#
# IMPORTANT: the conjuncts are evaluated BARE, never wrapped in
# `ignore-errors'.  This standalone reader's `ignore-errors' /
# `condition-case' returns nil instead of the protected body's VALUE, so an
# `(ignore-errors (featurep (quote org)))' probe reports nil even when
# `(featurep (quote org))' is t -- that wrapper is what made the 2026-07-04
# Increment-3 columns show featurep org/org-element as nil despite the
# replay having loaded the full chain (60/60 files ok).  `featurep' and
# `fboundp' are native builtins and cannot signal here; the only conjunct
# that could signal void-function, `commandp' (a shim defined by one of the
# 60 vendor files), is guarded with `(and (fboundp ...) ...)' instead.
CONJUNCTS='(featurep (quote org)) (featurep (quote org-element)) (fboundp (quote org-mode)) (fboundp (quote org-element-parse-buffer)) (and (fboundp (quote commandp)) (commandp (quote org-mode))) (fboundp (quote commandp))'

declare -a LOAD_US DUMP_US COLDBOOT_US COLDBOOT_RSS_KB
REPLAY_PROBE=""
COLD_PROBE=""

for i in $(seq 1 "$RUNS"); do
  echo "[cold-image-org-e2e] === run $i/$RUNS: replay-load + probe + dump ==="
  IMG="$WORKDIR/org-run$i.img"
  PROBE_FILE="$WORKDIR/probe-replay-run$i.txt"
  cp "$WORKDIR/load-only.repl" "$WORKDIR/phase-run$i.repl"
  {
    echo "(nl-write-file \"$WORKDIR/t_load_done_run$i\" (format \"%d\" (nl-unix-time-usec)))"
    echo "(nl-write-file \"$PROBE_FILE\" (format \"%S\" (list $CONJUNCTS)))"
    echo "(nelisp--arena-dump-image-stream \"$IMG\")"
    echo "(nl-write-file \"$WORKDIR/t_dump_done_run$i\" (format \"%d\" (nl-unix-time-usec)))"
    echo ",quit"
  } >> "$WORKDIR/phase-run$i.repl"

  pkill -9 -f "$NELISP_BIN_PAT --repl" 2>/dev/null
  T_START_US=$(date +%s%6N)
  timeout "$LOAD_TIMEOUT" "$NELISP_BIN" --repl --no-prompt --no-print \
    < "$WORKDIR/phase-run$i.repl" > "$WORKDIR/run$i.out" 2> "$WORKDIR/run$i.err"
  RC=$?
  T_END_US=$(date +%s%6N)

  if [ "$RC" != "0" ] || [ ! -f "$WORKDIR/t_dump_done_run$i" ]; then
    echo "[cold-image-org-e2e] run $i FAILED (exit=$RC); see $WORKDIR/run$i.err" >&2
    cat "$WORKDIR/run$i.err" >&2
    exit 1
  fi

  T_LOAD_DONE=$(cat "$WORKDIR/t_load_done_run$i")
  T_DUMP_DONE=$(cat "$WORKDIR/t_dump_done_run$i")
  LOAD_US[$i]=$(( T_LOAD_DONE - T_START_US ))
  DUMP_US[$i]=$(( T_DUMP_DONE - T_LOAD_DONE ))
  REPLAY_PROBE=$(cat "$PROBE_FILE")
  echo "[cold-image-org-e2e] run $i: load=${LOAD_US[$i]}us dump=${DUMP_US[$i]}us total_wall=$((T_END_US - T_START_US))us img=$(stat -c%s "$IMG" 2>/dev/null || echo '?')B"
done

# Keep the LAST run's image for the cold-boot phase.
LAST_IMG="$WORKDIR/org-run${RUNS}.img"

for i in $(seq 1 "$RUNS"); do
  echo "[cold-image-org-e2e] === run $i/$RUNS: cold boot ==="
  {
    for c in "(featurep (quote org))" \
             "(featurep (quote org-element))" \
             "(fboundp (quote org-mode))" \
             "(fboundp (quote org-element-parse-buffer))" \
             "(and (fboundp (quote commandp)) (commandp (quote org-mode)))" \
             "(fboundp (quote commandp))" \
             "(+ 40 3)"; do
      echo "$c"
    done
  } > "$WORKDIR/coldboot-forms.txt"

  pkill -9 -f "$NELISP_BIN_PAT --cold-load-from" 2>/dev/null
  T_START_US=$(date +%s%6N)
  "$NELISP_BIN" --cold-load-from "$LAST_IMG" --no-prompt \
    < "$WORKDIR/coldboot-forms.txt" > "$WORKDIR/coldboot$i.out" 2> "$WORKDIR/coldboot$i.err" &
  CPID=$!
  PEAK_KB=0
  while kill -0 "$CPID" 2>/dev/null; do
    v=$(awk '/VmHWM/{print $2}' "/proc/$CPID/status" 2>/dev/null)
    [ -n "$v" ] && PEAK_KB=$v
  done
  wait "$CPID"
  RC=$?
  T_END_US=$(date +%s%6N)
  if [ "$RC" != "0" ]; then
    echo "[cold-image-org-e2e] cold-boot run $i FAILED (exit=$RC); see $WORKDIR/coldboot$i.err" >&2
    cat "$WORKDIR/coldboot$i.err" >&2
    exit 1
  fi
  COLDBOOT_US[$i]=$(( T_END_US - T_START_US ))
  COLDBOOT_RSS_KB[$i]=$PEAK_KB
  # First 6 output lines are the conjuncts (in the same order as
  # $CONJUNCTS/$REPLAY_PROBE); the 7th is the "(+ 40 3)" = 43 sanity check.
  COLD_CONJUNCTS_JOINED=$(head -n 6 "$WORKDIR/coldboot$i.out" | tr '\n' ' ' | sed -E 's/ +$//')
  COLD_SANITY=$(sed -n '7p' "$WORKDIR/coldboot$i.out")
  COLD_PROBE="($COLD_CONJUNCTS_JOINED)"
  echo "[cold-image-org-e2e] cold-boot run $i: wall=${COLDBOOT_US[$i]}us peak_rss=${PEAK_KB}KB probe=$COLD_PROBE sanity(+40 3)=$COLD_SANITY"
done

median() {
  # Prints the median of the numeric args (odd count expected; falls back
  # to lower-middle for even counts).
  printf '%s\n' "$@" | sort -n | awk '{a[NR]=$0} END{print a[int((NR+1)/2)]}'
}

echo ""
echo "==================== TIMING TABLE (median of $RUNS runs) ===================="
LOAD_MED_US=$(median "${LOAD_US[@]}")
DUMP_MED_US=$(median "${DUMP_US[@]}")
COLD_MED_US=$(median "${COLDBOOT_US[@]}")
printf 'full .repl replay load : %8d us  (%.3f s)  [runs: %s]\n' \
  "$LOAD_MED_US" "$(echo "$LOAD_MED_US/1000000" | bc -l)" "${LOAD_US[*]}"
printf 'dump (in-process)      : %8d us  (%.3f s)  [runs: %s]\n' \
  "$DUMP_MED_US" "$(echo "$DUMP_MED_US/1000000" | bc -l)" "${DUMP_US[*]}"
printf 'cold boot to first eval: %8d us  (%.3f s)  [runs: %s]\n' \
  "$COLD_MED_US" "$(echo "$COLD_MED_US/1000000" | bc -l)" "${COLDBOOT_US[*]}"
printf 'cold-boot peak RSS      : %s KB  [runs: %s]\n' \
  "$(median "${COLDBOOT_RSS_KB[@]}")" "${COLDBOOT_RSS_KB[*]}"
printf 'speedup (replay/cold)   : %.1fx\n' "$(echo "$LOAD_MED_US/$COLD_MED_US" | bc -l)"

echo ""
echo "==================== FAITHFULNESS COLUMNS ===================="
echo "conjuncts: (featurep org) (featurep org-element) (fboundp org-mode) (fboundp org-element-parse-buffer) (commandp org-mode) (fboundp commandp)"
echo "replay-loaded process : $REPLAY_PROBE"
echo "cold-booted process   : $COLD_PROBE"
if [ "$REPLAY_PROBE" = "$COLD_PROBE" ]; then
  echo "RESULT: IDENTICAL -- cold-load is faithful to the replay-loaded state."
else
  echo "RESULT: MISMATCH -- cold-load diverges from the replay-loaded state."
fi
echo "================================================================"
