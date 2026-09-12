#!/usr/bin/env bash
# tools/nelisp-real-init-audit.sh
#
# Drive a real user Emacs init file through the NeLisp standalone binary,
# top-level form by top-level form, and report which forms error, whether
# the process reaches the end alive, and what it did to memory.
#
# ---------------------------------------------------------------------------
# Form-boundary discovery (AI.md rule: never a regex or a naive paren
# counter).  A real Emacs (`emacs --batch`, the SYSTEM Emacs, never the
# binary under test) reads the init file with the real Lisp reader
# (`read` on a buffer, in a loop, catching `end-of-file` and any other
# read error) and records each form's exact character span.  That
# splitting step never evaluates anything from the init file; it only
# parses.
#
# ---------------------------------------------------------------------------
# Driver fidelity choice (required by the task spec to be decided by
# experiment and stated explicitly -- see the probe transcript in the
# commit/report for the experiment; summary here):
#
#   The generator slices the EXACT bytes of each successfully-read form
#   out of the real init file and splices them, byte for byte, as literal
#   Lisp syntax into a generated driver .el file, each wrapped in its own
#   `condition-case`.  NeLisp's OWN reader and evaluator are what actually
#   read and run every form when the binary under test loads the driver;
#   only the *segmentation* (where one form ends and the next begins) is
#   supplied externally instead of being discovered by NeLisp's own
#   read-loop.
#
#   The alternative -- letting NeLisp itself drive the segmentation, by
#   calling `(read (current-buffer))` in a loop directly over the raw init
#   text inside the NeLisp process -- was rejected after the experiment in
#   the probe transcript: it makes the boundary-finding itself untested,
#   and a single place where NeLisp's reader disagreed with the real
#   Emacs reader about where a form ends would desync every following
#   index, turning one localized reader bug into hundreds of
#   falsely-attributed FORM_ERRORs (or silently merging two forms into
#   one) with no way to tell the difference from this tool's output
#   alone.  Pre-slicing with an externally-verified segmentation makes
#   each form's `condition-case` boundary reliable regardless of what
#   NeLisp's own reader does with the text inside it.
#
#   Fidelity limitations this choice accepts, stated explicitly:
#     - The generated driver lives at a different path than the real init
#       file.  Any form that depends on `load-file-name` or
#       `buffer-file-name` resolving to the real init file's own location
#       (e.g. a relative `require`/`load` of a sibling file) will not
#       resolve the same way here as it would loading the real file in
#       place.
#     - This harness does NOT test whether NeLisp's own reader, driving
#       the segmentation itself end-to-end over the raw file with no
#       externally-supplied spans, would find the same top-level forms in
#       the same places.  That is a distinct question this tool does not
#       answer.
#     - Wrapping every form in its own `condition-case` intentionally
#       continues past errors that a real `load` would abort on.  A
#       genuine `load` stops at the first error; this tool surveys every
#       form regardless, which is the point of an audit but is not what a
#       real interactive Emacs startup does.
#
# ---------------------------------------------------------------------------
# Memory accounting (see the report emitted by this script, and AI.md
# rule 3): this script reports two different kinds of number and does not
# collapse them into one conclusion:
#   (a) process-level RSS from /proc/<pid>/status and .../smaps_rollup --
#       this is the WHOLE process (code, stack, every mapping), not
#       allocator-specific, and cannot alone distinguish a leak from an
#       allocator that retains reusable arena space.  On Darwin, which has
#       no procfs, the same columns are filled from `ps -o rss=,vsz=' plus
#       a high-water mark this script tracks itself; the smaps_* columns
#       stay NA because macOS exposes no smaps_rollup equivalent and no
#       transparent huge pages to account for.  Before that branch existed
#       every one of these columns read NA on macOS, so the run sheet's
#       memory accounting was silently blind on the platform it was
#       written for -- measured 2026-09-12 on macos 26.6.2 arm64.
#   (b) NeLisp's own arena counters (`nelisp--arena-stats`, exposed at
#       runtime, no dependency on any dev-only build) -- `used-bytes`
#       and `bump-offset` are allocator-attributable totals ever handed
#       out of the Lisp heap; `live-after-last-gc` is only refreshed when
#       a collection actually runs, so the driver forces exactly one
#       explicit `(garbage-collect)` after the last form and reports the
#       arena tuple again immediately after, as the closest available
#       reading of the true live set.
#
set -uo pipefail

BINARY=""
INIT="${HOME}/.emacs.d/init.el"
OUT=""
LIMIT=0
TIMEOUT_SECS=1800
MEM_INTERVAL=1
ARENA_EVERY=25
EMACS_BIN="${NELISP_AUDIT_EMACS:-emacs}"

usage() {
  cat <<'USAGE'
Usage: nelisp-real-init-audit.sh --binary PATH --out DIR
           [--init PATH] [--limit N] [--timeout SECONDS]
           [--mem-interval SECONDS] [--arena-every N]

  --binary PATH   the NeLisp executable to run (run a COPY, never the
                   binary another process is using -- this tool does not
                   copy it for you)
  --init PATH     the real init file to split and drive
                   (default: $HOME/.emacs.d/init.el); treated read-only
  --out DIR       directory for every artifact this run produces
  --limit N       only include the first N successfully-read forms in the
                   driver (form discovery in forms.tsv/forms.json is
                   always full; only the run is truncated) -- for a short
                   smoke before a full run
  --timeout SECS  kill the driver run after this many seconds
                   (default: 1800)
  --mem-interval SECS  memory-sample period (default: 1)
  --arena-every N      print a NeLisp arena snapshot every N forms, and
                        always at form 1 (default: 25)

Environment:
  NELISP_AUDIT_EMACS  real Emacs used only to split the init file into
                       top-level forms (default: "emacs" on PATH)
USAGE
}

while [ $# -gt 0 ]; do
  case "$1" in
    --binary) BINARY="${2:-}"; shift 2 ;;
    --init) INIT="${2:-}"; shift 2 ;;
    --out) OUT="${2:-}"; shift 2 ;;
    --limit) LIMIT="${2:-}"; shift 2 ;;
    --timeout) TIMEOUT_SECS="${2:-}"; shift 2 ;;
    --mem-interval) MEM_INTERVAL="${2:-}"; shift 2 ;;
    --arena-every) ARENA_EVERY="${2:-}"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "error: unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
done

if [ -z "$BINARY" ] || [ -z "$OUT" ]; then
  echo "error: --binary and --out are required" >&2
  usage >&2
  exit 2
fi
if [ ! -f "$BINARY" ] || [ ! -x "$BINARY" ]; then
  echo "error: --binary '$BINARY' is not an executable file" >&2
  exit 2
fi
if [ ! -f "$INIT" ]; then
  echo "error: --init '$INIT' does not exist" >&2
  exit 2
fi
case "$LIMIT" in ''|*[!0-9]*) echo "error: --limit must be a non-negative integer, got '$LIMIT'" >&2; exit 2 ;; esac
case "$TIMEOUT_SECS" in ''|*[!0-9]*) echo "error: --timeout must be a positive integer, got '$TIMEOUT_SECS'" >&2; exit 2 ;; esac
case "$MEM_INTERVAL" in ''|*[!0-9]*) echo "error: --mem-interval must be a positive integer, got '$MEM_INTERVAL'" >&2; exit 2 ;; esac
case "$ARENA_EVERY" in ''|*[!0-9]*) echo "error: --arena-every must be a positive integer, got '$ARENA_EVERY'" >&2; exit 2 ;; esac
if [ "$ARENA_EVERY" -lt 1 ]; then ARENA_EVERY=1; fi

if ! command -v "$EMACS_BIN" >/dev/null 2>&1; then
  echo "error: '$EMACS_BIN' (real Emacs, used only to find form boundaries) not found on PATH" >&2
  exit 2
fi

mkdir -p "$OUT"
OUT="$(cd "$OUT" && pwd)"
BINARY="$(cd "$(dirname "$BINARY")" && pwd)/$(basename "$BINARY")"
INIT="$(cd "$(dirname "$INIT")" && pwd)/$(basename "$INIT")"

STDOUT_LOG="$OUT/driver-stdout.log"
STDERR_LOG="$OUT/driver-stderr.log"
SPLIT_LOG="$OUT/split.log"
FORMS_TSV="$OUT/forms.tsv"
FORMS_JSON="$OUT/forms.json"
SPLIT_MANIFEST="$OUT/split-manifest.json"
DRIVER_EL="$OUT/nelisp-audit-driver.el"
SPLIT_GEN_EL="$OUT/nelisp-audit-split-gen.el"
MEM_TSV="$OUT/mem-samples.tsv"
SUMMARY_TXT="$OUT/summary.txt"
SUMMARY_JSON="$OUT/summary.json"
RUN_ID="$(date -u +%Y%m%dT%H%M%SZ)"

echo "== nelisp-real-init-audit ($RUN_ID) ==" | tee "$SUMMARY_TXT"
echo "binary: $BINARY" | tee -a "$SUMMARY_TXT"
BINARY_SHA="$(sha256sum "$BINARY" | awk '{print $1}')"
echo "binary sha256: $BINARY_SHA" | tee -a "$SUMMARY_TXT"
echo "init file: $INIT (read-only, never modified by this tool)" | tee -a "$SUMMARY_TXT"
INIT_SHA_BEFORE="$(sha256sum "$INIT" | awk '{print $1}')"
echo "init sha256 (before run): $INIT_SHA_BEFORE" | tee -a "$SUMMARY_TXT"
echo "out dir: $OUT" | tee -a "$SUMMARY_TXT"
echo "limit: $LIMIT (0 = no limit)" | tee -a "$SUMMARY_TXT"
echo "timeout: ${TIMEOUT_SECS}s" | tee -a "$SUMMARY_TXT"
echo "host load at start: $(uptime 2>/dev/null || echo unavailable)" | tee -a "$SUMMARY_TXT"
echo "other processes matching 'nelisp' at start:" | tee -a "$SUMMARY_TXT"
ps -eo pid,pcpu,pmem,etime,args 2>/dev/null | grep -i nelisp | grep -v grep | grep -v "$$" >> "$SUMMARY_TXT" || echo "  (none seen)" >> "$SUMMARY_TXT"
echo "real emacs (form-boundary discovery only): $("$EMACS_BIN" --version | head -1)" | tee -a "$SUMMARY_TXT"

# ---------------------------------------------------------------------------
# Step 1: split the init file into top-level forms with a real reader, and
# generate the driver.  This step runs under the SYSTEM Emacs, never under
# the binary being audited.

cat > "$SPLIT_GEN_EL" <<'ELISP'
;;; nelisp-audit-split-gen.el --- runtime-generated, not part of the repo -*- lexical-binding: t; -*-
;;
;; Written by tools/nelisp-real-init-audit.sh into its --out directory at
;; run time.  Run only under a real Emacs (--batch).  Splits an init file
;; into top-level forms using the real reader (`read` on a buffer, never a
;; regex or paren counter), records each form's exact character span, and
;; generates a driver .el file that embeds each successfully-read form's
;; verbatim source text in its own `condition-case`.

(require 'cl-lib)

(defun nelisp-audit--json-str (s)
  (setq s (or s ""))
  (setq s (replace-regexp-in-string "\\\\" "\\\\\\\\" s))
  (setq s (replace-regexp-in-string "\"" "\\\\\"" s))
  (setq s (replace-regexp-in-string "\n" "\\\\n" s))
  (setq s (replace-regexp-in-string "\r" "\\\\r" s))
  (setq s (replace-regexp-in-string "\t" "\\\\t" s))
  (concat "\"" s "\""))

(defun nelisp-audit--flatten (text)
  (replace-regexp-in-string "[\n\r\t]+" " " (or text "")))

(defun nelisp-audit--head (text n)
  (let ((flat (nelisp-audit--flatten text)))
    (if (> (length flat) n) (substring flat 0 n) flat)))

(let* ((args (let ((a command-line-args-left))
               (if (equal (car a) "--") (cdr a) a)))
       (init-file (nth 0 args))
       (limit (string-to-number (or (nth 1 args) "0")))
       (arena-every (max 1 (string-to-number (or (nth 2 args) "25"))))
       (forms-tsv (nth 3 args))
       (forms-json (nth 4 args))
       (manifest-json (nth 5 args))
       (driver-el (nth 6 args)))
  (unless (and init-file forms-tsv forms-json manifest-json driver-el)
    (error "usage: split-gen.el INIT LIMIT ARENA_EVERY FORMS_TSV FORMS_JSON MANIFEST_JSON DRIVER_EL"))
  (with-temp-buffer
    (let ((buf (current-buffer))
          (forms nil)
          (idx 0)
          (done nil)
          (had-read-error nil))
      (insert-file-contents init-file)
      (goto-char (point-min))
      (while (not done)
        (let ((start (point))
              (attempt (1+ idx)))
          (condition-case err
              (progn
                (read buf)
                (setq idx attempt)
                (push (list idx start (point) t nil nil
                            (buffer-substring-no-properties start (point)))
                      forms))
            (end-of-file (setq done t))
            (error
             (setq idx attempt
                   had-read-error t)
             (push (list idx start (point) nil (format "%S" (car err))
                         (error-message-string err) "")
                   forms)
             (setq done t)))))
      (setq forms (nreverse forms))
      (let* ((total (length forms))
             (included (if (> limit 0) (min limit total) total)))

        ;; forms.tsv / forms.json: the FULL split, never truncated by --limit.
        (with-temp-file forms-tsv
          (insert "index\tstart\tend\tlength\tread_ok\terror_symbol\terror_message\thead60\n")
          (dolist (f forms)
            (cl-destructuring-bind (i s e ok esym emsg text) f
              (insert (format "%d\t%d\t%d\t%d\t%s\t%s\t%s\t%s\n"
                               i s e (- e s) (if ok "t" "nil")
                               (or esym "")
                               (nelisp-audit--flatten (or emsg ""))
                               (nelisp-audit--head text 60))))))

        (with-temp-file forms-json
          (insert "[\n")
          (let ((first t))
            (dolist (f forms)
              (cl-destructuring-bind (i s e ok esym emsg text) f
                (if first (setq first nil) (insert ",\n"))
                (insert (format "  {\"index\": %d, \"start\": %d, \"end\": %d, \"length\": %d, \"read_ok\": %s, \"error_symbol\": %s, \"error_message\": %s, \"head60\": %s}"
                                 i s e (- e s) (if ok "true" "false")
                                 (if (and esym (not (equal esym ""))) (nelisp-audit--json-str esym) "null")
                                 (if (and emsg (not (equal emsg ""))) (nelisp-audit--json-str emsg) "null")
                                 (nelisp-audit--json-str (nelisp-audit--head text 60)))))))
          (insert "\n]\n"))

        ;; driver.el: verbatim per-form condition-case wrappers, only for
        ;; forms that were actually read successfully, up to `included'.
        (with-temp-file driver-el
          (insert ";;; nelisp-audit-driver.el --- runtime-generated, not part of the repo -*- lexical-binding: t; -*-\n")
          (insert (format ";; generated %s from %s\n;; total_forms_found=%d forms_included=%d limit_requested=%d arena_every=%d\n"
                           (format-time-string "%Y-%m-%dT%H:%M:%S%z")
                           init-file total included limit arena-every))
          (insert "(defun nelisp-audit--bounded-message (err lim)
  (let* ((raw (condition-case nil (error-message-string err) (error (format \"%S\" err))))
         (flat (replace-regexp-in-string \"[\\n\\r]+\" \" \" raw)))
    (if (> (length flat) lim)
        (concat (substring flat 0 lim) \"...[truncated]\")
      flat)))
")
          (insert "(defun nelisp-audit--print-arena (tag)
  (if (fboundp 'nelisp--arena-stats)
      (condition-case err
          (princ (format \"ARENA %s %s\\n\" tag (mapconcat (lambda (x) (format \"%s\" x)) (nelisp--arena-stats) \" \")))
        (error (princ (format \"ARENA %s READ_ERROR %S\\n\" tag err))))
    (princ (format \"ARENA %s UNAVAILABLE\\n\" tag))))
")
          (insert (format "(princ \"AUDIT_START %d\\n\")\n" included))
          (insert "(nelisp-audit--print-arena \"0\")\n")
          (dolist (f forms)
            (cl-destructuring-bind (i s e ok esym emsg text) f
              (ignore s e esym emsg)
              (when (and ok (<= i included))
                (insert (format "(princ \"BOUNDARY %d\\n\")\n" i))
                (insert (format "(condition-case nelisp-audit--e\n    %s\n  (t (princ (format \"FORM_ERROR %d %%S %%S\\n\" (car nelisp-audit--e) (nelisp-audit--bounded-message nelisp-audit--e 300)))))\n"
                                 text i))
                (when (or (= i 1) (= 0 (mod i arena-every)))
                  (insert (format "(nelisp-audit--print-arena \"%d\")\n" i))))))
          (insert (format "(princ \"AUDIT_DONE %d\\n\")\n" included))
          (insert "(nelisp-audit--print-arena \"FINAL\")\n")
          (insert "(garbage-collect)\n")
          (insert "(nelisp-audit--print-arena \"POST_GC\")\n"))

        (with-temp-file manifest-json
          (insert (format "{\n  \"init_file\": %s,\n  \"total_forms_found\": %d,\n  \"forms_included_in_driver\": %d,\n  \"limit_requested\": %d,\n  \"arena_every\": %d,\n  \"read_error_encountered\": %s,\n  \"emacs_version\": %s\n}\n"
                           (nelisp-audit--json-str init-file)
                           total included limit arena-every
                           (if had-read-error "true" "false")
                           (nelisp-audit--json-str (emacs-version)))))

        (princ (format "split-gen: total_forms_found=%d forms_included=%d limit_requested=%d read_error_encountered=%s\n"
                        total included limit (if had-read-error "t" "nil")))))))
ELISP

if ! "$EMACS_BIN" --batch -Q --load "$SPLIT_GEN_EL" -- \
      "$INIT" "$LIMIT" "$ARENA_EVERY" \
      "$FORMS_TSV" "$FORMS_JSON" "$SPLIT_MANIFEST" "$DRIVER_EL" \
      > "$SPLIT_LOG" 2>&1
then
  echo "error: form-splitting step failed; see $SPLIT_LOG" | tee -a "$SUMMARY_TXT" >&2
  tail -40 "$SPLIT_LOG" >&2
  exit 3
fi

TOTAL_FORMS_FOUND="$(grep -o '"total_forms_found": [0-9]*' "$SPLIT_MANIFEST" | grep -o '[0-9]*')"
FORMS_INCLUDED="$(grep -o '"forms_included_in_driver": [0-9]*' "$SPLIT_MANIFEST" | grep -o '[0-9]*')"
READ_ERROR_ENCOUNTERED="$(grep -o '"read_error_encountered": [a-z]*' "$SPLIT_MANIFEST" | awk '{print $2}')"

echo "total top-level forms found by the real reader: $TOTAL_FORMS_FOUND" | tee -a "$SUMMARY_TXT"
echo "forms included in this run's driver: $FORMS_INCLUDED" | tee -a "$SUMMARY_TXT"
echo "read error encountered while splitting: $READ_ERROR_ENCOUNTERED" | tee -a "$SUMMARY_TXT"
echo "spans: $FORMS_TSV / $FORMS_JSON" | tee -a "$SUMMARY_TXT"
echo "generated driver: $DRIVER_EL" | tee -a "$SUMMARY_TXT"

if [ -z "$TOTAL_FORMS_FOUND" ] || [ "$FORMS_INCLUDED" -eq 0 ]; then
  echo "error: zero forms were included in the driver; refusing to call that a run" | tee -a "$SUMMARY_TXT" >&2
  exit 4
fi

# ---------------------------------------------------------------------------
# Step 2: run the driver under the binary being audited, sampling memory
# at a fixed interval from OUTSIDE the process the whole time.

: > "$MEM_TSV"
printf 'elapsed_sec\tepoch\tvmrss_kb\tvmhwm_kb\tvmsize_kb\tvmdata_kb\tsmaps_rss_kb\tsmaps_pss_kb\tsmaps_private_dirty_kb\tsmaps_anonymous_kb\tsmaps_swap_kb\tlast_stdout_tag\n' >> "$MEM_TSV"

: > "$STDOUT_LOG"
: > "$STDERR_LOG"

START_EPOCH="$(date +%s)"
echo "starting driver run under: $BINARY --load $DRIVER_EL (timeout ${TIMEOUT_SECS}s)" | tee -a "$SUMMARY_TXT"

timeout --signal=TERM --kill-after=10 "$TIMEOUT_SECS" "$BINARY" --load "$DRIVER_EL" \
  > "$STDOUT_LOG" 2> "$STDERR_LOG" &
CHILD_PID=$!

HOST_UNAME_S="$(uname -s 2>/dev/null || true)"
DARWIN_PEAK_RSS="NA"

# Sample the BINARY UNDER TEST, not the `timeout' wrapper.  `$!' above is
# `timeout''s pid, and every memory column here was therefore reporting
# `timeout''s own footprint -- a constant ~1.5 MiB that has nothing to do
# with the audited process.  It went unnoticed because on Linux it produced
# a plausible small number and on macOS, before this script could read
# memory at all, every column was NA.  Measured 2026-09-12 on macos 26.6.2
# arm64: the wrapper reported 1472 KiB while its child, the binary actually
# being audited, reported 206704 KiB at the same instant.
#
# `pgrep' is not portable enough to be the only way of asking.  Stock MSYS2
# ships no procps-ng, so `pgrep -P' is simply absent there: the substitution
# failed silently and this script went straight back to reporting the
# wrapper, on the one platform where nobody would notice from the numbers
# alone.  Measured 2026-09-12 on a Windows MSYS2 host at 771e17a29 -- the
# run completed and every memory column was the wrapper's again.  `ps -ef'
# prints PID in column 2 and PPID in column 3 on GNU/Linux, macOS and MSYS2
# alike, so it answers the same question wherever `timeout' itself exists.
nelisp_first_child_of() {
  nelisp_parent="$1"
  nelisp_child="$(pgrep -P "$nelisp_parent" 2>/dev/null | head -1 || true)"
  case "$nelisp_child" in
    ''|*[!0-9]*)
      nelisp_child="$(ps -ef 2>/dev/null \
        | awk -v parent="$nelisp_parent" \
            '$2 ~ /^[0-9]+$/ && $3 == parent { print $2; exit }' || true)" ;;
  esac
  printf '%s' "$nelisp_child"
}

MEM_PID="$CHILD_PID"
MEM_PID_CHILD="$(nelisp_first_child_of "$CHILD_PID")"
case "$MEM_PID_CHILD" in
  ''|*[!0-9]*) : ;;
  *) MEM_PID="$MEM_PID_CHILD" ;;
esac
echo "memory samples target pid: $MEM_PID (timeout wrapper pid: $CHILD_PID)" \
  | tee -a "$SUMMARY_TXT"

SAMPLE_N=0
while kill -0 "$CHILD_PID" 2>/dev/null; do
  SAMPLE_N=$((SAMPLE_N + 1))
  NOW_EPOCH="$(date +%s)"
  ELAPSED=$((NOW_EPOCH - START_EPOCH))

  VMRSS="NA"; VMHWM="NA"; VMSIZE="NA"; VMDATA="NA"
  if [ -r "/proc/$MEM_PID/status" ]; then
    STATUS_TXT="$(cat "/proc/$MEM_PID/status" 2>/dev/null || true)"
    VMRSS="$(printf '%s\n' "$STATUS_TXT" | awk '/^VmRSS:/{print $2; exit}')"
    VMHWM="$(printf '%s\n' "$STATUS_TXT" | awk '/^VmHWM:/{print $2; exit}')"
    VMSIZE="$(printf '%s\n' "$STATUS_TXT" | awk '/^VmSize:/{print $2; exit}')"
    VMDATA="$(printf '%s\n' "$STATUS_TXT" | awk '/^VmData:/{print $2; exit}')"
    [ -z "$VMRSS" ] && VMRSS="NA"
    [ -z "$VMHWM" ] && VMHWM="NA"
    [ -z "$VMSIZE" ] && VMSIZE="NA"
    [ -z "$VMDATA" ] && VMDATA="NA"
  fi

  # Darwin has no /proc.  `ps' reports rss and vsz in KiB, the same unit
  # VmRSS/VmSize use, so these land in the existing columns unconverted.
  # There is no VmHWM analogue, so the peak is accumulated here instead of
  # read; VmData has no equivalent at all and stays NA rather than being
  # filled with a number that would mean something else.
  if [ "$VMRSS" = "NA" ] && [ "$HOST_UNAME_S" = "Darwin" ]; then
    PS_LINE="$(/bin/ps -o rss=,vsz= -p "$MEM_PID" 2>/dev/null || true)"
    VMRSS="$(printf '%s\n' "$PS_LINE" | awk 'NF>=2{print $1; exit}')"
    VMSIZE="$(printf '%s\n' "$PS_LINE" | awk 'NF>=2{print $2; exit}')"
    [ -z "$VMRSS" ] && VMRSS="NA"
    [ -z "$VMSIZE" ] && VMSIZE="NA"
    if [ "$VMRSS" != "NA" ]; then
      case "$DARWIN_PEAK_RSS" in
        NA) DARWIN_PEAK_RSS="$VMRSS" ;;
        *)  if [ "$VMRSS" -gt "$DARWIN_PEAK_RSS" ] 2>/dev/null; then
              DARWIN_PEAK_RSS="$VMRSS"
            fi ;;
      esac
      VMHWM="$DARWIN_PEAK_RSS"
    fi
  fi

  SRSS="NA"; SPSS="NA"; SPDIRTY="NA"; SANON="NA"; SSWAP="NA"
  if [ -r "/proc/$MEM_PID/smaps_rollup" ]; then
    ROLLUP_TXT="$(cat "/proc/$MEM_PID/smaps_rollup" 2>/dev/null || true)"
    SRSS="$(printf '%s\n' "$ROLLUP_TXT" | awk '/^Rss:/{print $2; exit}')"
    SPSS="$(printf '%s\n' "$ROLLUP_TXT" | awk '/^Pss:/{print $2; exit}')"
    SPDIRTY="$(printf '%s\n' "$ROLLUP_TXT" | awk '/^Private_Dirty:/{print $2; exit}')"
    SANON="$(printf '%s\n' "$ROLLUP_TXT" | awk '/^Anonymous:/{print $2; exit}')"
    SSWAP="$(printf '%s\n' "$ROLLUP_TXT" | awk '/^Swap:/{print $2; exit}')"
    [ -z "$SRSS" ] && SRSS="NA"
    [ -z "$SPSS" ] && SPSS="NA"
    [ -z "$SPDIRTY" ] && SPDIRTY="NA"
    [ -z "$SANON" ] && SANON="NA"
    [ -z "$SSWAP" ] && SSWAP="NA"
  fi

  LAST_TAG="$(tail -1 "$STDOUT_LOG" 2>/dev/null | tr -d '\n' | tr -d '\t')"
  [ -z "$LAST_TAG" ] && LAST_TAG="NA"

  printf '%d\t%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$ELAPSED" "$NOW_EPOCH" "$VMRSS" "$VMHWM" "$VMSIZE" "$VMDATA" \
    "$SRSS" "$SPSS" "$SPDIRTY" "$SANON" "$SSWAP" "$LAST_TAG" >> "$MEM_TSV"

  sleep "$MEM_INTERVAL"
done

wait "$CHILD_PID"
CHILD_EXIT=$?
END_EPOCH="$(date +%s)"
DURATION=$((END_EPOCH - START_EPOCH))

CHILD_SIGNAL=""
if [ "$CHILD_EXIT" -gt 128 ]; then
  CHILD_SIGNAL=$((CHILD_EXIT - 128))
fi

echo "driver run finished in ${DURATION}s, exit=${CHILD_EXIT}${CHILD_SIGNAL:+ (signal $CHILD_SIGNAL)}" | tee -a "$SUMMARY_TXT"
echo "stdout: $STDOUT_LOG" | tee -a "$SUMMARY_TXT"
echo "stderr: $STDERR_LOG" | tee -a "$SUMMARY_TXT"
echo "memory samples ($SAMPLE_N, every ${MEM_INTERVAL}s): $MEM_TSV" | tee -a "$SUMMARY_TXT"

# ---------------------------------------------------------------------------
# Step 3: analyze stdout.log.  Every number below is read back out of
# STDOUT_LOG, not carried over from anything printed earlier in this shell.

BOUNDARY_COUNT="$(grep -c '^BOUNDARY [0-9][0-9]*$' "$STDOUT_LOG" || true)"
LAST_BOUNDARY="$(grep '^BOUNDARY [0-9][0-9]*$' "$STDOUT_LOG" | tail -1 | awk '{print $2}')"
[ -z "$LAST_BOUNDARY" ] && LAST_BOUNDARY="0"
AUDIT_DONE_LINE="$(grep -m1 '^AUDIT_DONE [0-9][0-9]*$' "$STDOUT_LOG" || true)"
AUDIT_DONE_COUNT=""
if [ -n "$AUDIT_DONE_LINE" ]; then
  AUDIT_DONE_COUNT="$(printf '%s' "$AUDIT_DONE_LINE" | awk '{print $2}')"
fi
FORM_ERROR_COUNT="$(grep -c '^FORM_ERROR [0-9][0-9]* ' "$STDOUT_LOG" || true)"

BREAKDOWN_FILE="$OUT/form-error-breakdown.tsv"
printf 'condition_symbol\tcount\n' > "$BREAKDOWN_FILE"
grep '^FORM_ERROR [0-9][0-9]* ' "$STDOUT_LOG" 2>/dev/null | awk '{print $3}' | sort | uniq -c | sort -rn | \
  awk '{print $2 "\t" $1}' >> "$BREAKDOWN_FILE"

ARENA_LINE_COUNT="$(grep -c '^ARENA ' "$STDOUT_LOG" || true)"
ARENA_FINAL_LINE="$(grep -m1 '^ARENA FINAL ' "$STDOUT_LOG" || true)"
ARENA_POSTGC_LINE="$(grep -m1 '^ARENA POST_GC ' "$STDOUT_LOG" || true)"

echo "" | tee -a "$SUMMARY_TXT"
echo "-- form-level results (from $STDOUT_LOG) --" | tee -a "$SUMMARY_TXT"
echo "BOUNDARY lines observed: $BOUNDARY_COUNT (last: $LAST_BOUNDARY)" | tee -a "$SUMMARY_TXT"
echo "AUDIT_DONE reached: $([ -n "$AUDIT_DONE_LINE" ] && echo "yes ($AUDIT_DONE_LINE)" || echo "NO")" | tee -a "$SUMMARY_TXT"
echo "FORM_ERROR count: $FORM_ERROR_COUNT" | tee -a "$SUMMARY_TXT"
echo "FORM_ERROR breakdown by condition symbol: $BREAKDOWN_FILE" | tee -a "$SUMMARY_TXT"
cat "$BREAKDOWN_FILE" | tee -a "$SUMMARY_TXT"
echo "" | tee -a "$SUMMARY_TXT"
echo "-- memory accounting (see header of this script for what each number is and is not) --" | tee -a "$SUMMARY_TXT"
echo "ARENA checkpoint lines observed: $ARENA_LINE_COUNT" | tee -a "$SUMMARY_TXT"
echo "final arena snapshot:  ${ARENA_FINAL_LINE:-none printed}" | tee -a "$SUMMARY_TXT"
echo "post-explicit-GC snapshot: ${ARENA_POSTGC_LINE:-none printed}" | tee -a "$SUMMARY_TXT"
echo "(ARENA fields, in order: base size bump-offset used-bytes live-after-last-gc next-trigger free-list-head collect-disabled reuse-disabled chunk-count chunk-bytes-reserved chunk-bytes-used)" | tee -a "$SUMMARY_TXT"
LAST_MEM_ROW="$(tail -1 "$MEM_TSV")"
echo "last process-level memory sample (elapsed epoch vmrss_kb vmhwm_kb vmsize_kb vmdata_kb smaps_rss_kb smaps_pss_kb smaps_private_dirty_kb smaps_anonymous_kb smaps_swap_kb last_stdout_tag):" | tee -a "$SUMMARY_TXT"
echo "  $LAST_MEM_ROW" | tee -a "$SUMMARY_TXT"

INIT_SHA_AFTER="$(sha256sum "$INIT" | awk '{print $1}')"
echo "" | tee -a "$SUMMARY_TXT"
echo "init sha256 (after run): $INIT_SHA_AFTER" | tee -a "$SUMMARY_TXT"
if [ "$INIT_SHA_BEFORE" != "$INIT_SHA_AFTER" ]; then
  echo "WARNING: init file sha256 changed during this run -- something else touched it" | tee -a "$SUMMARY_TXT"
fi

# ---------------------------------------------------------------------------
# Step 4: success/failure verdict.  Zero forms executed is never success;
# a signal death is never success; AUDIT_DONE must actually appear.

STATUS="ok"
EXIT_CODE=0
if [ "$BOUNDARY_COUNT" -eq 0 ]; then
  STATUS="fail: zero BOUNDARY lines observed"
  EXIT_CODE=4
elif [ -n "$CHILD_SIGNAL" ]; then
  STATUS="fail: driver killed by signal $CHILD_SIGNAL"
  EXIT_CODE=5
elif [ -z "$AUDIT_DONE_LINE" ]; then
  STATUS="fail: AUDIT_DONE not reached (last BOUNDARY seen: $LAST_BOUNDARY of $FORMS_INCLUDED included)"
  EXIT_CODE=6
elif [ "$CHILD_EXIT" -ne 0 ]; then
  STATUS="fail: AUDIT_DONE reached but driver process exited nonzero ($CHILD_EXIT)"
  EXIT_CODE=7
fi

echo "" | tee -a "$SUMMARY_TXT"
echo "VERDICT: $STATUS" | tee -a "$SUMMARY_TXT"

cat > "$SUMMARY_JSON" <<JSONEOF
{
  "run_id": "$RUN_ID",
  "binary": "$BINARY",
  "binary_sha256": "$BINARY_SHA",
  "init_file": "$INIT",
  "init_sha256_before": "$INIT_SHA_BEFORE",
  "init_sha256_after": "$INIT_SHA_AFTER",
  "limit_requested": $LIMIT,
  "timeout_seconds": $TIMEOUT_SECS,
  "total_forms_found": ${TOTAL_FORMS_FOUND:-0},
  "forms_included_in_driver": ${FORMS_INCLUDED:-0},
  "read_error_encountered_while_splitting": ${READ_ERROR_ENCOUNTERED:-false},
  "boundary_lines_observed": ${BOUNDARY_COUNT:-0},
  "last_boundary_seen": ${LAST_BOUNDARY:-0},
  "audit_done_reached": $([ -n "$AUDIT_DONE_LINE" ] && echo true || echo false),
  "audit_done_count": ${AUDIT_DONE_COUNT:-null},
  "form_error_count": ${FORM_ERROR_COUNT:-0},
  "child_exit_code": $CHILD_EXIT,
  "child_signal": ${CHILD_SIGNAL:-null},
  "duration_seconds": $DURATION,
  "memory_samples": $SAMPLE_N,
  "verdict": "$STATUS",
  "out_dir": "$OUT"
}
JSONEOF

echo "summary: $SUMMARY_TXT" | tee -a "$SUMMARY_TXT"
echo "summary (json): $SUMMARY_JSON" | tee -a "$SUMMARY_TXT"
echo "form-error breakdown: $BREAKDOWN_FILE" | tee -a "$SUMMARY_TXT"

exit "$EXIT_CODE"
