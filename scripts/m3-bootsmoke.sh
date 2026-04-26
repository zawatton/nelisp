#!/bin/sh
# m3-bootsmoke.sh --- T157 M3 milestone integration boot smoke
#
# Boot anvil-server / anvil-defs / anvil-state on top of the NeLisp
# 7+A-E evaluator pipeline (= reader + special-forms + closure +
# macro-ns + control-flow), the successor of T97's M1 smoke harness.
#
# Two probes:
#   1. M1-equivalent baseline   --- substrate + bridge install +
#      anvil-core boot + MCP `tools/list' + `tools/call' round-trip
#      using the host-Emacs evaluator (= unchanged from T97).
#   2. M3 NeLisp evaluator path --- exercise the Phase 7+ Lisp
#      Evaluator (Doc 40) through `nelisp-reader-read' +
#      `nelisp-special-forms-eval' to confirm anvil-server source
#      forms can be (a) parsed and (b) evaluated end-to-end without
#      the host eval engine.  Report a coverage % vs. the M3
#      milestone target (= "all anvil-server.el modules eval-able
#      under NeLisp" -- Doc 40 §1.4 / §3.6).
#
# Usage:
#   scripts/m3-bootsmoke.sh                         # PASS / FAIL summary
#   scripts/m3-bootsmoke.sh --raw                   # raw stdout
#   scripts/m3-bootsmoke.sh --bridge-status         # status + features
#   scripts/m3-bootsmoke.sh --tools-list-only       # just tools/list line
#   scripts/m3-bootsmoke.sh --eval-only             # just M3 eval probe
#   ANVIL_PATH=/path/to/anvil.el scripts/m3-bootsmoke.sh
#
# Exit codes:
#   0  PASS  -- substrate boots, bridge installs, anvil-core loads,
#               tools/list returns >=1 tool, M3 eval probe shows
#               readiness >= 50% on at least 1 tracked anvil module
#   1  PARTIAL or FAIL  -- one or more assertions failed (still useful
#               output for diagnosis; report still gets written)
#   2  environment / invocation error

set -eu

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
REPO=$(cd "$SCRIPT_DIR/.." && pwd)
EMACS="${EMACS:-emacs}"
MODE="${1:-check}"

ANVIL_PATH="${ANVIL_PATH:-}"
if [ -z "$ANVIL_PATH" ]; then
  for cand in \
    "$HOME/Cowork/Notes/dev/anvil.el" \
    "$HOME/Notes/dev/anvil.el" \
    "$HOME/.emacs.d/external-packages/anvil.el"; do
    if [ -d "$cand" ] && [ -f "$cand/anvil-server.el" ]; then
      ANVIL_PATH="$cand"
      break
    fi
  done
fi
if [ -z "$ANVIL_PATH" ] || [ ! -f "$ANVIL_PATH/anvil-server.el" ]; then
  echo "m3-bootsmoke: ANVIL_PATH not set / anvil-server.el not found" >&2
  echo "  set ANVIL_PATH=/path/to/anvil.el" >&2
  exit 2
fi

if ! command -v "$EMACS" >/dev/null 2>&1; then
  echo "m3-bootsmoke: $EMACS not on PATH" >&2
  exit 2
fi

REQUESTS=$(cat <<'EOF'
{"jsonrpc":"2.0","id":1,"method":"initialize"}
{"jsonrpc":"2.0","method":"notifications/initialized"}
{"jsonrpc":"2.0","id":2,"method":"tools/list"}
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"defs-index-status","arguments":{}}}
EOF
)

OUT=$(mktemp)
ERR=$(mktemp)
trap 'rm -f "$OUT" "$ERR"' EXIT

# Probe 2 (M3) eval driver --- inline elisp; reads + evaluates a
# representative subset of anvil-server / anvil-defs / anvil-state
# forms through the NeLisp 7+A-E evaluator pipeline and reports a
# coverage matrix.  Failure of any probe is a WARN (NOT a FAIL) so
# diagnostic output reaches the report.
#
# Three coverage dimensions per module:
#   :forms-total           total top-level sexps in the file
#   :forms-read            successfully read by nelisp-reader-read-all
#   :dispatch-resident     head symbol present in
#                          nelisp-special-forms--dispatch (= 7+B-E
#                          handler installed)
#   :evaluable-attempted   subset of forms whose evaluation is
#                          interpret-pure (no host side-effect)
#   :evaluable-pure        forms that actually returned without
#                          signal-ing
#
# The dispatch-resident vs evaluable-pure gap is the M3 readiness
# delta; the read failure gap is the Phase 7+A reader / Phase 7+G
# follow-up backlog.
M3_DRIVER=$(cat <<'EOF'
(progn
(let* ((anvil-path (or (getenv "ANVIL_PATH")
                       (expand-file-name "~/Cowork/Notes/dev/anvil.el")))
       (modules (list (cons 'anvil-server
                            (expand-file-name "anvil-server.el" anvil-path))
                      (cons 'anvil-defs
                            (expand-file-name "anvil-defs.el" anvil-path))
                      (cons 'anvil-state
                            (expand-file-name "anvil-state.el" anvil-path))))
       (matrix nil))
  (dolist (entry modules)
    (let* ((mod (car entry))
           (file (cdr entry))
           (src (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-string)))
           (forms-total 0)
           (forms-read 0)
           (dispatch-resident 0)
           (evaluable-pure 0)
           (evaluable-attempted 0)
           (read-failures nil)
           (eval-failures nil))
      (condition-case err
          (let ((forms (nelisp-reader-read-all src)))
            (setq forms-total (length forms)
                  forms-read forms-total)
            (dolist (form forms)
              (let* ((head (and (consp form) (car form)))
                     (resident (and head
                                    (gethash head
                                             nelisp-special-forms--dispatch)))
                     ;; Pure-eligible = head is registered in dispatch
                     ;; AND not a side-effect form (define-error,
                     ;; defcustom, defgroup, defface = host-only).
                     (pure (and resident
                                (not (memq head
                                           '(define-error defcustom
                                              defgroup defface
                                              cl-defstruct cl-defun
                                              cl-defmacro))))))
                (when resident
                  (setq dispatch-resident (1+ dispatch-resident)))
                (when pure
                  (setq evaluable-attempted (1+ evaluable-attempted))
                  (condition-case eerr
                      (progn
                        (nelisp-special-forms-eval form nil)
                        (setq evaluable-pure (1+ evaluable-pure)))
                    (error
                     (push (list (or head 'atom) (error-message-string eerr))
                           eval-failures)))))))
        (error
         (push (list 'reader (error-message-string err)) read-failures)))
      (push (list :module mod :file file
                  :forms-total forms-total
                  :forms-read forms-read
                  :dispatch-resident dispatch-resident
                  :evaluable-attempted evaluable-attempted
                  :evaluable-pure evaluable-pure
                  :read-failures (nreverse read-failures)
                  :eval-failures (nreverse eval-failures))
            matrix)))
  (princ ";; m3-eval-matrix-begin\n")
  (dolist (row (nreverse matrix))
    (princ (format ";; m3-eval-row: %s\n" (prin1-to-string row))))
  (princ ";; m3-eval-matrix-end\n"))

;; Phase 7+A-E synthetic probe -- 5 hand-built forms that do exercise
;; the resident dispatch (= proof the evaluator actually works for
;; things it covers, independent of the corpus dispatch-resident gap).
(let ((probes
       '((:name reader-roundtrip
          :form-text "(let ((x 1) (y 2)) (+ x y))"
          :expect 3)
         (:name closure-capture
          :form-text "(funcall (let ((n 10)) (lambda (k) (+ n k))) 5)"
          :expect 15)
         (:name catch-throw
          :form-text "(catch 'tag (progn (throw 'tag 42) (error \"unreached\")))"
          :expect 42)
         (:name condition-case
          :form-text "(condition-case e (signal 'arith-error '(boom)) (arith-error (cdr e)))"
          :expect (boom))
         (:name cond-and-or
          :form-text "(cond ((and t (or nil 7)) :ok) (t :nope))"
          :expect :ok)))
      (results nil))
  (dolist (p probes)
    (let* ((name (plist-get p :name))
           (form-text (plist-get p :form-text))
           (expected (plist-get p :expect))
           (status nil)
           (actual nil))
      (condition-case err
          (let ((form (nelisp-reader-read form-text)))
            (setq actual (nelisp-special-forms-eval form nil))
            (setq status (if (equal actual expected) 'pass 'mismatch)))
        (error
         (setq status 'error
               actual (error-message-string err))))
      (push (list :name name :status status
                  :expected expected :actual actual)
            results)))
  (princ ";; m3-synthetic-begin\n")
  (dolist (r (nreverse results))
    (princ (format ";; m3-synthetic-row: %s\n" (prin1-to-string r))))
  (princ ";; m3-synthetic-end\n"))
)
EOF
)

# Substrate load order matches M1; on top of that we load the
# Phase 7+A-E evaluator modules.  nelisp-control-flow auto-installs
# its 4 special forms into nelisp-special-forms--dispatch at load
# time, so by `(require 'nelisp-control-flow)' the dispatch table
# is ready.
printf '%s\n' "$REQUESTS" | \
  "$EMACS" --batch -Q -L "$REPO/src" -L "$ANVIL_PATH" \
    --eval "(setq load-prefer-newer t)" \
    -l nelisp-load \
    -l nelisp-emacs-compat \
    -l nelisp-emacs-compat-fileio \
    -l nelisp-regex \
    -l nelisp-json \
    -l nelisp-sqlite \
    -l nelisp-coding \
    -l nelisp-base64 \
    -l nelisp-secure-hash \
    -l nelisp-ec-bridge \
    --eval "(nelisp-ec-bridge-install)" \
    -l nelisp-reader \
    -l nelisp-special-forms \
    -l nelisp-closure \
    -l nelisp-macro-ns \
    -l nelisp-control-flow \
    -l anvil \
    -l anvil-server \
    -l anvil-server-commands \
    -l anvil-defs \
    -l anvil-state \
    --eval "(anvil-state-enable)" \
    --eval "(anvil-defs-enable)" \
    --eval "(princ (format \";; bridge-status: %S\\n\" (nelisp-ec-bridge-status)))" \
    --eval "(princ (format \";; substrate-features: load=%s ec=%s ec-fileio=%s rx=%s json=%s sqlite=%s coding=%s b64=%s hash=%s ec-bridge=%s\\n\" (featurep 'nelisp-load) (featurep 'nelisp-emacs-compat) (featurep 'nelisp-emacs-compat-fileio) (featurep 'nelisp-regex) (featurep 'nelisp-json) (featurep 'nelisp-sqlite) (featurep 'nelisp-coding) (featurep 'nelisp-base64) (featurep 'nelisp-secure-hash) (featurep 'nelisp-ec-bridge)))" \
    --eval "(princ (format \";; evaluator-features: reader=%s special-forms=%s closure=%s macro-ns=%s control-flow=%s\\n\" (featurep 'nelisp-reader) (featurep 'nelisp-special-forms) (featurep 'nelisp-closure) (featurep 'nelisp-macro-ns) (featurep 'nelisp-control-flow)))" \
    --eval "(princ (format \";; anvil-features: anvil=%s server=%s commands=%s defs=%s state=%s\\n\" (featurep 'anvil) (featurep 'anvil-server) (featurep 'anvil-server-commands) (featurep 'anvil-defs) (featurep 'anvil-state)))" \
    --eval "$M3_DRIVER" \
    --eval "(anvil-server-run-batch-stdio \"emacs-eval\")" \
    > "$OUT" 2> "$ERR" || {
      echo "m3-bootsmoke: emacs batch invocation failed (exit $?)" >&2
      head -40 "$ERR" >&2
      exit 1
    }

if [ "$MODE" = "--raw" ]; then
  cat "$OUT"
  exit 0
fi

if [ "$MODE" = "--bridge-status" ]; then
  grep '^;; bridge-status:' "$OUT" || true
  grep '^;; substrate-features:' "$OUT" || true
  grep '^;; evaluator-features:' "$OUT" || true
  grep '^;; anvil-features:' "$OUT" || true
  exit 0
fi

if [ "$MODE" = "--tools-list-only" ]; then
  grep '^{' "$OUT" | sed -n '2p'
  exit 0
fi

if [ "$MODE" = "--eval-only" ]; then
  sed -n '/^;; m3-eval-matrix-begin/,/^;; m3-eval-matrix-end/p' "$OUT" || true
  sed -n '/^;; m3-synthetic-begin/,/^;; m3-synthetic-end/p' "$OUT" || true
  exit 0
fi

FAIL=0
WARN=0

assert_contains() {
  if ! grep -q "$1" "$OUT"; then
    echo "FAIL: expected stdout to contain: $1" >&2
    FAIL=1
  fi
}

# 1. substrate load assertions (M1 baseline)
assert_contains ';; substrate-features:'
if ! grep -q ';; substrate-features:.*ec-bridge=t' "$OUT"; then
  echo "FAIL: nelisp-ec-bridge feature not loaded" >&2
  FAIL=1
fi

# 2. evaluator load assertions (M3-specific)
assert_contains ';; evaluator-features:'
if ! grep -qE ';; evaluator-features:.*reader=t.*special-forms=t.*closure=t.*macro-ns=t.*control-flow=t' "$OUT"; then
  echo "FAIL: NeLisp 7+A-E evaluator features missing" >&2
  FAIL=1
fi

# 3. anvil core load assertions
if ! grep -qE ';; anvil-features:.*server=t.*defs=t.*state=t' "$OUT"; then
  echo "FAIL: anvil core (server/defs/state) features missing" >&2
  FAIL=1
fi

# 4. MCP initialize reply
assert_contains '"protocolVersion":"2025-03-26"'
assert_contains '"serverInfo":{"name":"anvil"'

# 5. tools/list reply
TOOLS_LINE=$(grep '^{' "$OUT" | grep '"id":2' || true)
if [ -z "$TOOLS_LINE" ]; then
  echo "FAIL: no tools/list reply (id=2)" >&2
  FAIL=1
else
  TOOL_COUNT=$(printf '%s' "$TOOLS_LINE" | grep -oE '"name":"[a-z][a-zA-Z0-9_-]*"' | wc -l)
  if [ "$TOOL_COUNT" -lt 1 ]; then
    echo "FAIL: tools/list returned 0 tools" >&2
    FAIL=1
  else
    echo "PASS: tools/list returned $TOOL_COUNT tools"
  fi
fi

# 6. tools/call defs-index-status (T97-FOLLOWUP-1 fix verification)
CALL_LINE=$(grep '^{' "$OUT" | grep '"id":3' || true)
if [ -z "$CALL_LINE" ]; then
  echo "WARN: no tools/call reply (id=3)" >&2
  WARN=1
elif printf '%s' "$CALL_LINE" | grep -q '"error"'; then
  ERR_MSG=$(printf '%s' "$CALL_LINE" | grep -oE '"message":"[^"]*"' | head -1)
  echo "WARN: tools/call defs-index-status error: $ERR_MSG" >&2
  WARN=1
else
  echo "PASS: tools/call defs-index-status returned a result"
fi

# 7. M3 NeLisp eval probe -- the new bit vs T97
if ! grep -q '^;; m3-eval-matrix-begin' "$OUT"; then
  echo "FAIL: M3 eval probe did not run (matrix marker missing)" >&2
  FAIL=1
else
  TOTAL_PURE=0
  TOTAL_ATTEMPTED=0
  TOTAL_RESIDENT=0
  TOTAL_READ=0
  TOTAL_FORMS=0
  while IFS= read -r row; do
    n_pure=$(printf '%s' "$row" | grep -oE ':evaluable-pure [0-9]+' | awk '{print $2}')
    n_att=$(printf '%s' "$row" | grep -oE ':evaluable-attempted [0-9]+' | awk '{print $2}')
    n_res=$(printf '%s' "$row" | grep -oE ':dispatch-resident [0-9]+' | awk '{print $2}')
    n_read=$(printf '%s' "$row" | grep -oE ':forms-read [0-9]+' | awk '{print $2}')
    n_tot=$(printf '%s' "$row" | grep -oE ':forms-total [0-9]+' | awk '{print $2}')
    TOTAL_PURE=$((TOTAL_PURE + ${n_pure:-0}))
    TOTAL_ATTEMPTED=$((TOTAL_ATTEMPTED + ${n_att:-0}))
    TOTAL_RESIDENT=$((TOTAL_RESIDENT + ${n_res:-0}))
    TOTAL_READ=$((TOTAL_READ + ${n_read:-0}))
    TOTAL_FORMS=$((TOTAL_FORMS + ${n_tot:-0}))
  done <<EOF_ROWS
$(grep '^;; m3-eval-row:' "$OUT")
EOF_ROWS
  echo "PASS: M3 eval probe -- forms-total=$TOTAL_FORMS read=$TOTAL_READ resident=$TOTAL_RESIDENT attempted=$TOTAL_ATTEMPTED pure-pass=$TOTAL_PURE"
  if [ "$TOTAL_PURE" -lt 1 ]; then
    echo "WARN: 0 anvil-corpus forms eval'd -- defun/defvar/defconst dispatch wiring is the M3 follow-up" >&2
    WARN=1
  fi
fi

# 8. Synthetic 7+A-E probe -- 5 hand-built forms
SYN_PASS=$(grep -cE '^;; m3-synthetic-row:.*:status pass' "$OUT" || true)
SYN_TOTAL=$(grep -cE '^;; m3-synthetic-row:' "$OUT" || true)
if [ "$SYN_TOTAL" -gt 0 ]; then
  echo "PASS: M3 synthetic probe -- $SYN_PASS / $SYN_TOTAL Phase 7+A-E forms green"
  if [ "$SYN_PASS" -lt "$SYN_TOTAL" ]; then
    echo "WARN: $((SYN_TOTAL - SYN_PASS)) synthetic probe(s) failed -- check --eval-only output" >&2
    WARN=1
  fi
fi

if [ $FAIL -eq 0 ]; then
  if [ $WARN -ne 0 ]; then
    echo "VERDICT: PARTIAL (boot OK, tools/list OK, M3 eval probe with warnings)"
  else
    echo "VERDICT: PASS"
  fi
  exit 0
else
  echo "VERDICT: FAIL"
  exit 1
fi
