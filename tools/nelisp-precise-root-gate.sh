#!/usr/bin/env bash
# Precise root coverage for the mid-form collector.
#
# WHY THIS EXISTS.  `nl_gc_collect_recorded_mark_sweep' marks from two
# independent surfaces: the precise recorded-frame arms
# (`nl_gc_mark_recorded_frame' -- env / result / out / pool / src / cursor /
# bsym) and, in mode 0, a conservative native-stack scan
# (`nl_gc_conserv_maybe', SCAN_FLAG @268436464).  The shipped configuration
# runs both, and the scan is broad enough to keep alive anything a precise arm
# forgets.  That is good for production and terrible for verification: with the
# scan on, DELETING a precise root arm outright changes nothing observable, so
# no test over the shipped configuration can tell a sound root set from an
# unsound one.  Measured 2026-08-29: dropping the `result' arm entirely leaves
# the whole reader-smoke suite green.
#
# This gate removes the mask.  It runs a workload that fires several mid-form
# collections with the conservative scan disabled, so the precise arms are the
# only thing holding the executing form alive, and asserts the workload still
# computes the right answer.  Under that configuration the same `result'-arm
# deletion is an immediate SIGSEGV -- see the `precise-root-coverage' rows in
# tools/gate-mutations.txt.
#
# SCOPE.  This used to say the precise surface was NOT sufficient for real
# programs, because the anvil module load segfaulted in under a second with the
# scan off.  That is no longer true.  After the nine walkers below were closed,
# that same load runs to completion with the scan off -- 2/2 clean at 141 s for
# the plain path, and 3/3 at 186 s serving the MCP fast handshake with all
# 5,208 bytes of correct output.  The synthetic workload here is still what the
# gate runs, because it is seconds rather than minutes and it isolates one
# construct per case; the real load is the evidence that the set is no longer
# a toy.  It remains NOT a claim that the conservative scan is removable, and
# that has now been measured rather than left open.  A build with SCAN_FLAG
# (@268436464) initialised to 0 instead of 1 -- the scan off for every path,
# not just this gate's switch-28 cases -- run against the standalone tiers:
#
#   PASS   emacs-parity, binary-size-ratchet, selfhost, midform-gc-bounded,
#          chunk-growth, parallel-compile, and all 43 cases here
#   FAIL   standalone-reader-test: `compile-elisp-artifact --kind elc' dies
#          with `file-missing: #<object>' -- a clobbered Sexp where a filename
#          belongs, which is this defect's signature
#   FAIL   standalone-reader-ffi-unsupported-smoke, 1 of the 41 smokes:
#          expected `nl-ffi-call', got nothing
#
# Those two were artifact compilation and the FFI surface, and they are closed:
# the arg list needed rooting (`require' loses its own feature symbol while
# loading a file), the root stack needed a bound (it had none, and rooting one
# more slot per call walked it out of its region), and `catch' / `throw' each
# needed their tag rooted across the form they evaluate.  Re-run with the same
# scan-off build afterwards:
#
#   PASS   standalone-reader-test, all 41 reader smokes, selfhost, and every
#          case here -- the whole standalone tier, with the conservative
#          native-stack scan disabled at the driver
#   PASS   anvil's own module load serving the MCP fast handshake: exit 0 in
#          178 s with all 5,208 bytes of correct output
#
# The precise recorded-root surface now carries every workload this repository
# can point at.  That is still not a decision to remove the scan -- it is belt
# and braces for whatever no gate here exercises, and the cost of keeping it is
# a stack walk per collection -- but "the precise arms are not sufficient" has
# stopped being true, and any future claim that they are not should come with a
# failing command the way these did.
#
# WHERE THE BOUNDARY ACTUALLY IS, measured 2026-08-29 while trying to widen
# this workload.  The failing construct is three lines, and it is a binding
# form collecting inside its own value expression:
#
#     (nelisp--debug-switch 28)
#     (defun f () (garbage-collect) 7)
#     (setq a (f))            ; was rc=1; FIXED, and case 4 below holds it
#     (let ((x (f))) x)       ; was rc=1; FIXED, and case 5 below holds it
#
# Bare `(f)', `(if (f) 1 2)', `(+ 1 (f))', `(list (f) 2)', `(progn (f) 1)',
# `(cons (f) nil)', a `while' body and a nested `(defun g () (f)) (g)' all
# pass.  So the gap under the scan is not general -- it is the binding forms.
# It is also not mid-form-collector-specific: the same three lines fail with
# the collector disarmed (switch 6), because the collection here is an
# explicit `garbage-collect', which is what `anvil-runtime-shell--compat-load'
# calls after every file it loads.
#
# The structural cause is that `nl_cons_car_ptr' MATERIALISES a fresh 32-byte
# box for an immediate car (lisp/nelisp-cc-jit-cons-car-ptr.el) and hands back
# a raw pointer to it; nothing roots that box, and its own comment counts ~302
# consumer sites.  The mechanism to fix it already exists and is already used
# by `nl_eval_inner_cons': `nl_root_mark' / `nl_root_reserve' /
# `nl_root_release' (lisp/nelisp-cc-rootstack.el).  Rooting `let''s `val_slot'
# alone was tried and measured: it does NOT make the three lines pass, so the
# hole is elsewhere in the same chain and was not shipped on a guess.
#
# `setq' is closed: `nl_sf_setq' no longer takes `cdr(val-cdr)' before the
# eval, so the materialised view never exists across a collection (rooting it
# would have worked too; not creating it is cheaper and cannot be forgotten).
#
# `let' / `let*' are closed at top level the same way -- both walkers stopped
# taking the binding list's cdr before the value eval -- plus one rooted slot:
# `nl_let_collect_walk' also holds its `val_slot' across its own recursion,
# which evaluates the NEXT binding, so that one needed `nl_root_reserve'.  The
# cdr reorder alone was measured insufficient there.
#
# The LAMBDA BODY walker was the third instance and is closed the same way:
# `nl_ali_body' took the body list's cdr before evaluating the body form and
# carried it across.  `nl_ali_body_cdr' is gone; the cdr is taken in
# `nl_ali_body_step' after the eval returns.  That is what makes a `let',
# a `while' or an argument reference inside a function body work at all.
#
# READ THE FAILURE MODE BEFORE TRUSTING AN rc.  At the parent commit these
# returned WRONG VALUES rather than crashing:
#   (defun g () (f)) (g)            -> 47832544   instead of 7
#   (defun g (x) (+ x (f))) (g 1)   -> 7          instead of 8
#   ((lambda () (f)))               -> 47828840   instead of 7
# so every case below asserts the VALUE, not just exit status.  A gate here
# that only checked rc would have called all three green.
#
# `progn' was the FOURTH instance and is closed the same way.  It was missed
# twice by construct surveys that only tried `(progn (f) 1)', where the
# collecting form is not last and the cdr is a real cons -- the POSITION of the
# collecting form is part of the case, not a detail, which is why cases 7 below
# run all of them.
#
# ONE CASE REMAINS OPEN, measured on the binary that passes everything below:
#
#   ((lambda () (progn 1 (f))))                       ; SIGSEGV, 5/5 layouts
#
# It is a COMBINATION, not either construct: `((lambda () (f)))',
# `((lambda () 1 (f)))' and every `progn' row below all pass.  Two things
# separate it from the nine that were closed.  It does NOT move with process
# layout -- identical at five environment-padding sizes, where the original
# defect flipped between segfault, Lisp error and success.  And its backtrace
# is a wild jump rather than a call chain (`#0 0x...b09959 in ?? ()' with
# garbage frames under it), where every one of the nine was diagnosable from a
# single gdb frame.  Both say it is a different mechanism, not a tenth walker.
#
# Two cases that used to be listed here beside it -- a recursive `defun' and a
# `let' nested in a `let' -- turned out to be the SAME defect after all.
# Closing `if' and the `let' body fixed them with nothing aimed at either, and
# they are cases 10 below now.
#
# Widening this gate means closing them the same way, with each construct's
# three-line case added below as it goes green.
#
# It also pins the precondition for walking less of the reader parse pool than
# its whole capacity: with the pool slot walk suppressed (switch 26) the same
# workload must be byte-identical.  Measured 2026-08-29, the pool arm reaches
# nothing the other roots do not -- removing it entirely leaves every gate and
# the full anvil load green -- and this row is what would notice if that
# stopped being true.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

NELISP="${NELISP:-$REPO_ROOT/target/nelisp}"
TMP_DIR="$(mktemp -d)"
CHECKED=0
FINDINGS=0

# Every case records its own finding, so the trap only has to cover an
# unexpected abort (a `set -e' death before the summary) -- otherwise it would
# double-count the deliberate `exit 1' below.
cleanup() {
  status=$?
  if [ "$status" -ne 0 ] && [ "$FINDINGS" -eq 0 ]; then
    FINDINGS=1
  fi
  printf 'GATE-COUNT checked=%s findings=%s\n' "$CHECKED" "$FINDINGS"
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

if [ ! -x "$NELISP" ]; then
  echo "[precise-root] SKIP: no runnable binary at $NELISP"
  exit 0
fi

# ITERS is sized so the alloc-debt gate (16 MiB) fires several times inside one
# top-level form; the assertion is on a literal that lives only in that form, so
# a collection that loses the form loses the literal too.
ITERS="${NELISP_PRECISE_ROOT_ITERS:-250000}"
EXPECT=$((ITERS * 7))

write_workload() {  # $1 = file, $2 = switch prologue
  cat > "$1" <<EOF
$2
(setq acc 0)
(setq n 0)
(while (< n $ITERS)
  (setq junk (list n n n n n n n n n n n n n n n n))
  (setq acc (+ acc (car '(7 8 9))))
  (setq n (+ n 1)))
(nelisp--write-stderr-line (concat "ACC=" (number-to-string acc)))
(nelisp--write-stderr-line
 (concat "FIRED=" (number-to-string (nth 7 (nelisp--debug-switch 0)))))
EOF
}

run_case() {  # $1 = label, $2 = switch prologue, $3 = min collections required
  local label="$1" prologue="$2" min_fired="$3"
  local src="$TMP_DIR/$label.el" err="$TMP_DIR/$label.err"
  write_workload "$src" "$prologue"
  CHECKED=$((CHECKED + 1))
  local rc=0
  "$NELISP" "$src" >/dev/null 2>"$err" || rc=$?
  if [ "$rc" -ne 0 ]; then
    echo "precise_root_fail label=$label reason=nonzero-exit rc=$rc $(tr '\n' ' ' <"$err")" >&2
    FINDINGS=$((FINDINGS + 1))
    return 0
  fi
  local acc fired
  acc="$(sed -n 's/^ACC=//p' "$err")"
  fired="$(sed -n 's/^FIRED=//p' "$err")"
  if [ "$acc" != "$EXPECT" ]; then
    echo "precise_root_fail label=$label reason=wrong-answer got=$acc want=$EXPECT" >&2
    FINDINGS=$((FINDINGS + 1))
    return 0
  fi
  # A run that never collected proves nothing about root coverage.  This is the
  # "a gate that executed zero cases is not green" rule applied to the workload
  # itself rather than to the case count.
  if [ -z "$fired" ] || [ "$fired" -lt "$min_fired" ]; then
    echo "precise_root_fail label=$label reason=too-few-collections got=${fired:-none} want>=$min_fired" >&2
    FINDINGS=$((FINDINGS + 1))
    return 0
  fi
  echo "precise_root_result label=$label acc=$acc collections=$fired"
}

# 1. Baseline: the shipped configuration must agree with the expected answer,
#    so a failure in 2/3 cannot be blamed on the workload itself.
run_case shipped "" 1

# 2. The gate proper: precise arms only.  This is the case the `result'-arm
#    mutation turns red.
run_case no-conservative-scan "(nelisp--debug-switch 28)" 1

# 3. The pool arm carries no roots of its own, with the scan off so the scan
#    cannot be what makes that true.
run_case no-conservative-scan-no-pool-walk \
  "(nelisp--debug-switch 28)(nelisp--debug-switch 26)" 1

# 4. `setq' collecting inside its own value expression.  This was rc=1 until
#    `nl_sf_setq' stopped taking the cdr before the eval; it needs no loop and
#    no collection budget, because the `garbage-collect' is the whole case.
run_binding_case() {  # $1 = label, $2 = body, $3 = expected stderr word
  local label="$1" body="$2" want="$3"
  local src="$TMP_DIR/$label.el" err="$TMP_DIR/$label.err" rc=0
  {
    echo '(nelisp--debug-switch 28)'
    echo '(defun f () (garbage-collect) 7)'
    echo "$body"
  } > "$src"
  CHECKED=$((CHECKED + 1))
  "$NELISP" "$src" >/dev/null 2>"$err" || rc=$?
  if [ "$rc" -ne 0 ]; then
    echo "precise_root_fail label=$label reason=nonzero-exit rc=$rc $(tr '\n' ' ' <"$err")" >&2
    FINDINGS=$((FINDINGS + 1))
    return 0
  fi
  if ! grep -q "^$want\$" "$err"; then
    echo "precise_root_fail label=$label reason=missing-marker want=$want got=$(tr '\n' ' ' <"$err")" >&2
    FINDINGS=$((FINDINGS + 1))
    return 0
  fi
  echo "precise_root_result label=$label marker=$want"
}
run_binding_case setq-collects-in-value \
  '(setq a (f))
(nelisp--write-stderr-line (number-to-string a))' 7
run_binding_case setq-multi-pair \
  '(setq p 1 q 2 r 3)
(nelisp--write-stderr-line (number-to-string (+ p q r)))' 6

# 5. `let' / `let*' collecting inside a binding's value expression.  The
#    multi-binding rows matter as much as the single one: `nl_let_collect_walk'
#    holds its value slot across the recursion that evaluates the NEXT binding,
#    which is a second hazard the one-binding case never reaches.
run_binding_case let-collects-in-binding \
  '(nelisp--write-stderr-line (number-to-string (let ((x (f))) x)))' 7
run_binding_case letstar-collects-in-binding \
  '(nelisp--write-stderr-line (number-to-string (let* ((x (f))) x)))' 7
run_binding_case let-two-collecting-bindings \
  '(nelisp--write-stderr-line (number-to-string (let ((a (f)) (b (f))) (+ a b))))' 14
# letstar-dependent-binding was already green at the parent commit -- only its
# first binding collects, and `let*' binds each one before the next is
# evaluated, so nothing lives across the collection.  Kept anyway: it is the
# case that would notice if the reorder broke sequential binding order.
run_binding_case letstar-dependent-binding \
  '(nelisp--write-stderr-line (number-to-string (let* ((a (f)) (b (+ a 1))) (+ a b))))' 15
run_binding_case let-three-collecting-bindings \
  '(nelisp--write-stderr-line (number-to-string (let ((a (f)) (b (f)) (c (f))) (+ a (+ b c)))))' 21

# 6. Collections inside a FUNCTION BODY.  These are the lambda-body walker's
#    cases.  Three of them answered a wrong value rather than failing at the
#    parent commit, which is why the marker is checked and not just the exit
#    status.  Note the `defun' and the call are separate top-level forms on
#    purpose: wrapping them in a `progn' instead exercises `nl_sf_progn', which
#    still has this defect, and would make these cases fail for a reason that
#    has nothing to do with what they are here to hold.
run_binding_case body-one-form \
  '(defun g () (f))
(nelisp--write-stderr-line (number-to-string (g)))' 7
run_binding_case body-with-argument \
  '(defun g (x) (+ x (f)))
(nelisp--write-stderr-line (number-to-string (g 1)))' 8
run_binding_case body-let-called-twice \
  '(defun g () (let ((a (f)) (b (f))) (+ a b)))
(nelisp--write-stderr-line (number-to-string (+ (g) (g))))' 28
run_binding_case body-while-loop \
  '(defun g () (let ((n 0) (a 0)) (while (< n 3) (setq a (+ a (f))) (setq n (+ n 1))) a))
(nelisp--write-stderr-line (number-to-string (g)))' 21
run_binding_case bare-lambda-body \
  '(nelisp--write-stderr-line (number-to-string ((lambda () (f)))))' 7
run_binding_case bare-lambda-with-argument \
  '(nelisp--write-stderr-line (number-to-string ((lambda (x) (+ x (f))) 1)))' 8

# 7. `progn', with the collecting form in every position.  At the parent commit
#    the LAST-position rows answered 47826568 / 47826824 / 47827088 / 47827672
#    and the others were already correct -- the whole point of running all of
#    them.
run_binding_case progn-only-form \
  '(nelisp--write-stderr-line (number-to-string (progn (f))))' 7
run_binding_case progn-last-form \
  '(nelisp--write-stderr-line (number-to-string (progn 1 (f))))' 7
run_binding_case progn-first-form \
  '(nelisp--write-stderr-line (number-to-string (progn (f) 5)))' 5
run_binding_case progn-middle-form \
  '(nelisp--write-stderr-line (number-to-string (progn 1 (f) 5)))' 5
run_binding_case progn-three-forms \
  '(nelisp--write-stderr-line (number-to-string (progn 1 2 (f))))' 7
run_binding_case progn-nested \
  '(nelisp--write-stderr-line (number-to-string (progn 1 (progn 2 (f)))))' 7

# 8. The remaining walkers with an implicit body: `if's else branch and the
#    `let' / `let*' BODY (their BINDINGS were a separate fix).  Same shape,
#    found by the same position sweep, and at the parent commit the LAST-form
#    rows answered 47829128 / 47829384 / 47832584 / 47832200 while the
#    middle-and-first rows were already correct.
run_binding_case if-else-last-form \
  '(nelisp--write-stderr-line (number-to-string (if nil 1 (f))))' 7
run_binding_case if-else-three-forms \
  '(nelisp--write-stderr-line (number-to-string (if nil 1 2 (f))))' 7
run_binding_case if-else-middle-form \
  '(nelisp--write-stderr-line (number-to-string (if nil 1 (f) 3)))' 3
run_binding_case if-then-branch \
  '(nelisp--write-stderr-line (number-to-string (if 1 (f) 2)))' 7
run_binding_case if-test-collects \
  '(nelisp--write-stderr-line (number-to-string (if (f) 1 2)))' 1
run_binding_case if-nested-else \
  '(nelisp--write-stderr-line (number-to-string (if nil 1 (if nil 2 (f)))))' 7
run_binding_case let-body-last-form \
  '(nelisp--write-stderr-line (number-to-string (let ((a 1)) 2 (f))))' 7
run_binding_case let-body-first-form \
  '(nelisp--write-stderr-line (number-to-string (let ((a 1)) (f) 5)))' 5
run_binding_case let-binding-and-body \
  '(nelisp--write-stderr-line (number-to-string (let ((a (f))) 2 (f))))' 7
run_binding_case letstar-body-last-form \
  '(nelisp--write-stderr-line (number-to-string (let* ((a 1)) 2 (f))))' 7
run_binding_case letstar-body-first-form \
  '(nelisp--write-stderr-line (number-to-string (let* ((a 1)) (f) 5)))' 5
run_binding_case letstar-binding-and-body \
  '(nelisp--write-stderr-line (number-to-string (let* ((a (f)) (b (+ a 1))) 3 (f))))' 7

# 9. `while' bodies and `condition-case' handler bodies -- the last two walkers
#    the survey found.  At the parent commit the `while' rows SEGFAULTED and the
#    handler rows answered 47832200 / 47832464, while the not-last positions and
#    the condition and the protected form were all already correct.
run_binding_case while-body-last-form \
  '(nelisp--write-stderr-line (number-to-string (let ((n 0) (a 0)) (while (< n 2) (setq n (+ n 1)) (setq a (f))) a)))' 7
run_binding_case while-body-first-form \
  '(nelisp--write-stderr-line (number-to-string (let ((n 0) (a 0)) (while (< n 3) (setq a (+ a (f))) (setq n (+ n 1))) a)))' 21
run_binding_case while-condition-collects \
  '(nelisp--write-stderr-line (number-to-string (let ((n 0)) (while (< n (f)) (setq n (+ n 8))) n)))' 8
run_binding_case cc-handler-single-form \
  '(nelisp--write-stderr-line (number-to-string (condition-case nil (error "x") (error (f)))))' 7
run_binding_case cc-handler-last-form \
  '(nelisp--write-stderr-line (number-to-string (condition-case nil (error "x") (error 1 (f)))))' 7
run_binding_case cc-handler-first-form \
  '(nelisp--write-stderr-line (number-to-string (condition-case nil (error "x") (error (f) 5))))' 5
run_binding_case cc-protected-form-collects \
  '(nelisp--write-stderr-line (number-to-string (condition-case nil (f) (error 0))))' 7

# 10. Two cases the header used to list as open, closed as a side effect of the
#     `if' and `let'-body fixes rather than by anything aimed at them.  Held
#     here because nothing else in the set covers a recursive call or a `let'
#     nested in a `let', and because while they failed they looked like a
#     different class -- one segfaulted, the other quietly lost a binding.
#     5/5 across the layout sweep each.
run_binding_case recursive-defun-collects \
  '(defun g (n) (if (< n 1) (f) (g (- n 1))))
(nelisp--write-stderr-line (number-to-string (g 3)))' 7
run_binding_case nested-let-both-collect \
  '(nelisp--write-stderr-line (number-to-string (let ((a (f))) (let ((b (f))) (+ a b)))))' 14

# 11. `catch' bodies.  Two defects met here: the body walker took its cdr
#     before the eval like the nine before it, and TAG_SLOT -- which holds the
#     evaluated tag for the whole body -- was an `alloc-bytes' scratch, so a
#     collection in the body blanked the tag and a `throw' stopped matching its
#     own `catch'.  At the parent commit the first row answered 47827728.
run_binding_case catch-body-collects \
  '(nelisp--write-stderr-line (number-to-string (catch (quote tg) 1 (f))))' 7
run_binding_case catch-body-single-form \
  '(nelisp--write-stderr-line (number-to-string (catch (quote tg) (f))))' 7

# 12. `throw'.  Its TAG_SLOT holds the EVALUATED tag while the VALUE form is
#     evaluated, and that evaluation collects, so the tag came back blanked and
#     the throw missed its own `catch' -- `no-catch: (0 7)', a wrong answer
#     rather than a crash.  The value form's own car is now taken after the tag
#     eval for the same reason `progn' and the rest were changed.
run_binding_case throw-value-collects \
  '(nelisp--write-stderr-line (number-to-string (catch (quote tg) (throw (quote tg) (f)))))' 7
run_binding_case throw-inside-progn \
  '(nelisp--write-stderr-line (number-to-string (catch (quote tg) (progn 1 (throw (quote tg) (f))))))' 7
run_binding_case throw-past-inner-catch \
  '(nelisp--write-stderr-line (number-to-string (catch (quote a) (catch (quote b) (throw (quote a) (f))))))' 7
run_binding_case throw-literal-value \
  '(nelisp--write-stderr-line (number-to-string (catch (quote tg) (throw (quote tg) 5))))' 5

# 13. The combination that was the last one standing: a bare lambda whose body
#     is a `progn'.  It answered 47829904 at first, then segfaulted once
#     `progn' was closed, and neither construct alone accounted for it -- it
#     needed the builtin arg list and the catch/throw tags rooted as well.
run_binding_case lambda-wrapping-progn \
  '(nelisp--write-stderr-line (number-to-string ((lambda () (progn 1 (f))))))' 7
run_binding_case unwind-protect-body-collects \
  '(nelisp--write-stderr-line (number-to-string (unwind-protect (f) 1)))' 7

if [ "$FINDINGS" -ne 0 ]; then
  echo "precise-root-coverage: FAIL ($FINDINGS finding(s))"
  exit 1
fi
echo "precise-root-coverage: PASS ($CHECKED configurations, ${ITERS} iterations each)"
