#!/bin/sh
# nelisp-dolist-listp-parity-smoke.sh --- native `dolist' GNU-semantics
# regression (JIT ledger S6.8, tools/ai/eln-progress.org).
#
# The standalone evaluator's native `dolist' special form
# (scripts/nelisp-standalone-build.el, `nl_sf_dolist') used to rewrite
# `(dolist (VAR LIST [RESULT]) BODY...)' into
#   (progn (mapc (lambda (VAR) BODY...) LIST) (let ((VAR nil)) RESULT))
# For a non-list LIST this signalled `(wrong-type-argument sequencep
# LIST)' from `mapc' -- and `mapc' also silently accepted vectors and
# strings that GNU's `dolist' rejects -- where GNU's own car/cdr-based
# expansion signals `(wrong-type-argument listp LIST)'.  This blocked S6.8
# (`cconv--set-diff'), whose corpus includes the row `(5 nil)'.
#
# `nl_sf_dolist' now builds GNU's own subr.el expansion instead (`(let
# ((tail LIST)) (while tail (let ((VAR (car tail))) BODY... (setq tail
# (cdr tail)))) RESULT)', confirmed byte-identical to `macroexpand' on
# host emacs-gtk 30.1 and emacs 31.1), via an uninterned `tail' gensym so
# it cannot capture/be captured by a user binding of the same name.  This
# smoke compares host GNU Emacs against the standalone binary for exactly
# the shapes that distinguish the two expansions: a genuine list, nil, a
# non-list atom, a vector, a dotted list, a RESULT form, a closure
# capturing VAR per iteration (GNU 31 lexical `dolist' rebinds VAR each
# time round the `while' body), and a non-local exit from BODY.
#
# Usage: NELISP_BIN=target/nelisp-dl sh test/nelisp-dolist-listp-parity-smoke.sh
# Optional: EMACS_BIN (host Emacs to compare against; default "emacs").

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
emacs_bin=${EMACS_BIN:-emacs}

if [ ! -x "$binary" ]; then
    echo "GATE-SKIP NELISP_BIN is not executable: $binary"
    exit 0
fi

checked=0
findings=0

# check NAME EXPR EXPECTED
#   EXPR is a Lisp form (as literal source text) that must be safe to
#   evaluate on both sides without signalling out of the top level (wrap
#   any expected error in `condition-case' inside EXPR itself).  Compares
#   host `emacs --batch --eval "(princ EXPR)"' against
#   `$binary --eval "EXPR"' (the standalone binary already prints its
#   `--eval' result inline), and also pins both against EXPECTED so a
#   shared, independently-wrong printer on both sides cannot pass silently.
check() {
    name=$1
    expr=$2
    expected=$3
    checked=$((checked + 1))
    host_out=$("$emacs_bin" --batch -Q --eval "(princ $expr)" 2>"$script_dir/.dolist-smoke-host.err") || {
        echo "FAIL $name: host emacs errored"
        cat "$script_dir/.dolist-smoke-host.err" >&2
        findings=$((findings + 1))
        rm -f "$script_dir/.dolist-smoke-host.err"
        return
    }
    rm -f "$script_dir/.dolist-smoke-host.err"
    standalone_out=$("$binary" --eval "$expr" 2>"$script_dir/.dolist-smoke-vm.err") || {
        echo "FAIL $name: standalone binary errored"
        cat "$script_dir/.dolist-smoke-vm.err" >&2
        findings=$((findings + 1))
        rm -f "$script_dir/.dolist-smoke-vm.err"
        return
    }
    rm -f "$script_dir/.dolist-smoke-vm.err"
    if [ "$host_out" != "$expected" ]; then
        echo "FAIL $name: host itself disagrees with the pinned EXPECTED (test bug, not a NeLisp regression)"
        printf '  host:     %s\n  expected: %s\n' "$host_out" "$expected"
        findings=$((findings + 1))
        return
    fi
    if [ "$standalone_out" != "$host_out" ]; then
        echo "FAIL $name: standalone diverges from host"
        printf '  host:       %s\n  standalone: %s\n' "$host_out" "$standalone_out"
        findings=$((findings + 1))
        return
    fi
    echo "PASS $name: $host_out"
}

# 1. list
check list \
    "(let (acc) (dolist (x (list 1 2 3)) (push x acc)) (nreverse acc))" \
    "(1 2 3)"

# 2. nil (empty list -- body never runs, RESULT defaults to nil)
check nil-list \
    "(let ((n 0)) (dolist (x nil) (setq n (1+ n))) n)" \
    "0"

# 3. a non-list atom -- the S6.8 corpus row (5 nil): GNU signals
#    (wrong-type-argument listp 5), not mapc's (wrong-type-argument
#    sequencep 5).
check non-list-atom \
    "(condition-case e (dolist (x 5) x) (error e))" \
    "(wrong-type-argument listp 5)"

# 4. a vector -- GNU's dolist rejects it (car requires listp); mapc
#    would have silently iterated its elements instead.
check vector \
    "(condition-case e (dolist (x (vector 1 2 3)) x) (error e))" \
    "(wrong-type-argument listp [1 2 3])"

# 5. a dotted list -- GNU's `while' only tests truthiness, so a non-nil,
#    non-cons cdr keeps the loop going into one more `(car tail)', which
#    is where the listp error on the dotted tail actually comes from,
#    after the two proper elements have already been processed.
check dotted-list \
    "(let (acc caught) (condition-case e (dolist (x (quote (1 2 . 3))) (push x acc)) (error (setq caught e))) (list (nreverse acc) caught))" \
    "((1 2) (wrong-type-argument listp 3))"

# 6. RESULT form
check result-form \
    "(dolist (x (list 1 2 3) (quote done)) x)" \
    "done"

# 7. closures capturing VAR -- GNU 31's lexical dolist rebinds VAR with a
#    fresh `let' every time round the `while' body, so each closure
#    captures its own iteration's value, not a single shared binding.
check closures-capture-var \
    "(let (funcs) (dolist (x (list 1 2 3)) (push (lambda () x) funcs)) (mapcar (function funcall) (nreverse funcs)))" \
    "(1 2 3)"

# 8. non-local exit from BODY -- `throw' out of the `while'/`let'/`progn'
#    shape must propagate exactly like GNU's, not get swallowed.
check non-local-exit \
    "(catch (quote done) (dolist (x (list 1 2 3 4 5)) (when (= x 3) (throw (quote done) x))) (quote never))" \
    "3"

echo "GATE-COUNT checked=$checked findings=$findings"
if [ "$findings" -gt 0 ]; then
    exit 1
fi
exit 0
