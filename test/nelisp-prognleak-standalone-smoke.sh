#!/bin/sh
# nelisp-prognleak-standalone-smoke.sh --- GNU-semantics regression for
# unknown top-level forms.
#
# The libgaps coverage lane found that loading a real GNU file (term.el)
# through an earlier build of the standalone reader could misreport an
# unhandled top-level form (an undefined macro/function called at top
# level, e.g. `easy-menu-define' before this repo defined it) as
# `void-variable progn' instead of the correct `void-function SYMBOL' (or
# correct evaluation once the operator IS defined).  `easy-menu-define' is
# now defined, which hides the original trigger, so this smoke uses a
# handful of genuinely undefined operators in several shapes instead:
#   - a bare top-level call
#   - nested `progn'
#   - `eval-and-compile' / `eval-when-compile'
#   - a macro called before its own `defmacro' later in the same file
#   - a name resolved through `defalias' (both a real macro and an
#     undefined target)
#
# Root cause fixed alongside this smoke (fix/prognleak): `nl_eval_source_all' /
# `bf_load_eval_loop' / `bf_eval_source_string_loop' never cleared the
# shared M6 signal-stash (flag@268435472 / TAG@268435480 / VAL@268435512)
# between top-level forms.  Any driver mode that keeps going after
# printing an uncaught error (prelude priming, the REPL -- see the
# `wf_write_int_checked' comment in scripts/nelisp-standalone-build.el for
# the historical incident this generalizes) could hand a LATER top-level
# form's own unstashed abort the PREVIOUS form's leftover tag/value
# instead of its own.  All three drivers now clear the stash before each
# form's own read+eval attempt, so every top-level form's error state
# starts clean, matching GNU Emacs's per-form `load' semantics.  This
# smoke locks that invariant in with a live standalone binary, both for
# each shape's own correct diagnostic (never `void-variable'/`progn') and
# for two independent, back-to-back uncaught errors in one REPL session.
#
# Usage: NELISP_BIN=target/nelisp-prog test/nelisp-prognleak-standalone-smoke.sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
out_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-prognleak-smoke.XXXXXX")
trap 'rm -rf "$out_dir"' EXIT HUP INT TERM

if [ ! -x "$binary" ]; then
    echo "GATE-SKIP NELISP_BIN is not executable: $binary"
    exit 0
fi

checked=0
findings=0

# check_load NAME SOURCE WANT_SYMBOL
#   Write SOURCE to a fixture, `--load' it, and require the stderr
#   diagnostic to name the class `void-function' and WANT_SYMBOL, and to
#   never mention `void-variable' or a bare `progn' misattribution.
check_load() {
    name=$1
    source=$2
    want_symbol=$3
    fixture="$out_dir/$name.el"
    printf '%s\n' "$source" >"$fixture"
    checked=$((checked + 1))
    if "$binary" --batch -Q --load "$fixture" \
        >"$out_dir/$name.out" 2>"$out_dir/$name.err"; then
        echo "FAIL $name: expected a nonzero exit (uncaught $want_symbol), got 0"
        cat "$out_dir/$name.err"
        findings=$((findings + 1))
        return
    fi
    if ! grep -q "void-function: ($want_symbol)" "$out_dir/$name.err"; then
        echo "FAIL $name: stderr does not name (void-function $want_symbol)"
        cat "$out_dir/$name.err"
        findings=$((findings + 1))
        return
    fi
    if grep -q 'void-variable' "$out_dir/$name.err"; then
        echo "FAIL $name: stderr leaked a void-variable diagnostic"
        cat "$out_dir/$name.err"
        findings=$((findings + 1))
        return
    fi
    echo "PASS $name"
}

check_load plain-undefined \
    '(nelisp-prognleak-undef-plain foo (bar) "doc")' \
    nelisp-prognleak-undef-plain

check_load nested-progn \
    '(progn (progn (progn (nelisp-prognleak-undef-progn foo (bar) "doc"))))' \
    nelisp-prognleak-undef-progn

check_load eval-and-compile \
    '(eval-and-compile (nelisp-prognleak-undef-eac foo (bar) "doc"))' \
    nelisp-prognleak-undef-eac

check_load eval-when-compile \
    '(eval-when-compile (nelisp-prognleak-undef-ewc foo (bar) "doc"))' \
    nelisp-prognleak-undef-ewc

check_load macro-defined-later \
    '(nelisp-prognleak-undef-fwd 1 2)
(defmacro nelisp-prognleak-undef-fwd (a b) (list (quote +) a b))' \
    nelisp-prognleak-undef-fwd

check_load defalias-undefined-target \
    "(defalias 'nelisp-prognleak-alias-undef 'nelisp-prognleak-undef-target)
(nelisp-prognleak-alias-undef 1 2)" \
    nelisp-prognleak-undef-target

# check_defalias_macro_works: a name resolved through `defalias' to a
# REAL macro must still dispatch as a macro (not fall through the same
# "unhandled" path the undefined cases above exercise) and evaluate
# correctly.
checked=$((checked + 1))
cat >"$out_dir/defalias-macro-works.el" <<'EOF'
(defmacro nelisp-prognleak-real-macro (a b)
  (list '+ a b))
(defalias 'nelisp-prognleak-alias-macro 'nelisp-prognleak-real-macro)
(if (= (nelisp-prognleak-alias-macro 3 4) 7)
    (princ "NELISP-PROGNLEAK-ALIAS-MACRO-OK\n")
  (princ "NELISP-PROGNLEAK-ALIAS-MACRO-BAD\n"))
EOF
if ! "$binary" --batch -Q --load "$out_dir/defalias-macro-works.el" \
    >"$out_dir/defalias-macro-works.out" 2>"$out_dir/defalias-macro-works.err"; then
    echo "FAIL defalias-macro-works: expected exit 0"
    cat "$out_dir/defalias-macro-works.err"
    findings=$((findings + 1))
elif ! grep -q 'NELISP-PROGNLEAK-ALIAS-MACRO-OK' "$out_dir/defalias-macro-works.out"; then
    echo "FAIL defalias-macro-works: alias-dispatched macro gave the wrong result:"
    cat "$out_dir/defalias-macro-works.out"
    findings=$((findings + 1))
else
    echo "PASS defalias-macro-works"
fi

# cross-form-isolation: two independent, back-to-back uncaught errors in
# one REPL session (report_errors=2, the mode that keeps going after
# printing) must each report THEIR OWN operator, never the other one's --
# the exact invariant `nl_eval_source_all''s per-form stash clear
# guarantees.  A trailing successful form must also still evaluate
# correctly after two uncaught errors.
checked=$((checked + 1))
repl_in="$out_dir/repl-input.el"
repl_out="$out_dir/repl.out"
repl_err="$out_dir/repl.err"
printf '(nelisp-prognleak-repl-undef-a)\n(nelisp-prognleak-repl-undef-b)\n(+ 40 3)\n' \
    >"$repl_in"
"$binary" --repl --no-prompt <"$repl_in" >"$repl_out" 2>"$repl_err" || true
cross_ok=1
# Each of the two independent uncaught errors must name ITS OWN operator.
# A contaminated run (the bug this locks in) drops one of these two lines
# -- e.g. the second error re-prints the first operator's stale tag/value
# instead of its own -- so `undef-b' never appears at all; verified via a
# fabricated contaminated log during development.
if ! grep -q 'void-function: (nelisp-prognleak-repl-undef-a)' "$repl_err"; then
    cross_ok=0
fi
if ! grep -q 'void-function: (nelisp-prognleak-repl-undef-b)' "$repl_err"; then
    cross_ok=0
fi
if ! grep -q '^43$' "$repl_out"; then
    cross_ok=0
fi
if [ "$cross_ok" -eq 1 ]; then
    echo "PASS cross-form-isolation"
else
    echo "FAIL cross-form-isolation: stale error state leaked across top-level forms"
    echo "--- repl stdout ---"; cat "$repl_out"
    echo "--- repl stderr ---"; cat "$repl_err"
    findings=$((findings + 1))
fi

echo "GATE-COUNT checked=$checked findings=$findings"
if [ "$findings" -gt 0 ]; then
    exit 1
fi
exit 0
