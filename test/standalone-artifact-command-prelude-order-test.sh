#!/usr/bin/env bash
# Regression test for the artifact-command family's version of the D3
# defect (see test/standalone-extend-runtime-image-restore-order-test.sh
# for the `extend-runtime-image' original): every `nl_artifact_command_p'
# CLI subcommand (compile-elisp-artifact, compile-elisp-artifacts,
# compile-runtime-image, exec-elisp-artifact, eval-elisp-artifact,
# load-elisp-source, eval-elisp-source, native-exec-elisp-artifact,
# inspect-elisp-artifact) replays an embedded bootstrap blob that loads
# `scripts/nelisp-stdlib-prelude.el', which pulls in `load-path-src''s
# un-deferred `(require 'easy-mmode)' and (for the non-inline `compile-
# elisp-artifact' etc. bootstrap) a genuine `(load .../nelisp-stdlib-
# prelude.el)' of the whole file.  `load' unconditionally calls `do-after-
# load-evaluation' on completion ("called directly from the C code", see
# `vendor/staged-emacs-lisp/subr.el') and THAT unconditionally calls
# `string-match-p' (not merely for a matching `after-load-alist' entry --
# every time, to check the file name against an `/obsolete/' regexp) --
# so every one of these commands aborted with `void-function: (string-
# match-p)' during the SHARED bootstrap, before any user-supplied source
# ever ran, whenever `string-match-p' was not yet fbound at that point.
# Fixed by `nelisp-standalone--artifact-match-compat-src' (the regexp
# engine + `string-match' family, extracted into one shared function), now
# called: (a) after `scripts/nelisp-stdlib-prelude.el' but with `load-
# path-src' deferred (DEFER-EASY-MMODE=t) and `easy-mmode'/`rx'/`cl-seq'
# required explicitly afterward -- matching `nelisp-standalone--reader-
# repl-prelude-source''s already-working order -- in
# `nelisp-standalone--artifact-command-runtime-src',
# `nelisp-standalone--artifact-source-command-cache-src', and (defensively;
# currently unreachable dead code) `nelisp-standalone--artifact-command-
# cache-src'; and (b) inside `nelisp-standalone--prelude-bootstrap-with-
# list-accessors' itself, before its own `(load PATH)' of the whole
# prelude file, which is a THIRD, independent place the same class of bug
# was reachable from (only for the non-inline artifact bootstrap -- e.g.
# `compile-elisp-artifact' -- since the inline variant never re-`load's
# its own text at all).
#
# `audit-elisp-artifacts' is deliberately NOT exercised here for the bug
# itself: its CLI dispatch (`nl_cstr_eq_audit_elisp_artifacts') shells out
# to tools/nelisp-audit-fast.sh via `nl_audit_fast_run' before the driver's
# `t' fallback arm is ever reached, so it never replays an embedded Lisp
# blob and cannot exhibit this defect class; it is still enumerated below
# so this script accounts for every member of `nl_artifact_command_p'.
#
# Two further defects found while validating that fix are now covered too:
# `compile-elisp-artifacts' (step 5) and `inspect-elisp-artifact' on a missing
# artifact (final negative control).  The artifact bootstrap takes ~20s, which
# an earlier 15-30s bound misread as a busy-CPU hang; `RUN_TIMEOUT_SECS' below
# bounds every invocation well above that so a real hang never blocks the suite.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
binary="${NELISP_BIN:-$repo_root/target/nelisp}"
if [[ ! -x "$binary" ]]; then
  echo "standalone-artifact-command-prelude-order-test: missing executable: $binary" >&2
  exit 2
fi

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

fail() {
  echo "standalone-artifact-command-prelude-order-test: FAIL -- $1" >&2
  exit 1
}

# Fails loudly and specifically if the D3-class regression is back, instead
# of only reporting "some command failed".
check_no_prelude_regression() {
  local label="$1" errfile="$2"
  if grep -q "void-function: (string-match-p)" "$errfile" 2>/dev/null; then
    fail "$label hit the artifact-command prelude-order regression (string-match-p void during scripts/nelisp-stdlib-prelude.el's own require)"
  fi
}

# Artifact commands replay the embedded bootstrap (~20s on a loaded host), so
# the bound must sit well above that while still catching a real hang.
RUN_TIMEOUT_SECS="${RUN_TIMEOUT_SECS:-120}"

run_ok() {
  # run_ok LABEL OUTFILE ERRFILE ARGS...
  local label="$1" outfile="$2" errfile="$3" rc=0
  shift 3
  timeout "$RUN_TIMEOUT_SECS" "$binary" "$@" >"$outfile" 2>"$errfile" || rc=$?
  if [[ "$rc" -eq 124 ]]; then
    fail "$label timed out after ${RUN_TIMEOUT_SECS}s (hung, not a prelude-order crash)"
  fi
  if [[ "$rc" -ne 0 ]]; then
    check_no_prelude_regression "$label" "$errfile"
    echo "--- $label stderr ---" >&2
    cat "$errfile" >&2
    fail "$label exited nonzero"
  fi
  check_no_prelude_regression "$label" "$errfile"
}

src="$tmp_dir/src.el"
printf '(defun ac-hot-fn (x) (+ x 1))\n' > "$src"

# 1. compile-elisp-artifact
nelc="$tmp_dir/src.nelc"
run_ok "compile-elisp-artifact" "$tmp_dir/compile.out" "$tmp_dir/compile.err" \
  compile-elisp-artifact --kind nelc --input "$src" --output "$nelc"
[[ -e "$nelc" ]] || fail "compile-elisp-artifact did not write $nelc"
[[ -e "$nelc.manifest.el" ]] || fail "compile-elisp-artifact did not write $nelc.manifest.el"

# 2. eval-elisp-artifact
run_ok "eval-elisp-artifact" "$tmp_dir/eval-art.out" "$tmp_dir/eval-art.err" \
  eval-elisp-artifact "$nelc" "(ac-hot-fn 41)"
[[ "$(cat "$tmp_dir/eval-art.out")" == "42" ]] \
  || fail "eval-elisp-artifact result mismatch: $(cat "$tmp_dir/eval-art.out") (expected 42)"

# 3. exec-elisp-artifact (same replay path, no stdout value contract)
run_ok "exec-elisp-artifact" "$tmp_dir/exec-art.out" "$tmp_dir/exec-art.err" \
  exec-elisp-artifact "$nelc" "(ac-hot-fn 1)"

# 4. inspect-elisp-artifact
run_ok "inspect-elisp-artifact" "$tmp_dir/inspect.out" "$tmp_dir/inspect.err" \
  inspect-elisp-artifact "$nelc"
grep -q "nelisp-elisp-artifact-manifest-v1" "$tmp_dir/inspect.out" \
  || fail "inspect-elisp-artifact manifest missing expected marker"

# 5. compile-elisp-artifacts (batch/plural form, single FILE.el input).
# The plural command wrote its artifact next to the source; it is a hard
# check (a busy-CPU hang here shows up as a `timeout' failure via run_ok).
run_ok "compile-elisp-artifacts" "$tmp_dir/compile-many.out" "$tmp_dir/compile-many.err" \
  compile-elisp-artifacts --kind nelc "$src"
grep -q "^compiled=1 failed=0 kind=nelc$" "$tmp_dir/compile-many.out" \
  || fail "compile-elisp-artifacts unexpected output: $(cat "$tmp_dir/compile-many.out")"
[[ -e "$src.nelc" ]] || fail "compile-elisp-artifacts did not write $src.nelc"
[[ -e "$src.nelc.manifest.el" ]] || fail "compile-elisp-artifacts did not write $src.nelc.manifest.el"

# 6/7. compile-runtime-image (dump-runtime-image itself is a separate,
# already-safe cond arm -- it never replays this blob -- but its output
# feeds compile-runtime-image, which does).
image="$tmp_dir/base.nlri"
run_ok "dump-runtime-image (setup)" "$tmp_dir/dump.out" "$tmp_dir/dump.err" \
  dump-runtime-image "$image" "(defun ac-rt-hot (x) (+ x 3))"
rt_nelc="$tmp_dir/rt.nelc"
run_ok "compile-runtime-image" "$tmp_dir/compile-rt.out" "$tmp_dir/compile-rt.err" \
  compile-runtime-image --kind nelc --input "$image" --output "$rt_nelc"
[[ -e "$rt_nelc" ]] || fail "compile-runtime-image did not write $rt_nelc"
run_ok "eval-elisp-artifact (runtime-image artifact)" \
  "$tmp_dir/eval-rt.out" "$tmp_dir/eval-rt.err" \
  eval-elisp-artifact "$rt_nelc" "(ac-rt-hot 39)"
[[ "$(cat "$tmp_dir/eval-rt.out")" == "42" ]] \
  || fail "eval-elisp-artifact (runtime-image artifact) result mismatch: $(cat "$tmp_dir/eval-rt.out") (expected 42)"

# 8/9. load-elisp-source / eval-elisp-source: a plain .el with no adjacent
# .neln/.nelc artifact, so these exercise the full source-command bootstrap
# (the exact reproduction from `nelisp-standalone--reader-temp-name-
# uniqueness-smoke', which is what first surfaced this defect under
# `make standalone-reader-test').
src2="$tmp_dir/src2.el"
# `defvar' evaluates to the SYMBOL name (matching real Emacs semantics),
# not its value -- append the bare variable so the file's own last value
# (what `load-elisp-source' prints) is 41.
printf '(defvar ac-source-smoke-var 41)\nac-source-smoke-var\n' > "$src2"
run_ok "load-elisp-source" "$tmp_dir/load-src.out" "$tmp_dir/load-src.err" \
  load-elisp-source "$src2"
[[ "$(cat "$tmp_dir/load-src.out")" == "41" ]] \
  || fail "load-elisp-source result mismatch: $(cat "$tmp_dir/load-src.out") (expected 41)"
run_ok "eval-elisp-source" "$tmp_dir/eval-src.out" "$tmp_dir/eval-src.err" \
  eval-elisp-source "$src2" "(+ ac-source-smoke-var 1)"
[[ "$(cat "$tmp_dir/eval-src.out")" == "42" ]] \
  || fail "eval-elisp-source result mismatch: $(cat "$tmp_dir/eval-src.out") (expected 42)"

# 10. native-exec-elisp-artifact: requires a real .neln (native ELF)
# artifact.  Attempted on a best-effort basis -- a toolchain-shaped
# failure (no cc/ld, unsupported host target) is reported and skipped
# rather than treated as a prelude-order regression, since this command's
# replay-blob bootstrap is identical to the others and is already
# exercised by compile-elisp-artifact/eval-elisp-artifact above; only the
# string-match-p regression itself is a hard failure here.
neln="$tmp_dir/src.neln"
if timeout "$RUN_TIMEOUT_SECS" "$binary" compile-elisp-artifact --kind neln --input "$src" --output "$neln" \
     >"$tmp_dir/compile-neln.out" 2>"$tmp_dir/compile-neln.err"; then
  run_ok "native-exec-elisp-artifact" "$tmp_dir/native-exec.out" "$tmp_dir/native-exec.err" \
    native-exec-elisp-artifact "$neln" "ac-hot-fn" "41"
  [[ "$(cat "$tmp_dir/native-exec.out")" == "42" ]] \
    || fail "native-exec-elisp-artifact result mismatch: $(cat "$tmp_dir/native-exec.out") (expected 42)"
else
  check_no_prelude_regression "compile-elisp-artifact --kind neln (setup)" "$tmp_dir/compile-neln.err"
  echo "standalone-artifact-command-prelude-order-test: SKIP native-exec-elisp-artifact (no --kind neln toolchain on this host):" >&2
  cat "$tmp_dir/compile-neln.err" >&2
fi

# 11. audit-elisp-artifacts: enumerated for completeness (see header); does
# not replay this blob, so it is exercised only as a sanity check, not a
# regression probe for this defect.
run_ok "audit-elisp-artifacts" "$tmp_dir/audit.out" "$tmp_dir/audit.err" \
  audit-elisp-artifacts "$src"

# --- Negative controls: a genuine error must stay a genuine, named error,
# never masked into (or replaced by) the string-match-p crash. ---
#
# Note: `eval-elisp-source'/`load-elisp-source' on a MISSING FILE.el is not
# used as a negative control here -- that goes through the Nelix hot-startup
# fast path (`nelisp-standalone-source-cache-load-source-command' et al,
# defined inline in `nelisp-standalone--artifact-source-command-cache-src'),
# which calls `nelisp-load-file' without an existence/readability check and
# silently returns nil (exit 0) for a missing source; this is pre-existing
# behavior this task's fix does not touch (the fix is about bootstrap
# ORDER, not this dispatch's own error handling), so it is not a useful
# probe for a masked prelude-order regression.  `eval-elisp-artifact' on a
# missing artifact (below) exercises the same match-compat-protected
# substrate and DOES have a clear, named-error contract.

set +e
timeout "$RUN_TIMEOUT_SECS" "$binary" eval-elisp-artifact "$tmp_dir/does-not-exist.nelc" "(+ 1 1)" \
  >"$tmp_dir/neg-eval-art.out" 2>"$tmp_dir/neg-eval-art.err"
neg_rc=$?
set -e
[[ "$neg_rc" -eq 124 ]] && fail "eval-elisp-artifact on a missing artifact timed out after ${RUN_TIMEOUT_SECS}s (hung)"
[[ "$neg_rc" -eq 1 ]] || fail "eval-elisp-artifact on a missing artifact exit=$neg_rc (expected 1)"
check_no_prelude_regression "eval-elisp-artifact (missing artifact, negative control)" "$tmp_dir/neg-eval-art.err"
grep -q "invalid artifact magic\|nelisp direct artifact error" "$tmp_dir/neg-eval-art.err" \
  || fail "eval-elisp-artifact on a missing artifact did not report the expected named error: $(cat "$tmp_dir/neg-eval-art.err")"

# `inspect-elisp-artifact' on a missing artifact must fail fast with a clear
# `file-missing' error naming the missing manifest.  It used to spin (or, once
# the read returned "", report a misleading "empty private artifact form").
set +e
timeout "$RUN_TIMEOUT_SECS" "$binary" inspect-elisp-artifact "$tmp_dir/does-not-exist.nelc" \
  >"$tmp_dir/neg-inspect.out" 2>"$tmp_dir/neg-inspect.err"
neg_rc=$?
set -e
[[ "$neg_rc" -eq 124 ]] && fail "inspect-elisp-artifact on a missing artifact timed out after ${RUN_TIMEOUT_SECS}s (hung)"
[[ "$neg_rc" -eq 1 ]] || fail "inspect-elisp-artifact on a missing artifact exit=$neg_rc (expected 1)"
check_no_prelude_regression "inspect-elisp-artifact (missing artifact, negative control)" "$tmp_dir/neg-inspect.err"
grep -q "No such file or directory.*does-not-exist.nelc.manifest.el" "$tmp_dir/neg-inspect.err" \
  || fail "inspect-elisp-artifact on a missing artifact did not report a file-missing error: $(cat "$tmp_dir/neg-inspect.err")"
if grep -q "empty private artifact form" "$tmp_dir/neg-inspect.err"; then
  fail "inspect-elisp-artifact on a missing artifact reported the misleading empty-form error"
fi

echo "standalone-artifact-command-prelude-order-test: PASS"
