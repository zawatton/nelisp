#!/bin/sh
# S7.7 slice 1: general crash-containment boundary for genuine .eln native
# calls. Unlike test/nelisp-eln-same-artifact-smoke.sh's cleanup-failure
# fixture (S7.6), which injects exactly one pin-end failure, this lane
# exercises `nelisp-eln-registration--containment-boundary' at independent
# injection points and non-local exit types (error/throw/quit), plus two
# negative controls, to demonstrate the boundary is not fixture-specific.
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
shared_lisp=${NELISP_SHARED_LISP:-$repo/lisp}
ffi_root=${NELISP_ELN_CRASH_GENERAL_FFI_ROOT:-$repo}
cache_root=${XDG_CACHE_HOME:-$HOME/.cache}
mkdir -p "$cache_root"
out_dir=$(mktemp -d "$cache_root/nelisp-eln-crash-general.XXXXXX")
keep_artifacts=${NELISP_ELN_KEEP_ARTIFACTS:-0}
eln=$out_dir/crash-general.eln
driver=$script_dir/nelisp-eln-crash-general-driver.el

cleanup() {
  status=$?
  if [ "$status" -eq 0 ] && [ "$keep_artifacts" != 1 ]; then
    rm -rf "$out_dir"
  else
    echo "ARTIFACT_DIR=$out_dir" >&2
  fi
}
trap cleanup EXIT HUP INT TERM

if [ ! -x "$binary" ]; then
  echo "NELISP_BIN is not executable: $binary" >&2
  exit 2
fi
if [ ! -r "$driver" ]; then
  echo "missing driver: $driver" >&2
  exit 2
fi

export NELISP_ROOT=$repo
export NELISP_ELN_CRASH_GENERAL_ELN=$eln

# Emit one genuine, self-emitted admitted-leaf .eln (same recipe as the
# S7.6 fixture in nelisp-eln-same-artifact-smoke.sh) and reuse it, unwritten,
# across every scenario below: each scenario fails before the registration
# body ever writes into the artifact's mapped memory, which the sha256
# check at the end confirms.
if ! "$binary" -L "$repo/lisp" \
    --eval '(progn (require (quote nelisp-eln-emitter)) (let ((ir (nelisp-aot-compiler--parse-stmt (quote (defun nelisp-eln-crash-general-fixture () 17)) nil nil nil))) (nelisp-eln-emitter-write-ir ir (getenv "NELISP_ELN_CRASH_GENERAL_ELN"))) (princ "NELISP-ELN-CRASH-GENERAL-EMIT-PASS\n"))' \
    >"$out_dir/emit.stdout" 2>"$out_dir/emit.stderr"; then
  cat "$out_dir/emit.stdout"
  cat "$out_dir/emit.stderr" >&2
  exit 1
fi
if [ -s "$out_dir/emit.stderr" ] || \
   ! grep -Fx 'NELISP-ELN-CRASH-GENERAL-EMIT-PASS' "$out_dir/emit.stdout" >/dev/null; then
  cat "$out_dir/emit.stdout"
  cat "$out_dir/emit.stderr" >&2
  echo "NeLisp did not emit the crash-general fixture cleanly" >&2
  exit 1
fi
before=$(sha256sum "$eln" | cut -d ' ' -f 1)

run_scenario() {
  scenario=$1
  expected=$2
  stdout=$out_dir/$scenario.stdout
  stderr=$out_dir/$scenario.stderr
  if ! NELISP_ELN_CRASH_SCENARIO=$scenario "$binary" \
      -L "$shared_lisp" -L "$ffi_root/packages/nl-ffi/src" \
      --load "$driver" >"$stdout" 2>"$stderr"; then
    cat "$stdout"
    cat "$stderr" >&2
    echo "scenario $scenario failed to run" >&2
    exit 1
  fi
  if [ -s "$stderr" ] || ! grep -Fx "$expected" "$stdout" >/dev/null; then
    cat "$stdout"
    cat "$stderr" >&2
    echo "scenario $scenario did not produce: $expected" >&2
    exit 1
  fi
}

# Injection point 1 (independent of pin-end): cleanup fails inside
# `nelisp-eln-registration--restore-data-relocations', root cause before
# any owner is rooted.
run_scenario cleanup-fail-early \
  'CRASH_GENERAL_cleanup_fail_early=owners:0,pending:1,inconsistencies:0,reentry-blocked:1'

# Injection point 2 (independent of pin-end and of the above): cleanup
# fails inside `nelisp-eln-registration-objects-release-unit', root cause
# after the owner is already rooted in --owners.
run_scenario cleanup-fail-owned \
  'CRASH_GENERAL_cleanup_fail_owned=owners:1,pending:1,inconsistencies:0,reentry-blocked:1'

# Non-error exit type 1: a `throw' as the root cause, with cleanup
# succeeding normally -- a full, silent rollback, not a quarantine.
run_scenario throw-clean-rollback \
  'CRASH_GENERAL_throw_clean_rollback=owners:0,pending:0,inconsistencies:0,reentry-blocked:0'

# Non-error exit type 2: a `quit' signal as the root cause, combined with
# the same cleanup failure as injection point 1.
run_scenario quit-cleanup-fail \
  'CRASH_GENERAL_quit_cleanup_fail=owners:0,pending:1,inconsistencies:0,reentry-blocked:1'

# Negative control (a): disabling the boundary makes the same assertions
# that pass in cleanup-fail-early FAIL -- nothing is recorded and
# subsequent registration is not blocked.
run_scenario disabled-cleanup-fail-early \
  'CRASH_GENERAL_disabled_cleanup_fail_early=owners:0,pending:0,inconsistencies:0,reentry-blocked:0'

# Negative control (b): a deliberately inconsistent registry (a
# failed-cleanup entry whose owner was never rooted) makes the reporter
# report it.
run_scenario inconsistent-registry \
  'CRASH_GENERAL_inconsistent_registry=inconsistencies:1,found:1'

after=$(sha256sum "$eln" | cut -d ' ' -f 1)
if [ "$before" != "$after" ]; then
  echo "a scenario wrote into the shared fixture artifact" >&2
  exit 1
fi

printf 'NELISP-ELN-CRASH-GENERAL-SMOKE-PASS %s\n' "$before"
