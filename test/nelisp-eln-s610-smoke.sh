#!/bin/sh
# Doc 210 S10 smokes on the genuine byte-compile-form artifact.  Usage:
#   nelisp-eln-s610-smoke.sh scenarios|forced|tamper|mutation|all
# scenarios  the shared scenarios (test/nelisp-eln-s610-scenarios.el) run on
#            stock Emacs 31.1 (the very same pinned .eln loaded by GNU's own
#            loader) and on NeLisp through the ordinary registration path; the
#            `T ' transcripts must be byte-identical; the shadow handler chain,
#            block pool and specpdl are clean after every scenario.
# forced     the same with the guarded regions made reachable (a live nil
#            constant made non-nil, no code byte touched): the native
#            condition-case really runs; transcripts identical, push_handler
#            and landing counts as expected, and a pre-change control (native
#            handlers disabled) must DIFFER.
# tamper     authenticated slots tampered one at a time fail closed.
# mutation   single-byte mutants of the artifact are refused by the loader.
# Exit 0 only with empty driver stderr.  NELISP_S10_ELN overrides the artifact.
set -eu
mode=${1:?usage: $0 scenarios|forced|tamper|mutation|all}
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
emacs_bin=${EMACS_BIN:-emacs}
eln=${NELISP_S10_ELN:-$HOME/.cache/tmp/s6-survey-lex/byte-compile-form/overlay/eln/31.1-ba35c031/gnu-byte-compile-form.eln}
eln_sha256=7109fe7ea0c4cd7b8b4cdaf20ce3e9cf3deea16f71361fcdba7c4f6c8889730d
[ -x "$binary" ] || { echo "NELISP_BIN is not executable: $binary" >&2; exit 2; }
[ -f "$eln" ] || { echo "artifact not found: $eln" >&2; exit 2; }
[ "$(sha256sum "$eln" | awk '{print $1}')" = "$eln_sha256" ] || {
  echo "artifact does not match its pinned sha256" >&2; exit 2; }
. "$script_dir/lib/nelisp-boot-args.sh"
nl_cold_image_setup "$binary" || exit 1
cache_root=${XDG_CACHE_HOME:-$HOME/.cache}
mkdir -p "$cache_root/tmp"
out_dir=$(mktemp -d "$cache_root/tmp/nelisp-eln-s610.XXXXXX")
trap 'rm -rf "$out_dir"' EXIT HUP INT TERM
cd "$out_dir"

# run_driver MODE OUT ERR [RED]
run_driver() {
  NELISP_ROOT=$repo NELISP_S10_ELN=$eln NELISP_S10_TESTDIR=$script_dir \
    NELISP_S10_MODE=$1 NELISP_S10_RED=${4:-} NELISP_S10_WORKDIR=$out_dir/mutants \
    "$binary" ${NL_COLD_IMAGE_PATH:+--cold-load-from "$NL_COLD_IMAGE_PATH"} \
    -L "$repo/lisp" -L "$repo/packages/nl-ffi/src" \
    --load "$script_dir/nelisp-eln-s610-driver.el" >"$2" 2>"$3"
}

# check_output OUT ERR RC PASS-PREFIX CHECK...
check_output() {
  out=$1; err=$2; rc=$3; pass=$4; shift 4
  missing=
  for check in "$@"; do
    grep -Fx "S10_$check=PASS" "$out" >/dev/null || missing="$missing $check"
  done
  if [ "$rc" -ne 0 ] || [ -n "$missing" ] || [ -s "$err" ] || \
     ! grep -q "^$pass" "$out"; then
    head -c 3000 "$out"
    head -c 3000 "$err" >&2
    echo "driver failed (rc=$rc) missing checks:$missing" >&2
    return 1
  fi
  return 0
}

host_transcript() {
  NELISP_S10_ELN=$eln NELISP_S10_TESTDIR=$script_dir \
    "$emacs_bin" --batch -Q -l "$script_dir/nelisp-eln-s610-host.el" \
    >"$out_dir/host.out" 2>"$out_dir/host.err" || {
    cat "$out_dir/host.err" >&2; echo "host transcript failed" >&2; exit 1; }
  [ ! -s "$out_dir/host.err" ] || { cat "$out_dir/host.err" >&2; exit 1; }
  grep -q '^T end$' "$out_dir/host.out" || { echo "incomplete host transcript" >&2; exit 1; }
  grep '^T ' "$out_dir/host.out" >"$out_dir/host.T"
}

compare_transcript() {
  grep '^T ' "$1" >"$out_dir/nelisp.T"
  cmp -s "$out_dir/host.T" "$out_dir/nelisp.T" || {
    diff "$out_dir/host.T" "$out_dir/nelisp.T" | head -30 >&2
    echo "host/NeLisp transcripts differ" >&2; exit 1; }
  echo "transcript lines: $(wc -l <"$out_dir/host.T") identical"
}

do_scenarios() {
  host_transcript
  rc=0
  run_driver scenarios "$out_dir/out" "$out_dir/err" || rc=$?
  check_output "$out_dir/out" "$out_dir/err" "$rc" NELISP-ELN-S610-SCENARIOS-PASS \
    ARTIFACT_REGISTERED LOADER_BOUND_THE_CHAIN LOADER_BOUND_THE_PRIVATE_SETJMP \
    HANDLER_ACTIVITY_CONSTANT CHAIN_AT_SENTINEL_CONSTANT_AFTER \
    NO_LIVE_BLOCKS_CONSTANT_AFTER NO_SPECPDL_LEFT_ERROR_RESTORES_SPECBINDS \
    NO_SPECPDL_LEFT_THROW_RESTORES_SPECBINDS NO_SPECPDL_LEFT_QUIT_RESTORES_SPECBINDS \
    NO_SPECPDL_LEFT_CAUGHT_THEN_ERROR_RESTORES SCENARIOS_COMPLETED || exit 1
  compare_transcript "$out_dir/out"
  grep '^NELISP-ELN-S610-SCENARIOS-PASS' "$out_dir/out" | head -1
}

do_forced() {
  host_transcript
  rc=0
  run_driver forced "$out_dir/out" "$out_dir/err" || rc=$?
  check_output "$out_dir/out" "$out_dir/err" "$rc" NELISP-ELN-S610-FORCED-PASS \
    ARTIFACT_REGISTERED LOADER_BOUND_THE_CHAIN LOADER_BOUND_THE_PRIVATE_SETJMP \
    FORCE_SLOT_IS_NIL FORCE_SLOT_NOW_NON_NIL \
    HANDLER_ACTIVITY_BOUND_SYMBOL_HEAD HANDLER_ACTIVITY_CONSTANT_SYMBOL_HEAD \
    HANDLER_ACTIVITY_CONSTANT_SYMBOL_HEAD_TWICE \
    HANDLER_ACTIVITY_CAUGHT_THEN_ERROR_RESTORES \
    CHAIN_AT_SENTINEL_CAUGHT_THEN_ERROR_RESTORES \
    NO_LIVE_BLOCKS_CAUGHT_THEN_ERROR_RESTORES \
    NO_SPECPDL_LEFT_CAUGHT_THEN_ERROR_RESTORES \
    NO_SPECPDL_LEFT_ERROR_RESTORES_SPECBINDS \
    NO_SPECPDL_LEFT_THROW_RESTORES_SPECBINDS \
    NO_SPECPDL_LEFT_QUIT_RESTORES_SPECBINDS SCENARIOS_COMPLETED || exit 1
  compare_transcript "$out_dir/out"
  # Pre-change control: with native handlers disabled the forced scenarios
  # must NOT reproduce the host transcript.
  rc=0
  run_driver forced "$out_dir/red.out" "$out_dir/red.err" 1 || rc=$?
  grep '^T ' "$out_dir/red.out" >"$out_dir/red.T" || true
  if cmp -s "$out_dir/host.T" "$out_dir/red.T"; then
    echo "red control reproduced the host transcript: the check has no teeth" >&2
    exit 1
  fi
  echo "S10_RED_CONTROL_DIFFERS=PASS"
  grep '^NELISP-ELN-S610-FORCED-PASS' "$out_dir/out" | head -1
}

do_tamper() {
  rc=0
  run_driver tamper "$out_dir/out" "$out_dir/err" || rc=$?
  check_output "$out_dir/out" "$out_dir/err" "$rc" NELISP-ELN-S610-TAMPER-PASS \
    TAMPER_BASELINE_CALL_WORKS TAMPER_LINK_TABLE_SLOT_REFUSED \
    TAMPER_LINK_TABLE_RESTORED TAMPER_FRELOC_CELL_REFUSED TAMPER_FRELOC_CELL_RESTORED \
    TAMPER_SETJMP_GOT_REFUSED TAMPER_SETJMP_GOT_RESTORED \
    TAMPER_CURRENT_THREAD_REFUSED TAMPER_CURRENT_THREAD_RESTORED \
    TAMPER_NO_HANDLER_ACTIVITY || exit 1
  grep '^NELISP-ELN-S610-TAMPER-PASS' "$out_dir/out" | head -1
}

do_mutation() {
  rc=0
  run_driver mutation "$out_dir/out" "$out_dir/err" || rc=$?
  check_output "$out_dir/out" "$out_dir/err" "$rc" NELISP-ELN-S610-MUTATION-PASS \
    MUTATION_START_NO_OWNER MUTATION_HANDLER_BYTES_REFUSED_PRE_DLOPEN \
    MUTATION_DECLARED_MUTANTS_REFUSED_BY_THE_TEMPLATE MUTATION_TOP_LEVEL_RUN_REFUSED \
    MUTATION_ARITY_REFUSED MUTATION_CONSTANTS_REFUSED \
    MUTATION_GENUINE_STILL_ADMITTED || exit 1
  grep '^NELISP-ELN-S610-MUTATION-PASS' "$out_dir/out" | head -1
}

case "$mode" in
  scenarios) do_scenarios ;;
  forced) do_forced ;;
  tamper) do_tamper ;;
  mutation) do_mutation ;;
  all) do_scenarios; do_forced; do_tamper; do_mutation ;;
  *) echo "unknown mode: $mode" >&2; exit 2 ;;
esac
