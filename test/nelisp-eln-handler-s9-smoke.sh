#!/bin/sh
# Doc 210 S9 smokes.  Usage: nelisp-eln-handler-s9-smoke.sh
#   port|match|unwind|gc|debugger|negatives|all
# port      S9.1  the push_handler port (unit + the consuming longjmp stub)
# match     S9.2  find_handler_clause matching + the Doc 207 non-matching path
# unwind    S9.3  specpdl unwinding order, cleanups that raise, nested activations
# negatives S9.4  adapter divert exactly once, forged chain / replayed buffer
# gc              GC in the guarded region and in a cleanup
# debugger  S9.6  handler-bind and the debugger decision in GNU order
# all       S9.5  every scenario group, host vs NeLisp
# Each transcript mode runs the same scenarios on stock Emacs 31.1 (the very
# same pinned .eln loaded by GNU's own loader) and on NeLisp, requires the two
# `T ' transcripts to be byte-identical, and runs a pre-change control (native
# handlers disabled) that must DIFFER.  Exit 0 only with empty driver stderr.
set -eu
mode=${1:?usage: $0 port|match|unwind|gc|debugger|negatives|all}
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
emacs_bin=${EMACS_BIN:-emacs}
[ -x "$binary" ] || { echo "NELISP_BIN is not executable: $binary" >&2; exit 2; }
. "$script_dir/lib/nelisp-boot-args.sh"
. "$script_dir/lib/nelisp-eln-handler-s9-fixture.sh"
nl_cold_image_setup "$binary" || exit 1
cache_root=${XDG_CACHE_HOME:-$HOME/.cache}
mkdir -p "$cache_root/tmp"
out_dir=$(mktemp -d "$cache_root/tmp/nelisp-eln-handler-s9.XXXXXX")
trap 'rm -rf "$out_dir"' EXIT HUP INT TERM
cd "$out_dir"

# transcript GROUP CHECK... : host vs NeLisp identical, red control differs.
transcript() {
  group=$1; shift
  s9_host_transcript "$group" "$out_dir/host.out" "$out_dir/host.err" || {
    cat "$out_dir/host.err" >&2; echo "host transcript failed" >&2; exit 1; }
  [ ! -s "$out_dir/host.err" ] || { cat "$out_dir/host.err" >&2; exit 1; }
  grep -q '^T scenario ' "$out_dir/host.out" || { echo "empty host transcript" >&2; exit 1; }
  rc=0
  s9_run_driver "$group" "$out_dir/out" "$out_dir/err" || rc=$?
  s9_check_output "$out_dir/out" "$out_dir/err" "$rc" \
    "NELISP-ELN-HANDLER-S9-$(echo "$group" | tr a-z A-Z)-PASS" "$@" || exit 1
  grep '^T ' "$out_dir/host.out" >"$out_dir/host.T"
  grep '^T ' "$out_dir/out" >"$out_dir/nelisp.T"
  cmp -s "$out_dir/host.T" "$out_dir/nelisp.T" || {
    diff "$out_dir/host.T" "$out_dir/nelisp.T" | head -30 >&2
    echo "host/NeLisp transcripts differ ($group)" >&2; exit 1; }
  # Pre-change control: with native handlers disabled the same scenarios
  # must NOT reproduce the host transcript.
  rc=0
  s9_run_driver "$group" "$out_dir/red.out" "$out_dir/red.err" 1 || rc=$?
  grep '^T ' "$out_dir/red.out" >"$out_dir/red.T" || true
  if cmp -s "$out_dir/host.T" "$out_dir/red.T"; then
    echo "red control reproduced the host transcript ($group): the check has no teeth" >&2
    exit 1
  fi
  echo "S9_RED_CONTROL_DIFFERS=PASS"
  echo "transcript lines: $(wc -l <"$out_dir/host.T") identical"
  grep "^NELISP-ELN-HANDLER-S9-$(echo "$group" | tr a-z A-Z)-PASS " "$out_dir/out" | head -1
}

case $mode in
  port)
    rc=0
    s9_run_driver port "$out_dir/out" "$out_dir/err" || rc=$?
    s9_check_output "$out_dir/out" "$out_dir/err" "$rc" NELISP-ELN-HANDLER-PORT-STUB-PASS \
      PORT_RESUME_OFFSET_IS_672 PORT_ENTRY_TYPE_0_FAILS_CLOSED \
      PORT_RETIRE_RESTORES_BASE_AND_RELEASES PORT_ENTRY_UNAUTHENTICATED_TAG_FAILS_CLOSED \
      PORT_FAILED_PUSHES_LEAVE_NO_TRACE PORT_MINTS_GNU_LAYOUT PORT_RECORDS_PDLCOUNT \
      PORT_LINKS_THREAD_HANDLERLIST PORT_BLOCK_IS_LIVE_AND_ALIGNED \
      PORT_SECOND_PUSH_LINKS_TO_FIRST PORT_REJECTS_TYPE_0 PORT_REJECTS_TYPE_2 \
      PORT_REJECTS_TYPE_3 PORT_REJECTS_TYPE_4 PORT_REJECTS_TYPE_5 PORT_REJECTS_TYPE_6 \
      PORT_REJECTS_TYPE_99 PORT_REJECTION_LEAVES_NO_TRACE \
      PORT_REJECTS_NON_HANDLER_ACTIVATION PORT_RETIRE_UNPOPPED_FAILS_CLOSED \
      PORT_ALL_BLOCKS_RELEASED STUB_CONSUME_RETURNS_TWICE_WITH_VALUE \
      STUB_CONSUME_ZEROES_THE_BUFFER || exit 1
    # The adapter's resume block offset must equal the constant the Lisp side uses.
    "$emacs_bin" --batch -Q -l "$repo/lisp/nelisp-cc-eln-callback7.el" \
      --eval '(unless (= nelisp-cc-eln-callback7-resume-offset 672) (kill-emacs 1))' || {
      echo "adapter resume offset is not 672" >&2; exit 1; }
    grep '^NELISP-ELN-HANDLER-PORT-STUB-PASS ' "$out_dir/out" | head -1 ;;
  match)
    transcript match GROUP_MATCH_QUIESCENT GROUP_MATCH_LANDED ;;
  unwind)
    transcript unwind GROUP_UNWIND_QUIESCENT GROUP_UNWIND_LANDED ;;
  gc)
    transcript gc GROUP_GC_QUIESCENT GROUP_GC_LANDED ;;
  debugger)
    transcript debugger GROUP_DEBUGGER_QUIESCENT GROUP_DEBUGGER_LANDED ;;
  all)
    transcript all GROUP_ALL_QUIESCENT GROUP_ALL_LANDED ;;
  negatives)
    rc=0
    s9_run_driver negatives "$out_dir/out" "$out_dir/err" || rc=$?
    s9_check_output "$out_dir/out" "$out_dir/err" "$rc" \
      NELISP-ELN-HANDLER-S9-NEGATIVES-PASS \
      STUB_CONSUME_RETURNS_TWICE_WITH_VALUE STUB_CONSUME_ZEROES_THE_BUFFER \
      NEG_CONTROL_CLEAN_RUN_LANDS_ONCE \
      NEG_FORGED-HEAD_FAILS_CLOSED NEG_FORGED-HEAD_NO_LANDING NEG_FORGED-HEAD_QUIESCENT \
      NEG_FORGED-NEXT_FAILS_CLOSED NEG_FORGED-NEXT_NO_LANDING NEG_FORGED-NEXT_QUIESCENT \
      NEG_FORGED-TYPE_FAILS_CLOSED NEG_FORGED-TYPE_NO_LANDING NEG_FORGED-TYPE_QUIESCENT \
      NEG_FORGED-PDLCOUNT_FAILS_CLOSED NEG_FORGED-PDLCOUNT_NO_LANDING \
      NEG_FORGED-PDLCOUNT_QUIESCENT NEG_FORGED-THREAD-CELL_FAILS_CLOSED \
      NEG_FORGED-THREAD-CELL_NO_LANDING NEG_FORGED_CHAIN_RECOVERED \
      NEG_RIP-OUTSIDE-BODY_FAILS_CLOSED NEG_RIP-OUTSIDE-BODY_NO_LANDING \
      NEG_RIP-OUTSIDE-BODY_QUIESCENT NEG_RSP-MISALIGNED_FAILS_CLOSED \
      NEG_RSP-MISALIGNED_NO_LANDING NEG_RSP-MISALIGNED_QUIESCENT \
      NEG_RSP-TOO-FAR_FAILS_CLOSED NEG_RSP-TOO-FAR_NO_LANDING NEG_RSP-TOO-FAR_QUIESCENT \
      NEG_RSP-BELOW-ADAPTER_FAILS_CLOSED NEG_RSP-BELOW-ADAPTER_NO_LANDING \
      NEG_RSP-BELOW-ADAPTER_QUIESCENT NEG_UNPOPPED_FAILS_CLOSED NEG_UNPOPPED_NO_LANDING \
      NEG_UNPOPPED_QUIESCENT NEG_ADAPTER-RIP-OUTSIDE_REFUSED_BY_ADAPTER \
      NEG_ADAPTER-RIP-OUTSIDE_NO_LANDING NEG_ADAPTER-RIP-OUTSIDE_QUIESCENT \
      NEG_ADAPTER-RSP-WRONG_REFUSED_BY_ADAPTER NEG_ADAPTER-RSP-WRONG_NO_LANDING \
      NEG_ADAPTER-RSP-WRONG_QUIESCENT NEG_ADAPTER-OUTSIDE-REGION_REFUSED_BY_ADAPTER \
      NEG_ADAPTER-OUTSIDE-REGION_NO_LANDING NEG_ADAPTER-OUTSIDE-REGION_QUIESCENT \
      NEG_ADAPTER-MISALIGNED_REFUSED_BY_ADAPTER NEG_ADAPTER-MISALIGNED_NO_LANDING \
      NEG_ADAPTER-MISALIGNED_QUIESCENT NEG_REPLAY_BLOCK_IS_THE_LANDED_ONE \
      NEG_REPLAY_BLOCK_WAS_RELEASED NEG_REPLAY_REFUSED_BY_ADAPTER NEG_REPLAY_NO_LANDING \
      NEG_REPLAY_QUIESCENT NEG_RED_CONTROL_NO_HANDLERS_PROPAGATES \
      NEG_RED_CONTROL_QUIESCENT NEG_NON_HANDLER_ACTIVATION_REFUSED \
      NEG_NON_HANDLER_ACTIVATION_QUIESCENT || exit 1
    grep '^NELISP-ELN-HANDLER-S9-NEGATIVES-PASS ' "$out_dir/out" | head -1 ;;
  *) echo "unknown mode: $mode" >&2; exit 2 ;;
esac
