#!/bin/sh
# Doc 210 S8.1 (chain) and S8.3 (jump): the shadow thread block behind the
# artifact's own current_thread_reloc chain, and the private setjmp/longjmp
# pair bound to its GOT slot.  Usage: nelisp-eln-handler-substrate-smoke.sh
# chain|jump.  Exit 0 only when every expected S8_* check and the PASS line
# appear with empty stderr.
set -eu
mode=${1:?usage: $0 chain|jump}
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
[ -x "$binary" ] || { echo "NELISP_BIN is not executable: $binary" >&2; exit 2; }
. "$script_dir/lib/nelisp-boot-args.sh"
. "$script_dir/lib/nelisp-eln-handler-fixture.sh"
nl_cold_image_setup "$binary" || exit 1
cache_root=${XDG_CACHE_HOME:-$HOME/.cache}
mkdir -p "$cache_root/tmp"
out_dir=$(mktemp -d "$cache_root/tmp/nelisp-eln-handler-substrate.XXXXXX")
trap 'rm -rf "$out_dir"' EXIT HUP INT TERM
cd "$out_dir"
rc=0
s8_run_driver nelisp-eln-handler-substrate-driver.el "$mode" \
  "$out_dir/out" "$out_dir/err" || rc=$?
case $mode in
  chain)
    s8_check_output "$out_dir/out" "$out_dir/err" "$rc" \
      NELISP-ELN-HANDLER-SUBSTRATE-CHAIN-PASS \
      CHAIN_UNDECLARED_REFUSED_PREOPEN CHAIN_SENTINEL_AT_0X68 \
      CHAIN_UNBOUND_IS_DEAD CHAIN_THREE_LOADS_REACH_SENTINEL \
      CHAIN_SECOND_BIND_REFUSED CHAIN_TAMPERED_THREAD_DETECTED \
      CHAIN_MOVED_HANDLERLIST_DETECTED CHAIN_RESTORED || exit 1
    grep '^NELISP-ELN-HANDLER-SUBSTRATE-CHAIN-PASS ' "$out_dir/out" | head -1 ;;
  jump)
    s8_check_output "$out_dir/out" "$out_dir/err" "$rc" \
      NELISP-ELN-HANDLER-SUBSTRATE-JUMP-PASS \
      JUMP_LD_SO_BOUND_TO_LIBC JUMP_CONTROL_GLIBC_LAYOUT_DIFFERS \
      JUMP_UNDECLARED_BIND_REFUSED JUMP_GOT_SLOT_READBACK \
      JUMP_GOT_PAGE_RW_PER_PROC_MAPS JUMP_SLOT_IS_FOURTH_GOT_PLT_WORD \
      JUMP_PRIVATE_LAYOUT_RIP_IN_CALLER JUMP_RETURNS_TWICE_VALUE_7 \
      JUMP_ZERO_NORMALIZED_TO_ONE JUMP_CLOBBERED_CANARY_DETECTED \
      JUMP_TAMPERED_GOT_READBACK_REFUSED JUMP_REBOUND_OK \
      JUMP_STILL_RETURNS_TWICE || exit 1
    grep '^NELISP-ELN-HANDLER-SUBSTRATE-JUMP-PASS ' "$out_dir/out" | head -1 ;;
  *) echo "unknown mode: $mode" >&2; exit 2 ;;
esac
