#!/bin/sh
# Doc 210 S8.4 (normal): the genuine host-compiled condition-case probe runs
# its no-error path on NeLisp and leaves handlerlist at the sentinel; the
# probe's result is compared with host GNU Emacs 31.1 running the same
# artifact.  Usage: nelisp-eln-handler-probe-smoke.sh normal.
set -eu
mode=${1:?usage: $0 normal}
[ "$mode" = normal ] || { echo "unknown mode: $mode" >&2; exit 2; }
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
emacs_bin=${EMACS_BIN:-emacs}
[ -x "$binary" ] || { echo "NELISP_BIN is not executable: $binary" >&2; exit 2; }
. "$script_dir/lib/nelisp-boot-args.sh"
. "$script_dir/lib/nelisp-eln-handler-fixture.sh"
nl_cold_image_setup "$binary" || exit 1
cache_root=${XDG_CACHE_HOME:-$HOME/.cache}
mkdir -p "$cache_root/tmp"
out_dir=$(mktemp -d "$cache_root/tmp/nelisp-eln-handler-probe.XXXXXX")
trap 'rm -rf "$out_dir"' EXIT HUP INT TERM
cd "$out_dir"

# Host transcript: the very same artifact under GNU Emacs 31.1, x bound.
"$emacs_bin" --batch -Q --eval "(progn (defvar s8-probe-x 1) \
(native-elisp-load \"$s8_eln\") \
(princ (format \"S8_PROBE_RESULT=%s\\n\" (s8-handler-probe 's8-probe-x))))" \
  >"$out_dir/host.out" 2>"$out_dir/host.err" || {
  cat "$out_dir/host.err" >&2; echo "host probe run failed" >&2; exit 1; }
[ "$(cat "$out_dir/host.out")" = "S8_PROBE_RESULT=ok" ] || {
  cat "$out_dir/host.out" >&2; echo "host probe result is not ok" >&2; exit 1; }

rc=0
s8_run_driver nelisp-eln-handler-probe-driver.el normal \
  "$out_dir/out" "$out_dir/err" || rc=$?
s8_check_output "$out_dir/out" "$out_dir/err" "$rc" \
  NELISP-ELN-HANDLER-PROBE-NORMAL-PASS \
  PROBE_CHAIN_AT_SENTINEL_BEFORE PROBE_GLIBC_RUN_RESULT \
  PROBE_CONTROL_UNBOUND_BUFFER_NOT_PRIVATE PROBE_GLIBC_RUN_HANDLERLIST_SENTINEL \
  PROBE_NORMAL_PATH_RESULT PROBE_HANDLER_BLOCK_GNU_LAYOUT \
  PROBE_SETJMP_WAS_THE_PRIVATE_PAIR PROBE_HANDLERLIST_AT_SENTINEL_AFTER \
  PROBE_SECOND_RUN PROBE_NEGATIVE_WRONG_CHAIN_DETECTED PROBE_CHAIN_RESET || exit 1
# Transcript equality: host and NeLisp print the identical result line.
grep -Fx "$(cat "$out_dir/host.out")" "$out_dir/out" >/dev/null || {
  echo "host/NeLisp transcripts differ: host=$(cat "$out_dir/host.out")" >&2
  grep '^S8_PROBE_RESULT=' "$out_dir/out" >&2 || true
  exit 1; }
grep '^NELISP-ELN-HANDLER-PROBE-NORMAL-PASS ' "$out_dir/out" | head -1
