#!/usr/bin/env bash
# Verify restored image state as well as the existing hardlink startup limit.
# A unique symbol is defined only in a scratch image. Both the single-link
# control and the two-link companion must restore it, exit successfully, emit
# exactly t plus newline, and leave stderr empty. The two-link boot must also
# finish in under 2 s. INTEG_WH_BOOT_TIMEOUT may lower the 30 s process deadline.
set -u
cd "$(dirname "$0")/../.." || exit 2
fail() { printf 'HARDLINK-BOOT FAIL: %s\n' "$*" >&2; exit 1; }
BIN=$(realpath "${1:-${INTEG_WH_DYN:-target/integ/nelisp-dyn}}") || exit 2
[ -x "$BIN" ] || fail "missing executable $BIN"
deadline=${INTEG_WH_BOOT_TIMEOUT:-30}
case "$deadline" in ''|*[!0-9]*) fail "timeout must be an integer from 1 to 30" ;; esac
[ "$deadline" -ge 1 ] && [ "$deadline" -le 30 ] || fail "timeout must be from 1 to 30 seconds"
T=$(mktemp -d "$PWD/target/hardlink-boot.XXXXXX") || exit 2
trap 'rm -rf "$T"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
for f in "$BIN" "$BIN".*; do
  [ -f "$f" ] || continue
  cp -p "$f" "$T/nelisp${f#"$BIN"}" || exit 2
done
printf 't\n' > "$T/expected"

run_checked() {
  local stage=$1 expression=$2 start rc=0
  start=$(date +%s%N)
  (ulimit -v 8388608; timeout --kill-after=1s "${deadline}s" \
    "$T/nelisp" --eval "$expression") > "$T/$stage.out" 2> "$T/$stage.err" || rc=$?
  ms=$(( ($(date +%s%N) - start) / 1000000 ))
  printf 'HARDLINK-BOOT %s %s ms exit %s\n' "$stage" "$ms" "$rc"
  if [ "$rc" -ne 0 ]; then
    printf 'HARDLINK-BOOT FAIL: %s child exited %s\n' "$stage" "$rc" >&2
    head -c 256 "$T/$stage.err" >&2
    exit "$rc"
  fi
  [ ! -s "$T/$stage.err" ] || fail "$stage: unexpected stderr"
  cmp -s "$T/expected" "$T/$stage.out" || \
    fail "$stage: expected image sentinel t plus newline, stdout=$(head -c 160 "$T/$stage.out")"
}

# The probe expression never defines this symbol: a fresh-prelude fallback
# cannot satisfy it. Verify the dumped image before changing its link count.
sentinel=nelisp-hardlink-boot-${T##*/}
# The relative path contains only mktemp's fixed prefix and random suffix,
# so checkout paths containing Lisp string delimiters need no interpolation.
image_relative=target/${T##*/}/sentinel.cold
run_checked dump "(progn (setq $sentinel t) (if (> (nelisp--arena-dump-image-stream \"$image_relative\") 0) t nil))"
[ -s "$T/sentinel.cold" ] || fail "dump: missing scratch image"
mv "$T/sentinel.cold" "$T/nelisp.cold" || exit 2
[ "$(stat -c %h "$T/nelisp.cold")" = 1 ] || fail "control image link count is not 1"
probe="(if (and (boundp '$sentinel) (eq (symbol-value '$sentinel) t)) t nil)"
run_checked single-link "$probe"
ln "$T/nelisp.cold" "$T/other.cold" || exit 2
[ "$(stat -c %h "$T/nelisp.cold")" = 2 ] || fail "companion image link count is not 2"
run_checked hardlink "$probe"
[ "$ms" -lt 2000 ] || fail "hardlink: ${ms} ms, expected under 2000"
printf 'HARDLINK-BOOT PASS: restored the image-only sentinel with companion link count 2\n'
