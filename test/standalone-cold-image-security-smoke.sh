#!/usr/bin/env bash
# Regression coverage for the 2026-09-28 cold-image security fix
# (scripts/nelisp-standalone-build.el, `nl_cold_load_arena' /
# `nl_cold_overwrite_globals'):
#
#   1. A hostile file at the OLD fixed /tmp marker path
#      (/tmp/nelisp-cold-image.bin) must never be loaded -- with no
#      `--cold-load-from' flag, startup must behave exactly as if the file
#      did not exist, and (when `strace' is available) must not even
#      open() it.
#   2/3. An explicit `--cold-load-from PATH' pointed at a stale/mismatched
#      image (bad magic, or a SLEN outside the validated bounds) must be
#      REJECTED with a stderr diagnostic and fall through to normal boot,
#      not trusted.
#
# See docs/design/156-flat-arena-boot-install.org, "SECURITY FIX
# (2026-09-28)" status entry, for the full rationale.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
binary="${NELISP_BIN:-${1:-$repo_root/target/nelisp}}"
if [[ ! -x "$binary" && -x "$repo_root/target/nelisp-cold" ]]; then
  binary="$repo_root/target/nelisp-cold"
fi
if [[ ! -x "$binary" ]]; then
  echo "standalone-cold-image-security-smoke: missing executable (set NELISP_BIN)" >&2
  exit 2
fi

work="$(mktemp -d "${TMPDIR:-/tmp}/nelisp-coldimg-smoke.XXXXXX")"
legacy_marker="/tmp/nelisp-cold-image.bin"
legacy_marker_preexisting=0
legacy_marker_backup="$work/legacy-marker.orig"

cleanup() {
  if [[ "$legacy_marker_preexisting" == 1 ]]; then
    mv -f "$legacy_marker_backup" "$legacy_marker" 2>/dev/null || true
  else
    rm -f "$legacy_marker" 2>/dev/null || true
  fi
  rm -rf "$work"
}
trap cleanup EXIT

if [[ -e "$legacy_marker" ]]; then
  legacy_marker_preexisting=1
  cp -f "$legacy_marker" "$legacy_marker_backup"
fi

fail() { echo "FAIL: $*" >&2; exit 1; }

expect="3"

# ---------------------------------------------------------------------
# Test 1: hostile file at the OLD fixed /tmp marker path is not loaded.
# ---------------------------------------------------------------------
dd if=/dev/urandom of="$legacy_marker" bs=4096 count=1 status=none 2>/dev/null \
  || head -c 4096 /dev/urandom > "$legacy_marker"
chmod 666 "$legacy_marker" 2>/dev/null || true

got1="$("$binary" --eval '(+ 1 2)' 2>"$work/t1.err")" \
  || fail "process exited nonzero with a hostile legacy marker present (see $work/t1.err)"
[[ "$got1" == "$expect" ]] || fail "t1: expected $expect, got $got1"

if command -v strace >/dev/null 2>&1; then
  strace -f -e trace=openat,open -o "$work/t1.strace" \
    "$binary" --eval '(+ 1 2)' >/dev/null 2>"$work/t1.strace.err" || true
  if grep -q 'nelisp-cold-image[.]bin' "$work/t1.strace"; then
    fail "t1: startup opened the legacy /tmp marker path (should never touch it); see $work/t1.strace"
  fi
  echo "PASS t1 (strace-verified): legacy /tmp marker never opened, output=$got1"
else
  echo "PASS t1 (behavioral only, strace unavailable): output unaffected by hostile legacy marker, output=$got1"
fi

# `--cold-load-from PATH' does not dispatch to `--eval' -- it runs the same
# REPL-loop body as plain `--repl' (see docs/design/156-flat-arena-boot-
# install.org, Increment 2: "PATH consumes the arg2 slot ... the branch
# otherwise runs the identical REPL-loop body").  So probes below feed the
# form on stdin with `--no-prompt', matching how
# scripts/cold-image-org-e2e.sh already drives this flag.
run_cold_load_from() {
  local path="$1"
  printf '(+ 1 2)\n' | "$binary" --cold-load-from "$path" --no-prompt
}

# ---------------------------------------------------------------------
# Test 2: explicit --cold-load-from with a bad magic number is rejected.
# ---------------------------------------------------------------------
bad_magic="$work/bad-magic.bin"
python3 - "$bad_magic" <<'PY'
import struct, sys
with open(sys.argv[1], "wb") as f:
    # magic (wrong), slen, isz, tlen, goff, foff, uoff, ibase -- all in-bounds
    # except the magic itself, so this isolates the magic check.
    f.write(struct.pack("<8Q", 0xdeadbeef, 64, 0, 0, 0, 0, 0, 0))
PY

got2="$(run_cold_load_from "$bad_magic" 2>"$work/t2.err")" \
  || fail "process exited nonzero with a bad-magic cold image (see $work/t2.err)"
[[ "$got2" == "$expect" ]] || fail "t2: expected $expect, got $got2"
grep -qi 'cold-load rejected (invalid header)' "$work/t2.err" \
  || fail "t2: no rejection diagnostic on stderr (see $work/t2.err)"
echo "PASS t2: bad-magic cold image rejected with diagnostic, normal boot proceeded"

# ---------------------------------------------------------------------
# Test 3: explicit --cold-load-from with a valid magic but an SLEN outside
# the validated bounds (a stand-in for a mismatched/corrupt image) is
# rejected by the header bounds check, not just the magic check.
# ---------------------------------------------------------------------
bad_slen="$work/bad-slen.bin"
python3 - "$bad_slen" <<'PY'
import struct, sys
with open(sys.argv[1], "wb") as f:
    # magic OK (1179407692 = "FLAT"), slen far past the validated 8 GiB
    # ceiling (nl_cold_header_invalid_p in scripts/nelisp-standalone-build.el).
    f.write(struct.pack("<8Q", 1179407692, 1 << 40, 0, 0, 0, 0, 0, 0))
PY

got3="$(run_cold_load_from "$bad_slen" 2>"$work/t3.err")" \
  || fail "process exited nonzero with an oversized-SLEN cold image (see $work/t3.err)"
[[ "$got3" == "$expect" ]] || fail "t3: expected $expect, got $got3"
grep -qi 'cold-load rejected (invalid header)' "$work/t3.err" \
  || fail "t3: no rejection diagnostic on stderr (see $work/t3.err)"
echo "PASS t3: oversized-SLEN cold image rejected with diagnostic, normal boot proceeded"

# ---------------------------------------------------------------------
# Test 4: valid magic/header bounds but a relocation-table entry pointing
# outside the loaded chunk-0 region is rejected (nl_cold_reloc_table_valid_p)
# -- this is the arbitrary-offset-write primitive the fix closes even for a
# header that otherwise looks fine.
# ---------------------------------------------------------------------
bad_reloc="$work/bad-reloc.bin"
python3 - "$bad_reloc" <<'PY'
import struct, sys
# magic OK, slen=64 (in bounds), isz=0, tlen=1 (one reloc entry), rest 0.
hdr = struct.pack("<8Q", 1179407692, 64, 0, 1, 0, 0, 0, 0)
# one relocation-table entry: offset 1000, far outside the 64-byte region.
tbl = struct.pack("<Q", 1000)
region = b"\x00" * 64
with open(sys.argv[1], "wb") as f:
    f.write(hdr + tbl + region)
PY

got4="$(run_cold_load_from "$bad_reloc" 2>"$work/t4.err")" \
  || fail "process exited nonzero with an out-of-range relocation table (see $work/t4.err)"
[[ "$got4" == "$expect" ]] || fail "t4: expected $expect, got $got4"
grep -qi 'cold-load rejected (invalid relocation table)' "$work/t4.err" \
  || fail "t4: no rejection diagnostic on stderr (see $work/t4.err)"
echo "PASS t4: out-of-range relocation-table entry rejected with diagnostic, normal boot proceeded"

# ---------------------------------------------------------------------
# Test 5: a structurally valid header with no build-digest trailer, or with
# a wrong one, is rejected ("build digest mismatch") and the fallback boot
# still works.  5a: tlen=0, empty regions, no trailer.  5b/5c (only when a
# stamped "$binary.cold" exists): the real image with its last byte flipped
# (wrong digest), and with the trailer truncated away.
# ---------------------------------------------------------------------
no_trailer="$work/no-trailer.bin"
python3 - "$no_trailer" <<'PY'
import struct, sys
hdr = struct.pack("<8Q", 1179407692, 64, 0, 0, 0, 0, 0, 0)
with open(sys.argv[1], "wb") as f:
    f.write(hdr + b"\x00" * 64)
PY
check_t5() {
  local name="$1" img="$2" out
  out="$(run_cold_load_from "$img" 2>"$work/$name.err")" \
    || fail "$name: process exited nonzero (see $work/$name.err)"
  [[ "$out" == "$expect" ]] || fail "$name: expected $expect, got $out"
  grep -qi 'cold-load rejected (build digest mismatch)' "$work/$name.err" \
    || fail "$name: no digest-mismatch diagnostic (see $work/$name.err)"
}
check_t5 t5a "$no_trailer"
if [[ -f "$binary.cold" ]]; then
  bad_digest="$work/bad-digest.bin"
  cp "$binary.cold" "$bad_digest"
  python3 - "$bad_digest" <<'PY'
import sys
with open(sys.argv[1], "r+b") as f:
    f.seek(-1, 2); b = f.read(1)
    f.seek(-1, 2); f.write(bytes([b[0] ^ 0xff]))
PY
  check_t5 t5b "$bad_digest"
  cp "$binary.cold" "$work/no-trailer-real.bin"
  truncate -s -48 "$work/no-trailer-real.bin"
  check_t5 t5c "$work/no-trailer-real.bin"
  echo "PASS t5: missing/incorrect build-digest trailer rejected (synthetic, flipped digest, truncated)"
else
  echo "PASS t5a: header without trailer rejected (real image absent, 5b/5c skipped)"
fi

echo "PASS standalone-cold-image-security-smoke"
