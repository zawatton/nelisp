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
#      REJECTED with a stderr diagnostic, not trusted.  Since 65e3ff534 an
#      explicit `--cold-load-from' that cannot be loaded is FATAL (exit 1,
#      "could not load dump file", like GNU load_pdump); only the
#      executable's own `.cold' companion falls back to a normal boot (t2c).
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

# An explicit `--cold-load-from PATH' of an unloadable image is fatal:
# exit 1, the loader's own rejection line, then GNU's "could not load dump
# file" line, and no evaluation output.  $1 name, $2 image, $3 diag regex.
expect_fatal() {
  local name="$1" img="$2" diag="$3" out rc
  set +e
  out="$(run_cold_load_from "$img" 2>"$work/$name.err")"
  rc=$?
  set -e
  [[ "$rc" == 1 ]] || fail "$name: expected exit 1, got $rc (see $work/$name.err)"
  [[ "$out" != *"$expect"* ]] || fail "$name: evaluation ran after a rejected image: $out"
  grep -qi "$diag" "$work/$name.err" \
    || fail "$name: no rejection diagnostic /$diag/ on stderr (see $work/$name.err)"
  grep -q 'could not load dump file ".*": not a dump file' "$work/$name.err" \
    || fail "$name: no fatal 'could not load dump file' line (see $work/$name.err)"
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

expect_fatal t2 "$bad_magic" 'cold-load rejected (invalid header)'
echo "PASS t2: bad-magic explicit cold image rejected, fatal exit 1"

# Test 2b: an explicit --cold-load-from of a truncated real image is
# rejected after the loader has grown chunk 0; still fatal, exit 1.
if [[ -f "$binary.cold" ]]; then
  trunc="$work/truncated.bin"
  head -c "$(( $(stat -c %s "$binary.cold") / 2 ))" "$binary.cold" > "$trunc"
  expect_fatal t2b "$trunc" 'cold-load rejected'
  echo "PASS t2b: truncated explicit cold image rejected, fatal exit 1"
else
  echo "SKIP t2b: no $binary.cold"
fi

# Test 2c: the executable's OWN companion `.cold' falls back to a normal
# boot when it is rejected after chunk 0 was grown.  The companion must be
# mode 644: a group/other-writable companion is skipped before any load
# attempt (nl_boot_image_pin) and would not exercise the reject path.
# Regression: the grown chunk 0 used to stay mapped and the fallback boot
# spun forever in mid-form GC.
if [[ -f "$binary.cold" ]]; then
  c2="$work/t2c"; mkdir -p "$c2"
  cp "$binary" "$c2/nelisp"
  head -c "$(( $(stat -c %s "$binary.cold") / 2 ))" "$binary.cold" > "$c2/nelisp.cold"
  chmod 755 "$c2/nelisp"; chmod 644 "$c2/nelisp.cold"
  set +e
  got2c="$(cd "$c2" && timeout 30 ./nelisp --eval '(+ 1 2)' 2>"$work/t2c.err" </dev/null)"
  rc2c=$?
  set -e
  [[ "$rc2c" == 0 ]] || fail "t2c: expected exit 0, got $rc2c (124 = hang) (see $work/t2c.err)"
  [[ "$got2c" == "$expect" ]] || fail "t2c: expected $expect, got $got2c"
  grep -qi 'cold-load rejected' "$work/t2c.err" \
    || fail "t2c: reject path not exercised, no diagnostic (see $work/t2c.err)"
  echo "PASS t2c: rejected companion image falls back to normal boot, output=$got2c"
else
  echo "SKIP t2c: no $binary.cold"
fi

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

expect_fatal t3 "$bad_slen" 'cold-load rejected (invalid header)'
echo "PASS t3: oversized-SLEN cold image rejected with diagnostic, fatal exit 1"

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

expect_fatal t4 "$bad_reloc" 'cold-load rejected (\(invalid relocation table\|not a complete version 5 packed image\)'
echo "PASS t4: out-of-range relocation-table entry rejected with diagnostic, fatal exit 1"

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
  expect_fatal "$1" "$2" 'cold-load rejected (\(build digest mismatch\|not a complete version 5 packed image\)'
}
check_t5 t5a "$no_trailer"
if [[ -f "$binary.cold" ]]; then
  # The v5 trailer (magic, version 5, 32-byte digest) sits before the
  # trailing relocation bytes, so locate it instead of using file offsets.
  bad_digest="$work/bad-digest.bin"
  cp "$binary.cold" "$bad_digest"
  trailer_off="$(python3 - "$bad_digest" <<'PY'
import struct, sys
with open(sys.argv[1], "r+b") as f:
    d = f.read()
    i = d.rfind(struct.pack("<QQ", 1195655758, 5))
    if i < 0:
        print(-1); sys.exit(0)
    f.seek(i + 20); f.write(bytes([d[i + 20] ^ 0xff]))
    print(i)
PY
)"
  [[ "$trailer_off" -ge 0 ]] || fail "t5: build-digest trailer not found in $binary.cold"
  check_t5 t5b "$bad_digest"
  cp "$binary.cold" "$work/no-trailer-real.bin"
  truncate -s "$trailer_off" "$work/no-trailer-real.bin"
  check_t5 t5c "$work/no-trailer-real.bin"
  echo "PASS t5: missing/incorrect build-digest trailer rejected (synthetic, flipped digest, truncated)"
else
  echo "PASS t5a: header without trailer rejected (real image absent, 5b/5c skipped)"
fi

echo "PASS standalone-cold-image-security-smoke"
