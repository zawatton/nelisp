#!/bin/bash
# Doc 241 S1 probe. Usage: probe-equal.sh BINARY  (or BINARY in $PROBE_BIN).
# The dump format switch is read from the process's initial environment, so
# the dense and sparse images come from two identical fresh processes run
# with ASLR disabled (setarch -R) and the same relative image name.
BIN=${1:-${PROBE_BIN:-}}
[ -n "$BIN" ] && [ -x "$BIN" ] || { echo "usage: $0 BINARY (or PROBE_BIN)"; exit 2; }
BIN=$(readlink -f "$BIN"); HERE=$(cd "$(dirname "$0")" && pwd)
T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
fail=0; bad() { echo "FAIL: $*"; fail=1; }
mkdir "$T/d" "$T/s"
(cd "$T/d" && ulimit -v 8388608 && env -i HOME="$HOME" PATH="$PATH" NELISP_COLD_IMAGE_FORMAT=dense timeout 600 setarch "$(uname -m)" -R "$BIN" --load "$HERE/dump.el") >"$T/d.log" 2>&1
(cd "$T/s" && ulimit -v 8388608 && env -i HOME="$HOME" PATH="$PATH" NELISP_COLD_IMAGE_FORMAT=xxxxx timeout 600 setarch "$(uname -m)" -R "$BIN" --load "$HERE/dump.el") >"$T/s.log" 2>&1
[ -s "$T/d/img.cold" ] && [ -s "$T/s/img.cold" ] || { echo "FAIL: dump produced no image"; tail -3 "$T/d.log" "$T/s.log"; exit 1; }
python3 -I "$HERE/decode.py" "$T/s/img.cold" "$T/dec.cold" || bad "decode"
# Both processes get an environment of identical shape; the only value that differs is
# NELISP_COLD_IMAGE_FORMAT ("dense" vs "xxxxx", same length).  The heap holds that
# value (and a derived word near it), so differing bytes are allowed only within 64
# bytes after an occurrence of the value; anything else, or a size difference, fails.
python3 -I - "$T/dec.cold" "$T/d/img.cold" <<'PY' || bad "decoded sparse differs from dense"
import sys
a = open(sys.argv[1], "rb").read(); b = open(sys.argv[2], "rb").read()
if len(a) != len(b):
    sys.exit(f"size differs: decoded {len(a)} dense {len(b)}")
diff = [i for i in range(len(a)) if a[i] != b[i]]
occ = [i for i in range(len(b)) if b.startswith(b"dense", i)] if diff else []
bad = [i for i in diff if not any(o <= i < o + 64 for o in occ)]
print(f"differing bytes {len(diff)}, outside env-value windows {len(bad)}")
if bad or len(diff) > 64:
    sys.exit(f"unexplained differences at {bad[:10]}")
PY
echo "equal: decoded sparse == dense (except the env value)"
echo "sizes: dense $(stat -c %s "$T/d/img.cold") sparse $(stat -c %s "$T/s/img.cold")"
(ulimit -v 8388608; timeout 600 "$BIN" --cold-load-from "$T/s/img.cold" --load "$HERE/small.el") >"$T/ls.out" 2>"$T/ls.err"
grep -q INTERN-PROBE-PASS "$T/ls.out" || bad "intern probe on sparse image: $(tail -2 "$T/ls.out")"
grep -q 'cold-load rejected' "$T/ls.err" && bad "sparse image was rejected: $(head -1 "$T/ls.err")"
(ulimit -v 8388608; timeout 600 "$BIN" --cold-load-from "$T/d/img.cold" --load "$HERE/small.el") >"$T/ld.out" 2>"$T/ld.err"
grep -q 'cold-load rejected' "$T/ld.err" || bad "dense v2 image was not refused loudly"
[ $fail = 0 ] && { echo PASS; exit 0; } || { echo FAIL; exit 1; }
