#!/bin/bash
# Doc 241 S2.2. IMAGE_SIZE_BIN=<binary> (default build30). Content zero runs >= 64 KiB total <= 262144 B, then the S1.2 gates.
BIN=${IMAGE_SIZE_BIN:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds/build-on30/nelisp-dynamic}
HERE=$(cd "$(dirname "$0")" && pwd)
IMG=${IMAGE_SIZE_IMG:-}
if [ -z "$IMG" ]; then for c in "$BIN.cold" "$BIN.img" "$BIN.image"; do [ -f "$c" ] && IMG=$c && break; done; fi
[ -f "$IMG" ] || { echo "no cold image beside $BIN (set IMAGE_SIZE_IMG)"; exit 1; }
DEC=$(mktemp); trap 'rm -f "$DEC"' EXIT
python3 -I "$HERE/decode.py" "$IMG" "$DEC" || exit 1
T=$(python3 -I "$HERE/../a241-s2/walk-content.py" "$DEC" --total) || exit 1
echo "content zero runs >= 64 KiB total: $T"
[ "$T" -le 262144 ] || { echo "zero runs total $T > 262144"; exit 1; }
# the scratch must still be recreated lazily and matching must work
P=$(mktemp); echo '(princ (format "scratch=%S match=%S after=%S\n" nlre--nat-scratch (string-match "a+b" "xaab") (and (stringp nlre--nat-scratch) (length nlre--nat-scratch))))' > "$P"
OUT=$(ulimit -v 8388608; timeout 60 "$BIN" --load "$P" -- 2>&1 | head -1); rm -f "$P"
echo "$OUT"; [ "$OUT" = "scratch=nil match=1 after=3200000" ] || { echo "scratch not dropped/recreated"; exit 1; }
IMAGE_SIZE_BIN=$BIN bash "$HERE/check-s12v4.sh"
