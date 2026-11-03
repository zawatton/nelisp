#!/bin/bash
# S1.1 structural check for the application image producer (with 3 negative controls).
set -u
. "$(dirname "$0")/common.sh"
DRV=$RUNDIR/benchmark-driver-native-on.el
bad() { echo "FAIL: $*"; exit 1; }
[ -r "$ROOT/tools/nelisp-application-image.el" ] || bad "tools/nelisp-application-image.el absent"
# (a) positive
"$PRODUCE" "$BIN" "$DRV" hash-get1024 "$T/a.img" >/dev/null || bad "(a) produce failed"
want=$(sed -n 's/^image-sha256 //p' "$T/a.img.manifest")
[ "$want" = "$(sha256sum "$T/a.img" | cut -d' ' -f1)" ] || bad "(a) manifest image sha mismatch"
out=$(run_nelisp BENCH_ROW=hash-get1024 BENCH_N=16 BENCH_HIGH=16 BENCH_WARMUPS=0 -- --cold-load-from "$T/a.img" --load "$T/a.img.main.el" 2>&1) || bad "(a) cold-load main failed: $(echo "$out"|tail -2)"
echo "$out" | grep -q '^BENCH-CHECK n=16 result=16:' || bad "(a) no BENCH-CHECK"
echo "ok (a) produce + manifest sha + cold-load BENCH-CHECK"
# (b) corrupted pin: producer must fail and leave nothing
cp "$DRV" "$T/bad.el"
python3 -I - "$T/bad.el" <<'P' || bad "(b) could not corrupt pin"
import sys,re
s=open(sys.argv[1]).read()
m=re.search(r'\(:package "[^"]*" "([0-9a-f])',s); assert m
i=m.start(1); s=s[:i]+("0" if m.group(1)!="0" else "1")+s[i+1:]
open(sys.argv[1],"w").write(s)
P
if "$PRODUCE" "$BIN" "$T/bad.el" hash-get1024 "$T/b.img" >"$T/b.log" 2>&1; then bad "(b) producer succeeded on corrupted pin"; fi
grep -q 'drift' "$T/b.log" || bad "(b) failure was not a pin drift: $(tail -3 "$T/b.log")"
ls "$T"/b.img* >/dev/null 2>&1 && bad "(b) leftover image/manifest"
echo "ok (b) corrupted pin -> producer fails (pin drift), no image/manifest"
# (c) redefined recorded name
{ echo ';;; -*- lexical-binding: t; -*-'; echo '(defun bench-hash-get1024 (n input) nil)'; echo '(nelisp-application-image-verify)'; } >"$T/c.el"
if out=$(run_nelisp BENCH_ROW=hash-get1024 BENCH_N=16 BENCH_HIGH=16 -- --cold-load-from "$T/a.img" --load "$T/c.el" 2>&1); then bad "(c) verify passed after redefinition"; fi
echo "$out" | grep -q 'was redefined' || bad "(c) wrong failure: $(echo "$out"|tail -2)"
echo "ok (c) redefined name -> verify fails"
# (d) wrong row on the 8-row image
if out=$(run_nelisp BENCH_ROW=nested-function-call2 BENCH_N=16 BENCH_HIGH=16 -- --cold-load-from "$T/a.img" --load "$T/a.img.main.el" 2>&1); then bad "(d) verify passed for foreign row"; fi
echo "$out" | grep -q 'not covered by this image' || bad "(d) wrong failure: $(echo "$out"|tail -2)"
echo "ok (d) foreign row -> verify fails"
echo "S1.1 PASS"
