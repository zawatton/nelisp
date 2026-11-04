#!/bin/bash
# S2.1 structural check for the application image launcher (negative controls 1-5).
set -u
. "$(dirname "$0")/common.sh"
DRV=$RUNDIR/benchmark-driver-native-on.el
mkdir -p "$T/good" "$T/lane-scratch"
"$PRODUCE" "$BIN" "$DRV" hash-get1024 "$T/good/hash-get1024.img" >/dev/null || bad "produce failed"
G=$T/good/hash-get1024.img
# neg NAME EXPECT_REGEX : launcher must exit non-zero, print no BENCH-CHECK, and match EXPECT
neg() {
  [ "$RC" -ne 0 ] || bad "$1: launcher exited 0"
  echo "$OUT" | grep -q 'BENCH-CHECK' && bad "$1: BENCH-CHECK printed"
  echo "$OUT" | grep -q -- "$2" || bad "$1: unexpected message (want /$2/): $(echo "$OUT" | tail -2)"
  echo "ok $1 -> rc=$RC: $(echo "$OUT" | grep -m1 -- "$2" | cut -c1-150)"
}
mkvariant() {  # mkvariant NAME : copies manifest+main (image copied only if $2 given)
  mkdir -p "$T/$1"; cp "$G.manifest" "$T/$1/hash-get1024.img.manifest"; cp "$G.main.el" "$T/$1/hash-get1024.img.main.el"
}
setkey() {  # setkey FILE KEY VALUE
  python3 -I - "$@" <<'P' || bad "setkey $2"
import sys
f,k,v=sys.argv[1:]
L=open(f).read().split("\n"); n=0
for i,l in enumerate(L):
    if l.split(" ")[0]==k: L[i]=k+" "+v; n+=1
assert n==1
open(f,"w").write("\n".join(L))
P
}
# (a) positive
launch "$T/good" "$BIN" "$BSHA" 16
[ "$RC" -eq 0 ] && echo "$OUT" | grep -q '^BENCH-CHECK n=16 result=16:' || bad "(a) positive run failed: $(echo "$OUT" | tail -2)"
echo "ok (a) launcher prints BENCH-CHECK at N=16"
# (1) one byte of the image flipped, manifest unchanged
mkvariant c1; cp "$G" "$T/c1/hash-get1024.img"
python3 -I - "$T/c1/hash-get1024.img" <<'P' || bad "(1) flip"
import sys
f=open(sys.argv[1],"r+b"); f.seek(20_000_000); b=f.read(1); f.seek(20_000_000); f.write(bytes([b[0]^1]))
P
launch "$T/c1" "$BIN" "$BSHA" 16; neg "(1) flipped image byte" 'image sha256 .* differs from manifest'
# (2) manifest image-sha256 edited
mkvariant c2; cp "$G" "$T/c2/hash-get1024.img"
setkey "$T/c2/hash-get1024.img.manifest" image-sha256 "$(printf '0%.0s' $(seq 64))"
launch "$T/c2" "$BIN" "$BSHA" 16; neg "(2) manifest image-sha256 edited" 'image sha256 .* differs from manifest'
# (3a) different binary with its true sha, build28 manifest
[ -x "$BIN27" ] || bad "(3) other binary missing: $BIN27"
S27=$(sha256sum "$BIN27" | cut -d' ' -f1); [ "$S27" != "$BSHA" ] || bad "(3) other binary identical"
launch "$T/good" "$BIN27" "$S27" 16; neg "(3a) other binary, build28 manifest" 'binary-sha256 differs'
# (3b) manifest binary-sha256 rewritten to the other binary's
mkvariant c3b; cp "$G" "$T/c3b/hash-get1024.img"
setkey "$T/c3b/hash-get1024.img.manifest" binary-sha256 "$S27"
launch "$T/c3b" "$BIN27" "$S27" 16
[ "$RC" -ne 0 ] && ! echo "$OUT" | grep -q BENCH-CHECK || bad "(3b) other binary ran the image: rc=$RC"
echo "ok (3b) other binary + rewritten manifest -> rc=$RC (stage: $(echo "$OUT" | grep -v '^$' | head -1 | cut -c1-150))"
# (4a) image without any application record: the base binary's .cold (verify itself is void there)
mkvariant c4a; cp "$BIN.cold" "$T/c4a/hash-get1024.img"
setkey "$T/c4a/hash-get1024.img.manifest" image-sha256 "$(sha256sum "$BIN.cold" | cut -d' ' -f1)"
launch "$T/c4a" "$BIN" "$BSHA" 16; neg "(4a) base .cold image, no record" 'void-function\|record missing\|Application image'
# (4b) image that has the verify function but no record (library only, then dump)
mkvariant c4b
printf '%s\n' ';;; -*- lexical-binding: t; -*-' "(load \"$ROOT/tools/nelisp-application-image.el\" nil t)" \
  "(nelisp--arena-dump-image-stream \"$T/c4b/hash-get1024.img\")" '(kill-emacs 0)' >"$T/c4b.el"
(ulimit -v 8388608; env -i HOME="$HOME" PATH="$PATH" timeout 600 "$BIN" --load "$T/c4b.el") >"$T/c4b.log" 2>&1 || bad "(4b) library-only dump failed: $(tail -2 "$T/c4b.log")"
setkey "$T/c4b/hash-get1024.img.manifest" image-sha256 "$(sha256sum "$T/c4b/hash-get1024.img" | cut -d' ' -f1)"
launch "$T/c4b" "$BIN" "$BSHA" 16; neg "(4b) verify present, record missing" 'record missing'
# (5) main.el redefines a recorded name before verify
mkvariant c5; cp "$G" "$T/c5/hash-get1024.img"
M=$T/c5/hash-get1024.img.main.el
{ echo ';;; -*- lexical-binding: t; -*-'; echo '(defun bench-hash-get1024 (n input) nil)'; sed 1d "$G.main.el"; } >"$M"
launch "$T/c5" "$BIN" "$BSHA" 16; neg "(5a) redefined name, main-sha256 stale" 'main file sha256 .* differs from manifest'
setkey "$T/c5/hash-get1024.img.manifest" main-sha256 "$(sha256sum "$M" | cut -d' ' -f1)"
launch "$T/c5" "$BIN" "$BSHA" 16; neg "(5b) redefined name, main-sha256 updated" 'was redefined'
# manifest strictness
mkvariant c6; cp "$G" "$T/c6/hash-get1024.img"; echo "extra 1" >>"$T/c6/hash-get1024.img.manifest"
launch "$T/c6" "$BIN" "$BSHA" 16; neg "(6) unknown manifest key" 'unknown key'
echo "S2.1 PASS ($LAUNCH)"
