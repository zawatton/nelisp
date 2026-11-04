#!/bin/sh
# S2.11 (mode oor) / S2.12 (mode write) on build16: re-run the probes on the pinned image.
cd "$(dirname "$0")" || exit 1
I=$HOME/.cache/tmp/nelisp-sbcl-impl-builds/build-on16/nelisp-dynamic
test "$(sha256sum $I | cut -d' ' -f1)" = "$(cat image-b16.sha256)" || exit 1
W=$(mktemp -d) || exit 1
trap 'rm -rf "$W"' EXIT
run() { (cd "$W" && ulimit -v 8388608 && ulimit -c 0 && env "$@" timeout 300 "$I" --load "$OLDPWD/$PROBE" < /dev/null 2>&1); }
case "$1" in
write)
  PROBE=dump-write-probe.el; out=$(run OORDIR=$W)
  echo "$out" | grep -qx 'OK-SIGNALED file-missing' || exit 1
  echo "$out" | grep -qE '^OK-WRITTEN ([0-9]+) \1$' || exit 1 ;;
oor)
  PROBE=oor-probe.el
  one=$(run OORDIR=$W); two=$(run OORDIR=$W OOR2=1)
  echo "$one" | grep -qE '^clean dump: [0-9]{7,}$' || exit 1
  # One planted slot: exactly one reference, named as slot 2 of a 4-slot vector (header 1032).
  echo "$one" | grep -qE '"heap image has 1 out-of-image references; first at [0-9a-f]+ target [0-9a-f]+" \([0-9]+ [0-9]+ 1032 2\)\)$' || exit 1
  # Two planted slots: two references, slots 0 and 2.
  echo "$two" | grep -qE '"heap image has 2 out-of-image references;.*\([0-9]+ [0-9]+ 1032 0\) \([0-9]+ [0-9]+ 1032 2\)\)$' || exit 1 ;;
*) exit 2 ;;
esac
echo PASS
