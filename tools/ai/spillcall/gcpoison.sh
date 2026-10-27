#!/bin/sh
# spillcall GC stress under the map-only collector with dead-word poisoning: before each corpus, BIN
# pins the collector mode to 3 (map-only, every word the stack map calls dead is overwritten with a
# poison word, `(nelisp--debug-switch 97 3)'); the corpora force collections themselves (every-safepoint
# collection, switch 70, exceeds 280 s per corpus even on v34).  A register-held reference the collector missed, or a frame word the
# map wrongly called dead, then reads poison or a freed object.  Outputs must equal GNU 31.1's
# (the weak probe: its expected file).
# Usage: sh tools/ai/spillcall/gcpoison.sh BIN
cd "$(dirname "$0")" || exit 1
BIN=$1
case "$BIN" in /*) ;; *) BIN=$(cd ../../.. && realpath "$BIN") ;; esac
[ -x "$BIN" ] || { echo "gcpoison: no binary $BIN"; exit 2; }
WEAK=$(realpath ../../../../resume-4943-20261024/target/resume-acceptance/d7-critical-weak24)
O=$(mktemp -d) || exit 1
fail=0
run() { # NAME FILE EXPECTED-FILE
  printf '(princ (format "gcpoison-mode %%S\\n" (list (nelisp--debug-switch 97 3))))\n(load "%s" nil t)\n' "$2" > "$O/$1.el"
  ( ulimit -v 8388608; timeout 280 "$BIN" --load "$O/$1.el" -- < /dev/null > "$O/$1.out" 2> "$O/$1.err" ); rc=$?
  head -1 "$O/$1.out" > "$O/$1.mode"; tail -n +2 "$O/$1.out" > "$O/$1.body"
  if [ $rc = 0 ] && grep -q "gcpoison-mode (1)" "$O/$1.mode" && cmp -s "$O/$1.body" "$3"; then
    echo "gcpoison: $1 equal ($(wc -c < "$3") bytes, $(cat "$O/$1.mode"))"
  else
    echo "gcpoison: $1 DIFF rc=$rc $(cat "$O/$1.mode")"; diff "$O/$1.body" "$3" | head -5; fail=1
  fi
}
for c in vmfast2 regalloc; do
  ( cd ../$c && timeout 280 /usr/local/bin/emacs -Q --batch -l gcstress.el > "$O/$c.gnu" 2>/dev/null )
  run "$c-gcstress" "$(realpath ../$c/gcstress.el)" "$O/$c.gnu"
done
run weak24 "$WEAK/probe.el" "$WEAK/expected"
rm -rf "$O"
exit $fail
