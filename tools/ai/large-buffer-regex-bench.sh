#!/bin/sh
# Usage: large-buffer-regex-bench.sh A1|A2|A3|A4|A8
# Env:   NELISP_ORG_EL  large org.el (required for A1-A4)
#        NELISP_BIN     binary under test (default ./target/nelisp)
#        NELISP_BASE_BIN baseline binary for A8 (default ./target/nelisp.base)
# Exit 0 iff the criterion holds.  Thresholds live here, not in the ledger.
id=$1
bin=${NELISP_BIN:-./target/nelisp}
base=${NELISP_BASE_BIN:-./target/nelisp.base}
here=$(cd "$(dirname "$0")" && pwd)
el="$here/large-buffer-regex-bench.el"
run() { # binary id
  ( ulimit -v 8388608; BENCH=$2 timeout 300 "$1" --load "$el" -- 2>&1 | grep '^BENCH' | head -1 )
}
field() { printf '%s\n' "$1" | sed -n "s/.*$2=\([^ ]*\).*/\1/p"; }
le() { awk -v a="$1" -v b="$2" 'BEGIN{exit !(a+0<=b+0)}'; }
case $id in
  A1) out=$(run "$bin" A1); echo "$out"
      [ "$(field "$out" value)" = nil ] && le "$(field "$out" elapsed)" 1.0 ;;
  A2) out=$(run "$bin" A2); echo "$out"
      [ "$(field "$out" value)" = nil ] && le "$(field "$out" elapsed)" 0.3 ;;
  A3|A4)
      if [ "$id" = A3 ]; then lim=3; else lim=15; fi
      out=$(run "$bin" "$id"); echo "$out"
      host=$(BENCH=$id emacs --batch -Q -l "$el" 2>&1 | grep '^BENCH' | head -1); echo "host: $host"
      [ -n "$(field "$out" value)" ] && [ "$(field "$out" value)" = "$(field "$host" value)" ] \
        && le "$(field "$out" elapsed)" "$lim" ;;
  A8) rn=""; rb=""
      for k in 1 2 3; do
        o=$(run "$bin" A8); b=$(run "$base" A8)
        rn="$rn $(field "$o" elapsed)"; rb="$rb $(field "$b" elapsed)"
      done
      echo "new:$rn base:$rb"
      med() { printf '%s\n' $1 | sort -n | sed -n 2p; }
      mn=$(med "$rn"); mb=$(med "$rb"); echo "median new=$mn base=$mb"
      awk -v a="$mn" -v b="$mb" 'BEGIN{r=a/b; printf "ratio=%.3f\n", r; exit !(r<=1.05)}' ;;
  *) echo "usage: $0 A1|A2|A3|A4|A8" >&2; exit 2 ;;
esac
