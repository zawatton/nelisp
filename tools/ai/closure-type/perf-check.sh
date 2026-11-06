#!/usr/bin/env bash
# Usage: CT_BIN=NEW CT_BASE_BIN=BASE perf-check.sh
# Runs bench.el 5 times per binary in alternating order (fresh processes).
# Exit 0 iff every workload's checksum agrees and NEW's median <= 1.05 x BASE's
# median (+2 ms floor for noise).  Prints one line per workload.
here=$(cd "$(dirname "$0")" && pwd) || exit 2
[ -x "${CT_BIN:-}" ] && [ -x "${CT_BASE_BIN:-}" ] || { echo "CT_BIN/CT_BASE_BIN unset" >&2; exit 2; }
out=$(mktemp -d) || exit 2
for i in 1 2 3 4 5; do
  for side in base new; do
    bin=$CT_BASE_BIN; [ $side = new ] && bin=$CT_BIN
    (ulimit -v 8388608; timeout 300 "$bin" --load "$here/bench.el" 2>/dev/null) | grep -a '^BENCH ' >> "$out/$side" || exit 2
  done
done
rc=0
for w in $(awk '{print $2}' "$out/base" | sort -u); do
  med() { grep -a "^BENCH $w " "$out/$1" | awk '{print $3}' | sort -n | sed -n 3p; }
  sums() { grep -a "^BENCH $w " "$out/$1" | awk '{print $4}' | sort -u; }
  b=$(med base); n=$(med new)
  ok=$(awk -v b="$b" -v n="$n" 'BEGIN{print (n <= b*1.05 + 2) ? "OK" : "SLOW"}')
  [ "$(sums base)" = "$(sums new)" ] && [ "$(sums new | wc -l)" = 1 ] || ok=CHECKSUM
  echo "$w base=${b}ms new=${n}ms $ok"
  [ "$ok" = OK ] || rc=1
done
rm -rf "$out"; exit $rc
