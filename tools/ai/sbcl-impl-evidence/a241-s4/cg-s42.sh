#!/bin/bash
# Doc 241 S4.2 investigation: callgrind Ir per step of vector-allocate64, full lane
# (preparation in-process) vs app image (cold-load of the saved application), same binary.
# Usage: cg-s42.sh BINARY DRIVER_DIR APP_IMAGE OUT [LO HI]
set -u
I=$1; D=$2; IMG=$3; O=$4; LO=${5:-1048576}; HI=${6:-2097152}
mkdir -p "$O" || exit 1
SHA=$(sha256sum "$I" | cut -d' ' -f1)
run() { # path n
  local p=$1 n=$2 S=$O/s-$1-$2; mkdir -p "$S/cache"
  local args=(--load "$D/benchmark-driver-native-on.el")
  [ "$p" = app ] && args=(--cold-load-from "$IMG" --load "$IMG.main.el")
  ( ulimit -v 8388608; ulimit -c 0; env -i HOME="$HOME" PATH="$PATH" BENCH_ENGINE=nelisp NELISP_PRODUCER_IMAGE_SHA256="$SHA" \
      BENCH_ROW=vector-allocate64 BENCH_N=$n BENCH_HIGH=$HI BENCH_WARMUPS=2 TMPDIR="$S" XDG_CACHE_HOME="$S/cache" \
      valgrind --tool=callgrind --callgrind-out-file="$O/cg.$p.$n" "$I" "${args[@]}" < /dev/null > "$O/o.$p.$n" 2> "$O/e.$p.$n" ) || { echo "run $p $n failed"; exit 1; }
  grep -q "result=$n:" "$O/o.$p.$n" || { echo "no result for $p $n"; exit 1; }
}
for p in full app; do for n in $LO $HI; do run $p $n; done; done
python3 -I - "$O" "$LO" "$HI" <<'P'
import subprocess, sys, re, collections
O, lo, hi = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
def fn(path):
    out = subprocess.run(["callgrind_annotate", "--threshold=100", path], capture_output=True, text=True).stdout
    tot = int(re.search(r"([\d,]+) \(100\.0%\)\s+PROGRAM TOTALS", out).group(1).replace(",", ""))
    d = collections.Counter()
    for l in out.splitlines():
        m = re.match(r"\s*([\d,]+) \([ \d.]+%\)\s+\S*:?(\S+)", l)
        if m and "PROGRAM TOTALS" not in l:
            d[l.split()[-1]] += int(m.group(1).replace(",", ""))
    return tot, d
per = {}
for p in ("full", "app"):
    t1, d1 = fn(f"{O}/cg.{p}.{lo}"); t2, d2 = fn(f"{O}/cg.{p}.{hi}")
    per[p] = ((t2 - t1) / (hi - lo), {k: (d2[k] - d1[k]) / (hi - lo) for k in set(d1) | set(d2)})
    print(f"{p:4} Ir/step {per[p][0]:9.1f}")
print(f"app/full {per['app'][0] / per['full'][0]:.3f}")
diff = sorted(((per['full'][1].get(k, 0) - per['app'][1].get(k, 0), k) for k in set(per['full'][1]) | set(per['app'][1])), key=lambda x: -abs(x[0]))
print("largest per-step differences (full - app), Ir/step:")
for v, k in diff[:15]:
    print(f"  {v:9.1f}  full {per['full'][1].get(k,0):9.1f}  app {per['app'][1].get(k,0):9.1f}  {k}")
P
