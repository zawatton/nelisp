#!/bin/bash
# Doc 241 S4.2: instruction-level callgrind of vector-allocate64 (full lane vs app image) at one N,
# attributed to functions with the binary's nm table (callgrind does not resolve its symbols).
# Usage: cg-instr-s42.sh BINARY DRIVER_DIR APP_IMAGE OUT [N]
set -u
I=$1; D=$2; IMG=$3; O=$4; NN=${5:-1048576}
mkdir -p "$O" || exit 1
SHA=$(sha256sum "$I" | cut -d' ' -f1)
for p in full app; do
  args=(--load "$D/benchmark-driver-native-on.el"); [ "$p" = app ] && args=(--cold-load-from "$IMG" --load "$IMG.main.el")
  ( ulimit -v 8388608; ulimit -c 0; cd "$O" && env -i HOME="$HOME" PATH="$PATH" BENCH_ENGINE=nelisp NELISP_PRODUCER_IMAGE_SHA256="$SHA" \
      BENCH_ROW=vector-allocate64 BENCH_N=$NN BENCH_HIGH=$NN BENCH_WARMUPS=2 TMPDIR="$O" XDG_CACHE_HOME="$O" \
      valgrind --tool=callgrind --dump-instr=yes --compress-pos=no --compress-strings=no --callgrind-out-file="$O/cgi.$p" "$I" "${args[@]}" < /dev/null > "$O/oi.$p" 2> "$O/ei.$p" ) || { echo "run $p failed"; exit 1; }
  grep -q "result=$NN:" "$O/oi.$p" || { echo "no result $p"; exit 1; }
done
nm -n "$I" | awk '$2 ~ /^[TtWw]$/ {print $1, $3}' > "$O/syms.txt"
python3 -I - "$O" "$I" <<'P'
import sys, bisect, collections, re
O, I = sys.argv[1], sys.argv[2]
syms = [(int(a, 16), n) for a, n in (l.split() for l in open(f"{O}/syms.txt"))]
addrs = [a for a, _ in syms]
def load(p):
    c = collections.Counter(); inbin = False; incall = False
    for l in open(f"{O}/cgi.{p}"):
        if l.startswith("ob="): inbin = l.strip().endswith(I) or I in l
        elif l.startswith("calls="): incall = True; continue
        elif l.startswith("0x"):
            if incall: incall = False; continue   # inclusive cost line of a call
            if not inbin: continue
            parts = l.split(); a = int(parts[0], 16); v = parts[-1]
            i = bisect.bisect_right(addrs, a) - 1
            c[syms[i][1] if i >= 0 else "?"] += int(v)
    return c
f, a = load("full"), load("app")
tf, ta = sum(f.values()), sum(a.values())
print(f"binary Ir total: full {tf} app {ta} diff {tf-ta}")
rows = sorted(set(f) | set(a), key=lambda k: -(f[k] - a[k]))
print("top functions by (full - app) Ir:")
for k in rows[:15]: print(f"  {f[k]-a[k]:>13}  full {f[k]:>13}  app {a[k]:>13}  {k}")
print("most negative:")
for k in rows[-5:]: print(f"  {f[k]-a[k]:>13}  full {f[k]:>13}  app {a[k]:>13}  {k}")
P
