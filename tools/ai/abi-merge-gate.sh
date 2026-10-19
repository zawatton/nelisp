#!/bin/bash
# Doc 215 builtin word ABI merge gate (tools/ai/abi-progress.org, stage S6).
#
#   abi-merge-gate.sh perf CANDIDATE BASELINE
#       Runs CANDIDATE and BASELINE alternately on this machine: `--eval nil'
#       boot wall time (ABI_GATE_BOOT_ROUNDS, default 15) and the regex line of
#       test/perf/shallow-binding-bench.el (3 rounds).  Exits 0 only if each
#       candidate median is <= the BASELINE median measured in the same run
#       times ABI_GATE_RATIO (default 1.00), so machine load moves both sides
#       alike.  The absolute figures ABI_GATE_BOOT_MAX (3.34 s) and
#       ABI_GATE_REGEX_MAX (705 us), measured once on 441166d9d, are printed
#       for information only.  Prints every sample, both medians and the load
#       average.
#       Why 15 boot rounds: a boot is ~3.15 s with a run-to-run spread of
#       ~+-1%, and a candidate at parity (f3, 2026-10-19: Ir -0.37%) failed a
#       5-round x1.00 comparison by 1 ms (3.1539 vs 3.1528 s) while 15 rounds
#       gave 3.138 vs 3.146.  More rounds keep the ratio honest at 1.00
#       instead of widening it.
#   abi-merge-gate.sh ir CANDIDATE BASELINE
#       Instruction counts (valgrind callgrind) instead of wall time, which is
#       noisy on this shared machine: `--eval nil' boot and a regex load (the
#       regex line of test/perf/shallow-binding-bench.el, 20 x 300 matches, >=10 GC cycles per binary),
#       both binaries under ABI_LAYOUT_PAD=0/700/1400/2100 bytes in parallel.
#       Exits 0 only if all outputs agree and each workload's mean candidate
#       count is <= the mean baseline count. Prints per-layout Ir/GC and spread.
#   abi-merge-gate.sh files CANDIDATE BASELINE
#       Five vendor sources plus builtin micro-suite under the same four
#       layouts, fresh caches, identical output and mean total Ir <= baseline.
#   abi-merge-gate.sh census CENSUS_TSV MAX
#       Exits 0 only if box-census.py's TOTAL for CENSUS_TSV is below MAX.
#   abi-merge-gate.sh store-word CENSUS_TSV MAX
#       Exits 0 only if the nl_val_store_word count (all families) is <= MAX;
#       prints per-family nl_val_store_word + nl_abi_rc_to_word dependencies.
export LC_ALL=C
cd "$(dirname "$0")/../.." || exit 2

median() { sort -g | awk '{a[NR]=$1} END{if (NR==0) {print "nan"; exit} print (NR%2) ? a[(NR+1)/2] : (a[NR/2]+a[NR/2+1])/2}'; }

case "$1" in
  perf)
    cand=$2; base=$3
    [ -x "$cand" ] && [ -x "$base" ] || { echo "usage: $0 perf CANDIDATE BASELINE"; exit 2; }
    boot_max=${ABI_GATE_BOOT_MAX:-3.34}; regex_max=${ABI_GATE_REGEX_MAX:-705}
    tmp=$(mktemp -d) || exit 2
    echo "load start: $(cut -d' ' -f1-3 /proc/loadavg)"
    for i in $(seq ${ABI_GATE_BOOT_ROUNDS:-15}); do
      for who in base cand; do
        b=$base; [ $who = cand ] && b=$cand
        s=$(date +%s.%N); timeout 120 "$b" --eval nil >/dev/null 2>&1; e=$(date +%s.%N)
        t=$(echo "$e - $s" | bc); echo "$t" >> $tmp/boot-$who; echo "boot $i $who $t"
      done
    done
    for i in 1 2 3; do
      for who in base cand; do
        b=$base; [ $who = cand ] && b=$cand
        r=$(timeout 900 nice -n 10 "$b" --load test/perf/shallow-binding-bench.el 2>&1 \
              | awk '/SB-BENCH regex/ {print $3; exit}')
        [ -n "$r" ] || r=999999
        echo "$r" >> $tmp/regex-$who; echo "regex $i $who $r"
      done
    done
    echo "load end: $(cut -d' ' -f1-3 /proc/loadavg)"
    bb=$(median < $tmp/boot-base); bc_=$(median < $tmp/boot-cand)
    rb=$(median < $tmp/regex-base); rc_=$(median < $tmp/regex-cand)
    rm -rf "$tmp"
    ratio=${ABI_GATE_RATIO:-1.00}
    echo "median boot base $bb cand $bc_; regex base $rb cand $rc_ (gate: cand <= base x $ratio)"
    echo "info: absolute boot $bc_ vs $boot_max s, regex $rc_ vs $regex_max us (not gating)"
    awk -v b="$bc_" -v bb="$bb" -v r="$rc_" -v rb="$rb" -v k="$ratio" \
      'BEGIN { ok = (b + 0 <= (bb + 0) * k) && (r + 0 <= (rb + 0) * k);
               printf "boot %s, regex %s\n", (b + 0 <= (bb + 0) * k) ? "PASS" : "FAIL",
                      (r + 0 <= (rb + 0) * k) ? "PASS" : "FAIL"; exit !ok }'
    ;;
  ir)
    python3 tools/ai/abi-ir.py ir "$2" "$3"
    ;;
  files)
    python3 tools/ai/abi-ir.py files "$2" "$3"
    ;;
  census)
    total=$(python3 tools/ai/box-census.py summary "$2" | awk '$1 == "TOTAL" {print $2}')
    echo "census TOTAL ${total:-none} (must be < $3)"
    [ -n "$total" ] && [ "$total" -lt "$3" ]
    ;;
  store-word)
    python3 - "$2" "$3" <<'PYGATE'
import importlib.util, sys
spec = importlib.util.spec_from_file_location("census", "tools/ai/box-census.py")
c = importlib.util.module_from_spec(spec); spec.loader.exec_module(c)
s, _ = c.tally(c.load(sys.argv[1]))
keys = (*c.CONVERSION_CALLS, "inline-box-to-word")
if not all(any(k == "inline-box-to-word" for _, k, _ in rows) for rows in c.load(sys.argv[1]).values()):
    sys.exit("census predates inline-conversion checking; regenerate from the build dump")
if not all(any(k == c.CONVERSION_WALK_VERSION for _, k, _ in rows) for rows in c.load(sys.argv[1]).values()):
    sys.exit("census predates complete conversion walking; regenerate from the build dump")
for family, counts in s["families"].items():
    print("family", family, " ".join(f"{k}={counts.get(k, 0)}" for k in keys))
n = sum(s["idioms"].get(k, 0) for k in keys)
print(f"box-to-word dependencies {n} (must be <= {sys.argv[2]})")
sys.exit(0 if s["units"] and n <= int(sys.argv[2]) else 1)
PYGATE
    ;;
  *) echo "usage: $0 perf|ir|files CANDIDATE BASELINE | census TSV MAX | store-word TSV MAX"; exit 2 ;;
esac
