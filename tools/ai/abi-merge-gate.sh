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
#   abi-merge-gate.sh census CENSUS_TSV MAX
#       Exits 0 only if box-census.py's TOTAL for CENSUS_TSV is below MAX.
#   abi-merge-gate.sh store-word CENSUS_TSV MAX
#       Exits 0 only if the nl_val_store_word count (all families) is <= MAX;
#       prints the per-family counts.
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
  census)
    total=$(python3 tools/ai/box-census.py summary "$2" | awk '$1 == "TOTAL" {print $2}')
    echo "census TOTAL ${total:-none} (must be < $3)"
    [ -n "$total" ] && [ "$total" -lt "$3" ]
    ;;
  store-word)
    out=$(python3 tools/ai/box-census.py summary "$2")
    n=$(echo "$out" | awk '$1 == "nl_val_store_word" {print $2}')
    echo "$out" | grep -o '^family [a-z-]* \|nl_val_store_word=[0-9]*' | paste -sd' ' | sed 's/ family /\nfamily /g'
    echo "nl_val_store_word ${n:-none} (must be <= $3)"
    [ -n "$n" ] && [ "$n" -le "$3" ]
    ;;
  *) echo "usage: $0 perf CANDIDATE BASELINE | census TSV MAX | store-word TSV MAX"; exit 2 ;;
esac
