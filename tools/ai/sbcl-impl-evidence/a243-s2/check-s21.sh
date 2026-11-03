#!/bin/bash
# Doc 243 S2.1: no nl_ht_reindex call on the fast path; hash tables and the Doc 240 launcher
# behave the same on the forced-relocation path.
# Env: COLD_INIT_BIN (candidate, default build-on50), COLD_INIT_REF (default build-on33),
#      COLD_INIT_BUILDS, NL243_W (worktree root override), COLD_INIT_DRIVERS / COLD_INIT_REF_DRIVERS
#      (dirs holding benchmark-driver-native-on.el; default run/bK of each build, else run/b33).
set -u; export LC_ALL=C
HERE=$(cd "$(dirname "$0")" && pwd)
NB=${COLD_INIT_BUILDS:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds}
BIN=${COLD_INIT_BIN:-$NB/build-on50/nelisp-dynamic}
REF=${COLD_INIT_REF:-$NB/build-on33/nelisp-dynamic}
W=${NL243_W:-$(cd "$HERE/../../../.." && pwd)}
NOP=$HERE/../a242-s2/nop.el; [ -r "$NOP" ] || NOP=$W/tools/ai/sbcl-impl-evidence/a242-s2/nop.el
for x in "$BIN" "$REF" "$NOP" "$HERE/ht-probe.el" "$HERE/cg-calls.py" "$W/tools/nelisp-application-image-produce.sh" "$W/tools/nelisp-application-image-launch.sh"; do [ -e "$x" ] || { echo "FAIL: missing $x"; echo S2.1-FAIL; exit 1; }; done
for t in valgrind nm python3; do command -v $t >/dev/null || { echo "FAIL: $t not installed"; echo S2.1-FAIL; exit 1; }; done
mkdir -p "$HOME/.cache/tmp"; T=$(mktemp -d "$HOME/.cache/tmp/a243s21.XXXXXX"); trap 'rm -rf "$T"' EXIT
fail=0; bad() { echo "FAIL: $*"; fail=1; }
SYNV=(); for i in $(seq 0 59); do SYNV+=("NL243_VAR_$i=$(printf '%064d' $((i*7919+13)) | tr 0-9 a-j)"); done
# (a) nl_ht_reindex call count in a bare start under SYN
reindex_calls() { # BIN TAG -> count (or "nosym")
  local b=$1 a addrs
  addrs=$(nm "$b" 2>/dev/null | awk '$3 ~ /^nl_ht_reindex/ {print $1}')
  if [ -z "$addrs" ]; then nm "$b" 2>/dev/null | grep -q ' nl_ht_' && { echo 0; return; }; echo nosym; return; fi
  (ulimit -v 8388608; env -i PATH=/usr/bin:/bin HOME="$HOME" "${SYNV[@]}" valgrind --tool=callgrind --callgrind-out-file="$T/cg.$2" "$b" --load "$NOP" >/dev/null 2>&1)
  # shellcheck disable=SC2086
  python3 -I "$HERE/cg-calls.py" "$T/cg.$2" $addrs
}
NC=$(reindex_calls "$BIN" cand); NR=$(reindex_calls "$REF" ref)
if [ "$NR" = nosym ] || [ "$NC" = nosym ]; then bad "(a) binary has no symbols (nm), cannot resolve nl_ht_reindex (cand $NC ref $NR)"
elif [ "$NC" = 0 ] && [ "$NR" -gt 0 ]; then echo "PASS (a) nl_ht_reindex calls: cand $NC ref $NR"
elif [ "$NR" -le 0 ]; then bad "(a) counting self-check failed: ref has $NR calls (expected > 0), cand $NC"
else bad "(a) nl_ht_reindex calls: cand $NC (must be 0) ref $NR"; fi
# (b1) hash-table probe on the forced relocation path
for w in cand ref; do b=$BIN; [ $w = ref ] && b=$REF
  (cd "$T" && ulimit -v 8388608 && env NELISP_COLD_FORCE_RELOCATE=1 timeout 120 "$b" --load "$HERE/ht-probe.el" > "$T/ht.$w" 2>&1 </dev/null); echo "rc=$?" >> "$T/ht.$w"; done
if [ "$(wc -l < "$T/ht.ref")" -ge 8 ] && grep -q '^rc=0$' "$T/ht.ref" && cmp -s "$T/ht.cand" "$T/ht.ref"; then echo "PASS (b) ht-probe equal under NELISP_COLD_FORCE_RELOCATE=1 ($(wc -l < "$T/ht.ref") lines)"
else echo "FAIL (b) ht-probe differs or failed"; diff "$T/ht.cand" "$T/ht.ref" | head -4 | cut -c1-160; fail=1; fi
# (b2) Doc 240 launcher hash-get1024 result, forced relocation (result equality only)
drv() { local k d; k=$(basename "$(dirname "$1")" | sed -n 's/^build-on//p'); d=$NB/run/b$k; [ -r "$d/benchmark-driver-native-on.el" ] || d=$NB/run/b33; echo "$d"; }
DC=${COLD_INIT_DRIVERS:-$(drv "$BIN")}; DR=${COLD_INIT_REF_DRIVERS:-$(drv "$REF")}
launch_result() { # BIN DRIVERDIR SIDE -> result string
  local b=$1 d=$2 s=$3 sha; mkdir -p "$T/$s" "$T/$s-scratch"; sha=$(sha256sum "$b" | cut -d' ' -f1)
  "$W/tools/nelisp-application-image-produce.sh" "$b" "$d/benchmark-driver-native-on.el" hash-get1024 "$T/$s/hash-get1024.img" >/dev/null 2>&1 || { echo PRODUCE-FAILED; return; }
  (ulimit -v 8388608; env NELISP_COLD_FORCE_RELOCATE=1 NELISP_SCRATCH_ROOT="$T/$s-scratch" BENCH_ROW=hash-get1024 BENCH_N=16384 BENCH_HIGH=16384 BENCH_WARMUPS=0 \
    NELISP_APP_IMAGE_DIR="$T/$s" NELISP_LANE_IMAGE="$b" NELISP_LANE_IMAGE_SHA256="$sha" timeout 300 "$W/tools/nelisp-application-image-launch.sh" --load PLACEHOLDER </dev/null 2>&1) \
    | sed -n 's/.*BENCH-CHECK n=16384 result=\([^ ]*\) elapsed=.*/\1/p' | head -1
}
LC=$(launch_result "$BIN" "$DC" cand); LR=$(launch_result "$REF" "$DR" ref)
if [ -n "$LR" ] && [ "$LR" != PRODUCE-FAILED ] && [ "$LC" = "$LR" ]; then echo "PASS (b) hash-get1024 launcher result equal on forced relocation: $LR"
else echo "FAIL (b) hash-get1024 launcher result cand '${LC:-none}' ref '${LR:-none}'"; fail=1; fi
[ $fail = 0 ] && { echo S2.1-PASS; exit 0; }
echo S2.1-FAIL; exit 1
