#!/bin/sh
# Supplementary long-interval S8 rows (see s8-supplement-long.json).
# Usage: campaign-long.sh NEW_IMAGE NEW_DRIVER_DIR OUT_ROOT [ROW...]
# Uses the same engines, driver and lane wrapper as ../s8-campaign/campaign.sh.
set -u
NEW=$1; DD=$2; OUT=$3; shift 3
X=$HOME/.cache/tmp/nelisp-v8-native-20261029/out/gnu-native-batch6/experimental-speedup-20261031
PB=$HOME/.codex/skills/nelisp-gnu-gap-bench/scripts/paired_bench.py
GNU=/usr/local/bin/emacs-31.1
SHA=$(sha256sum "$NEW" | cut -d' ' -f1)
cat > "$DD/lane-new" <<W
#!/bin/sh
NELISP_LANE=on NELISP_LANE_IMAGE=$NEW NELISP_LANE_IMAGE_SHA256=$SHA NELISP_SCRATCH_ROOT=$DD/scratch exec python3 -I $DD/lane_runner_phased.py "\$@"
W
chmod +x "$DD/lane-new"
ROWS=${*:-"vector-allocate64 string-slice16 string-concat16 hash-get1024"}
for ROW in $ROWS; do
  case $ROW in
    vector-allocate64|string-concat16|string-slice16) N1=1048576 N2=2097152 N4=4194304 ;;
    hash-get1024) N1=65536 N2=131072 N4=262144 ;;
    *) echo "ROW-UNKNOWN $ROW"; continue ;;
  esac
  DRIVER=$HOME/.cache/tmp/nelisp-cross-language-20261029-t8wGC4/driver.el
  export BENCH_ROW=$ROW BENCH_HIGH=$N4 BENCH_WARMUPS=2 BENCH_ENGINE=gnu
  unset BENCH_BROKEN
  for IV in short long; do
    if [ $IV = short ]; then LO=$N1 HI=$N2; else LO=$N2 HI=$N4; fi
    echo "== $ROW $IV low=$LO high=$HI"
    python3 "$PB" --engine "old=$X/lane-on" --engine "new=$DD/lane-new" --engine "gnu=$GNU" \
      --driver "$DRIVER" --low "$LO" --high "$HI" --reps 5 --cpu 1 --out "$OUT/$ROW-$IV" || echo "ROW-FAILED $ROW $IV"
  done
done
