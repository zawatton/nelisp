#!/bin/sh
# S8.3 campaign: GNU 31.1 bytecode / old (recorded r3 ON) / new (final ON),
# same driver arguments, same N per row as the frozen protocol, paired_bench.
# Usage: campaign.sh NEW_IMAGE NEW_DRIVER_DIR OUT_ROOT [ROW...]
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
ROWS=${*:-"integer-sum function-call2 nested-function-call2 vector-read vector-write vector-allocate64 string-slice16 string-concat16 hash-get1024"}
for ROW in $ROWS; do
  case $ROW in
    integer-sum) LOW=4000000 HIGH=16000000 ;;
    vector-read|vector-write|function-call2|nested-function-call2) LOW=1048576 HIGH=4194304 ;;
    vector-allocate64|string-concat16|string-slice16) LOW=262144 HIGH=1048576 ;;
    hash-get1024) LOW=16384 HIGH=65536 ;;
  esac
  if [ "$ROW" = nested-function-call2 ]; then
    DRIVER=$X/gnu-comparison/nested-primary-gnu.el; GE=$X/gnu-comparison/emacs-gnu
    export GNU_EMACS_BIN=$GNU V8_FIXTURE_MODULE=$HOME/.cache/tmp/nelisp-v8-native-20261029/out/qualification/fixtures/application.el
  else
    DRIVER=$HOME/.cache/tmp/nelisp-cross-language-20261029-t8wGC4/driver.el; GE=$GNU
  fi
  export BENCH_ROW=$ROW BENCH_HIGH=$HIGH BENCH_WARMUPS=2 BENCH_ENGINE=gnu
  unset BENCH_BROKEN
  echo "== $ROW low=$LOW high=$HIGH"
  python3 "$PB" --engine "old=$X/lane-on" --engine "new=$DD/lane-new" --engine "gnu=$GE" \
    --driver "$DRIVER" --low "$LOW" --high "$HIGH" --reps 7 --cpu 1 --out "$OUT/$ROW" || echo "ROW-FAILED $ROW"
done
