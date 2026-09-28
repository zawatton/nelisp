#!/bin/sh
# nelisp-gc-pause-budget.sh --- pass/fail a GC pause budget for one scenario
#
# Usage:
#   nelisp-gc-pause-budget.sh --scenario a|b|c|d --budget-ms MS
#                             [--runs N] [--min-collections K]
#                             [--replay-log FILE]
#
# Scenarios (see test/nelisp-gc-pause-budget-driver.el for the workload):
#   a  garbage-only cons loop        (no persistent live set)
#   b  garbage-only string/split-string loop
#   c  100k live objects + garbage
#   d  1.3M live objects + garbage
#
# Runs NELISP_BIN (required unless --replay-log is given) --runs times
# (default 1), each time loading a tiny generated config file of `setq'
# forms followed by test/nelisp-gc-pause-budget-driver.el, exactly the
# pattern documented in scripts/nelisp-gc-pause-growth.el for its own
# parameters. Each run prints one GCB-RESULT line; this script aggregates:
#   - max pause  = the max over ALL collections across ALL runs
#   - median pause = the MIN of each run's own median (min-of-runs, so one
#     unlucky noisy run cannot inflate the reported "typical" pause)
# and requires at least --min-collections (default 5) total collections
# before it will certify a verdict at all -- too few collections means the
# budget was never actually exercised, which must not read as PASS.
#
# Exit 0 iff the aggregated max pause <= budget AND enough collections were
# observed; exit 1 on a budget miss; exit 2 on a setup/measurement problem
# (bad args, missing binary, too few collections). GCB-SUMMARY is always
# printed so a human or the ledger's `cmd:` detail line has the numbers.
#
# --replay-log FILE checks a previously captured run's output instead of
# invoking the binary. Scenario d (1.3M live objects) needs this: building
# the live set alone measured over 100s on the reference binary, far past
# any ledger `cmd:` timeout, so its budget check replays a frozen capture
# (see tools/ai/gc-pause-progress.org for the evidence entry that pins that
# capture's sha256) rather than rebuilding the scenario on every check.
# Regenerate that capture by running this script with --scenario d and no
# --replay-log directly (several minutes, NELISP_BIN required), not through
# the ledger.
#
# Logs land under ${GCB_LOG_ROOT:-$HOME/.cache/tmp/gc-ledger}, never /tmp.

set -eu

die() { echo "nelisp-gc-pause-budget.sh: $*" >&2; exit 2; }

usage() {
  cat >&2 <<'EOF'
usage: nelisp-gc-pause-budget.sh --scenario a|b|c|d --budget-ms MS
                                 [--runs N] [--min-collections K]
                                 [--replay-log FILE]
EOF
}

scenario=""
budget_ms=""
runs=1
min_collections=5
replay_log=""

while [ $# -gt 0 ]; do
  case "$1" in
    --scenario) scenario="$2"; shift 2 ;;
    --budget-ms) budget_ms="$2"; shift 2 ;;
    --runs) runs="$2"; shift 2 ;;
    --min-collections) min_collections="$2"; shift 2 ;;
    --replay-log) replay_log="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) usage; die "unknown argument: $1" ;;
  esac
done

case "$scenario" in
  a|b|c|d) ;;
  *) usage; die "--scenario must be one of a b c d, got: ${scenario:-<none>}" ;;
esac
case "$budget_ms" in
  ''|*[!0-9]*) usage; die "--budget-ms must be a non-negative integer, got: ${budget_ms:-<none>}" ;;
esac
case "$runs" in ''|*[!0-9]*|0) die "--runs must be a positive integer" ;; esac
case "$min_collections" in ''|*[!0-9]*) die "--min-collections must be a non-negative integer" ;; esac

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
driver="$script_dir/nelisp-gc-pause-budget-driver.el"
[ -r "$driver" ] || die "driver not found: $driver"

log_root=${GCB_LOG_ROOT:-"$HOME/.cache/tmp/gc-ledger"}
mkdir -p "$log_root" || die "cannot create log dir: $log_root"

budget_us=$((budget_ms * 1000))

# Per-scenario workload config, matching the sizing measured against the
# reference binary (see the worklog for the tuning runs): each fits well
# inside a single-digit-to-twenty-second run except (d), which is never
# run live here -- see --replay-log above.
scenario_config() {
  case "$1" in
    a) echo "cons 0 20000 26" ;;
    b) echo "str 0 150 30" ;;
    c) echo "cons 100000 60000 19" ;;
    d) echo "cons 1300000 200000 15" ;;
  esac
}

all_collections=0
all_max=0
medians=""

collect_line() {
  # $1 = one GCB-RESULT line; updates all_collections/all_max, appends the
  # run's median to $medians (space-separated) for the min-of-runs step.
  line="$1"
  c=$(printf '%s\n' "$line" | sed -n 's/.*collections=\([0-9]*\).*/\1/p')
  med=$(printf '%s\n' "$line" | sed -n 's/.*median-pause-us=\([0-9]*\).*/\1/p')
  mx=$(printf '%s\n' "$line" | sed -n 's/.*max-pause-us=\([0-9]*\).*/\1/p')
  [ -n "$c" ] && [ -n "$med" ] && [ -n "$mx" ] || die "unparsable GCB-RESULT line: $line"
  all_collections=$((all_collections + c))
  [ "$mx" -gt "$all_max" ] && all_max=$mx
  medians="$medians $med"
}

if [ -n "$replay_log" ]; then
  [ -r "$replay_log" ] || die "--replay-log not readable: $replay_log"
  found=0
  while IFS= read -r line; do
    case "$line" in
      GCB-RESULT*) collect_line "$line"; found=1 ;;
    esac
  done < "$replay_log"
  [ "$found" = 1 ] || die "no GCB-RESULT line in replay log: $replay_log"
else
  [ -n "${NELISP_BIN:-}" ] || die "NELISP_BIN is required for a live run (or pass --replay-log)"
  [ -x "$NELISP_BIN" ] || die "NELISP_BIN is not executable: $NELISP_BIN"
  set -- $(scenario_config "$scenario")
  mode=$1; live_n=$2; turn_alloc=$3; max_turns=$4
  run_dir=$(mktemp -d "$log_root/nelisp-gc-pause-budget.XXXXXX") || die "mktemp failed under $log_root"
  trap 'rm -rf "$run_dir"' EXIT HUP INT TERM
  i=1
  while [ "$i" -le "$runs" ]; do
    cfg="$run_dir/cfg-$i.el"
    out="$run_dir/run-$i.log"
    {
      printf "(setq gcb-mode '%s)\n" "$mode"
      printf '(setq gcb-live-n %s)\n' "$live_n"
      printf '(setq gcb-turn-alloc %s)\n' "$turn_alloc"
      printf '(setq gcb-max-turns %s)\n' "$max_turns"
    } > "$cfg"
    if ! (cd "$repo" && "$NELISP_BIN" --load "$cfg" --load "$driver") > "$out" 2>&1; then
      cat "$out" >&2
      die "run $i of $runs exited non-zero (scenario $scenario)"
    fi
    line=$(grep '^GCB-RESULT' "$out" || true)
    [ -n "$line" ] || { cat "$out" >&2; die "run $i produced no GCB-RESULT line"; }
    collect_line "$line"
    i=$((i + 1))
  done
fi

# min-of-runs median: the smallest per-run median, not the median of medians.
median_us=""
for m in $medians; do
  if [ -z "$median_us" ] || [ "$m" -lt "$median_us" ]; then median_us=$m; fi
done
median_us=${median_us:-0}

if [ "$all_collections" -lt "$min_collections" ]; then
  printf 'GCB-SUMMARY scenario=%s runs=%s collections=%s median-us=%s max-us=%s budget-ms=%s verdict=INCONCLUSIVE\n' \
    "$scenario" "$runs" "$all_collections" "$median_us" "$all_max" "$budget_ms"
  die "only $all_collections collections observed, need >= $min_collections"
fi

if [ "$all_max" -le "$budget_us" ]; then
  verdict=PASS
else
  verdict=FAIL
fi
printf 'GCB-SUMMARY scenario=%s runs=%s collections=%s median-us=%s max-us=%s budget-ms=%s verdict=%s\n' \
  "$scenario" "$runs" "$all_collections" "$median_us" "$all_max" "$budget_ms" "$verdict"

[ "$verdict" = PASS ]
