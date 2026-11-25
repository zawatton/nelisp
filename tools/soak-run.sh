#!/usr/bin/env bash
# Usage: tools/soak-run.sh [seconds [scenario ...]]
# Defaults: 1800 s, all smoke scenarios, 60 s samples, 600 s RSS baseline.
# Optional environment overrides for short harness checks:
# SOAK_SAMPLE_SECONDS, SOAK_WARMUP_SECONDS, SOAK_STALL_SECONDS.
set -euo pipefail
source "$(dirname -- "${BASH_SOURCE[0]}")/smoke-common.sh"

duration=${1:-1800}
if (($#)); then shift; fi
if (($# == 0)); then set -- "${smoke_cases[@]}"; fi
sample_interval=${SOAK_SAMPLE_SECONDS:-60}
warmup=${SOAK_WARMUP_SECONDS:-600}
stall=${SOAK_STALL_SECONDS:-90}
for value in "$duration" "$sample_interval" "$warmup" "$stall"; do
    if [[ ! $value =~ ^[1-9][0-9]*$ ]]; then
        printf 'Expected a positive integer number of seconds, got %s\n' "$value" >&2
        exit 2
    fi
done
for scenario; do
    case " ${smoke_cases[*]} " in
        *" $scenario "*) ;;
        *) printf 'Unknown smoke scenario: %s\n' "$scenario" >&2; exit 2 ;;
    esac
done

smoke_prepare soak
for scenario; do smoke_make_fixture "$scenario"; done
export NELISP_SMOKE_DRIVER=$smoke_suite/soak.el
export NELISP_SOAK_CASES="$*" NELISP_SOAK_SEED=$smoke_seed
export NELISP_SOAK_SAMPLE=$smoke_run/gc.current NELISP_SOAK_STOP=$smoke_run/stop
printf 'Evidence: %s\n' "$smoke_run"
printf 'duration=%s\nsample=%s\nwarmup=%s\nstall=%s\ncases=%s\n' \
    "$duration" "$sample_interval" "$warmup" "$stall" "$*" > "$smoke_run/settings"
header=$'elapsed_s\tVmRSS_KiB\tgcs_done\tforced_gc\tcases\tcase_errors\tedits\theartbeat_age_s'
printf '%s\n' "$header" | tee "$smoke_run/samples.tsv"

supervisor=
trap 'if [[ -n $supervisor ]]; then kill "$supervisor" 2>/dev/null || :; wait "$supervisor" || :; fi' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
smoke_editor nelisp "$((duration + stall + 30))" "$smoke_run/editor.out" "$smoke_run/editor.err" \
    bash -c 'printf "%s\n" "$$" > "$1"; shift; exec "$@"' smoke-pid "$smoke_run/pid" &
supervisor=$!
start=$SECONDS
while [[ ! -s $smoke_run/pid || ! -s $NELISP_SOAK_SAMPLE ]]; do
    if ! kill -0 "$supervisor" 2>/dev/null || ((SECONDS - start >= stall)); then
        printf 'FAIL: editor did not start the workload\n' | tee "$smoke_run/verdict"
        exit 1
    fi
    sleep 1
done
read -r pid < "$smoke_run/pid"
start=$SECONDS
baseline=0
peak=0
growth_failed=0
next=0
while :; do
    elapsed=$((SECONDS - start))
    if ! kill -0 "$supervisor" 2>/dev/null ||
       ! read -r rss < <(awk '/^VmRSS:/ {print $2}' "/proc/$pid/status" 2>/dev/null); then
        printf 'FAIL: editor exited at %s s\n' "$elapsed" | tee "$smoke_run/verdict"
        exit 1
    fi
    IFS=$'\t' read -r gcs forced cases errors edits heartbeat < "$NELISP_SOAK_SAMPLE"
    age=$(($(date +%s) - heartbeat))
    if ((elapsed >= next || (baseline == 0 && elapsed >= warmup))); then
        printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
            "$elapsed" "$rss" "$gcs" "$forced" "$cases" "$errors" "$edits" "$age" \
            | tee -a "$smoke_run/samples.tsv"
        if ((elapsed >= warmup)); then
            if ((baseline == 0)); then baseline=$rss; fi
            if ((rss > peak)); then peak=$rss; fi
            if ((rss * 5 > baseline * 6)); then growth_failed=1; fi
        fi
        if ((elapsed >= next)); then
            next=$((next + sample_interval))
        fi
        if ((next > duration)); then next=$duration; fi
    fi
    if ((age >= stall)); then
        printf 'FAIL: workload heartbeat stalled for %s s\n' "$age" | tee "$smoke_run/verdict"
        exit 1
    fi
    if ((elapsed >= duration)); then break; fi
    sleep 1
done
touch "$NELISP_SOAK_STOP"
status=0
wait "$supervisor" || status=$?
supervisor=
if ((status != 0)) || ! grep -Eq '^SMOKE SOAK-DONE \(' "$smoke_run/editor.out"; then
    printf 'FAIL: editor shutdown status %s or missing completion marker\n' "$status" | tee "$smoke_run/verdict"
    exit 1
fi
IFS=$'\t' read -r gcs forced cases errors edits heartbeat < "$NELISP_SOAK_SAMPLE"
if ((forced == 0 || cases == 0 || edits == 0)); then
    printf 'FAIL: workload completed %s cases, %s edits and %s explicit GCs\n' "$cases" "$edits" "$forced" | tee "$smoke_run/verdict"
    exit 1
fi
if ((baseline == 0)); then
    printf 'COMPLETE: %s s, %s cases, %s case errors, %s edits, %s explicit GCs; RSS baseline requires %s s\n' \
        "$duration" "$cases" "$errors" "$edits" "$forced" "$warmup" | tee "$smoke_run/verdict"
    exit 0
fi
growth=$(awk -v peak="$peak" -v baseline="$baseline" 'BEGIN {printf "%.2f%%", 100*(peak-baseline)/baseline}')
verdict=PASS
if ((growth_failed)); then verdict=FAIL; fi
printf '%s: %s s, baseline %s KiB, peak %s KiB (+%s), %s cases, %s case errors, %s edits, %s explicit GCs\n' \
    "$verdict" "$duration" "$baseline" "$peak" "$growth" "$cases" "$errors" "$edits" "$forced" | tee "$smoke_run/verdict"
exit "$growth_failed"
