#!/usr/bin/env bash
# Usage: tools/smoke-run.sh [org magit evil dired eat skk files search completion]
# SMOKE_TIMEOUT overrides the per-editor deadline. Defaults in seconds:
# GNU 15, NeLisp 120 for Org and 30 for the other scenarios.
set -euo pipefail
source "$(dirname -- "${BASH_SOURCE[0]}")/smoke-common.sh"

if (($# == 0)); then
    set -- "${smoke_cases[@]}"
fi
for scenario; do
    case " ${smoke_cases[*]} " in
        *" $scenario "*) ;;
        *) printf 'Unknown smoke scenario: %s\n' "$scenario" >&2; exit 2 ;;
    esac
done
smoke_prepare smoke
printf 'Evidence: %s\n' "$smoke_run"
export NELISP_SMOKE_DRIVER=$smoke_suite/batch.el
overall=0
start=$SECONDS
for scenario; do
    smoke_make_fixture "$scenario"
    export NELISP_SMOKE_CASE=$scenario
    statuses=()
    for engine in gnu nelisp; do
        smoke_reset_fixture "$scenario"
        rm -rf -- "$smoke_home"
        mkdir -p -- "$smoke_home/.emacs.d"
        prefix=$smoke_run/$scenario.$engine
        case "$engine:$scenario" in
            gnu:*) limit=15 ;;
            nelisp:org) limit=120 ;;
            *) limit=30 ;;
        esac
        status=0
        smoke_editor "$engine" "${SMOKE_TIMEOUT:-$limit}" "$prefix.out" "$prefix.err" || status=$?
        statuses+=("$status")
        sed -n '/^SMOKE /p' "$prefix.out" > "$prefix.transcript"
        if ! grep -Fxq "SMOKE END (\"$scenario\" 0)" "$prefix.transcript" ||
           grep -q '^SMOKE ERROR ' "$prefix.transcript"; then
            statuses[-1]="${status}/incomplete-or-errors"
        fi
    done
    gnu=$smoke_run/$scenario.gnu.transcript
    nelisp=$smoke_run/$scenario.nelisp.transcript
    diff_status=0
    diff -u --label GNU --label NeLisp "$gnu" "$nelisp" > "$smoke_run/$scenario.diff" || diff_status=$?
    verdict=PASS
    if [[ ${statuses[*]} != '0 0' ]] || ((diff_status > 1)); then
        verdict=ERROR
    elif ((diff_status == 1)); then
        verdict=DIFF
    fi
    printf '%-11s %-5s GNU=%s NeLisp=%s\n' "$scenario" "$verdict" "${statuses[@]}"
    if ((diff_status == 1)); then
        awk '
          FILENAME == ARGV[1] { gnu[FNR] = $0; next }
          { seen = FNR }
          !reported && (!(FNR in gnu) || $0 != gnu[FNR]) {
            printf "  transcript line %d\n  GNU    %s\n  NeLisp %s\n", FNR,
                   ((FNR in gnu) ? gnu[FNR] : "<end>"), $0
            reported = 1
          }
          END {
            if (!reported && ((seen+1) in gnu))
              printf "  transcript line %d\n  GNU    %s\n  NeLisp <end>\n", seen+1, gnu[seen+1]
          }
        ' "$gnu" "$nelisp"
    fi
    if [[ $verdict != PASS ]]; then
        overall=1
        for engine in gnu nelisp; do
            status=${statuses[0]}
            if [[ $engine == nelisp ]]; then status=${statuses[1]}; fi
            if [[ $status == 124/* || $status == 137/* ]]; then
                last_step=$(sed -n '/^STEP /h;${x;p;}' "$smoke_run/$scenario.$engine.out")
                printf '  %s timed out after %s\n' "$engine" "${last_step:-startup}"
            fi
        done
    fi
done
printf 'Elapsed: %s s\n' "$((SECONDS - start))"
exit "$overall"
