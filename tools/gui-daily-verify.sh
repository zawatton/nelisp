#!/usr/bin/env bash
# Post-integration verification for the GUI daily-driver work.
#
# Rebuilds the bootstrap bundle and both heap images from scratch, then runs
# every gate that an integrated lane can break, sequentially (the gates share
# build/ outputs and must not race), and prints one PASS/FAIL line per gate.
# Exit status is 0 only when every selected gate passes.
#
# Usage: NELISP_BIN=/path/to/nelisp tools/gui-daily-verify.sh [GATE...]
# Gates: ccore pty wait scenario launcher layout S3.2 S3.3 S4.1 S4.2 S4.3 S5.0 S5.0b
# With no arguments every gate runs (about 1.5 h on a quiet machine).
set -u
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root" || exit 2
: "${NELISP_BIN:?NELISP_BIN must name the standalone runtime}"
export EMACS="${EMACS:-emacs}"
export C_CORE_PARITY_JOBS="${C_CORE_PARITY_JOBS:-3}"
export REDISPLAY_PARITY_JOBS="${REDISPLAY_PARITY_JOBS:-6}"
log="$root/build/gui-daily-verify"
mkdir -p "$log"

gates=("$@")
[ ${#gates[@]} -gt 0 ] || gates=(ccore pty wait scenario launcher layout S3.2 S3.3 S4.1 S4.2 S4.3 S5.0 S5.0b)
xvfb=(xvfb-run -a -s "-screen 0 1600x1000x24 -dpi 96 -nolisten tcp")

step() { # name command...
  local name=$1 start rc; shift
  start=$(date +%s)
  "$@" > "$log/$name.log" 2>&1
  rc=$?
  printf '%-9s %s %4ss  %s\n' "$name" "$([ $rc = 0 ] && echo PASS || echo FAIL)" \
    "$(( $(date +%s) - start ))" "$(tail -1 "$log/$name.log" | cut -c1-90)"
  return $rc
}

# Fresh inputs: a stale bundle or tty snapshot has produced false results before.
step bundle bash -c 'rm -f build/nemacs-bootstrap.el && make build-nelisp-bootstrap' || exit 1
rm -rf build/c-core-image/tty
step image bash tools/c-core-image.sh build || exit 1
step nw-image bin/nemacs-nw --build-image || exit 1
case " ${gates[*]} " in *" S"*) step gui-image python3 scripts/gui-daily-build.py || exit 1 ;; esac

failed=0
for gate in "${gates[@]}"; do
  case $gate in
    ccore)    step ccore bash test/nelisp-emacs-lib/c-core-parity-smoke.sh run ;;
    pty)      step pty python3 tools/nemacs-pty-smoke.py --output build/nemacs-pty-smoke/verify ;;
    wait)     step wait python3 tools/nemacs-pty-smoke.py --group wait --output build/nemacs-pty-smoke/verify-wait ;;
    scenario) step scenario python3 tools/nemacs-pty-smoke.py --group scenario --output build/nemacs-pty-smoke/verify-scenario ;;
    launcher) step launcher python3 tools/nemacs-pty-smoke.py --group launcher --output build/nemacs-pty-smoke/verify-launcher ;;
    layout)   step layout python3 tools/redisplay-layout-parity.py run --timeout 180 --output build/redisplay-layout/verify ;;
    S3.2)     step S3.2 "${xvfb[@]}" python3 scripts/gui-daily-gate.py S3.2 --init=-Q --fixture=render --require-production-quit ;;
    S3.3)     step S3.3 python3 scripts/gui-daily-gate.py S3.3 --init=-Q --fixture=metrics --dpi=96,144,192 ;;
    S4.1)     step S4.1 python3 scripts/gui-daily-gate.py S4.1 --init=-Q --fixture=skk-evil --keymaps=us,jp,de ;;
    S4.2)     step S4.2 python3 scripts/gui-daily-gate.py S4.2 --init=-Q --fixture=mouse-menu ;;
    S4.3)     step S4.3 python3 scripts/gui-daily-gate.py S4.3 --init=-Q --fixture=selections --peer=xclip --bytes=1048576 ;;
    S5.0)     step S5.0 "${xvfb[@]}" python3 scripts/gui-daily-gate.py S5.0 --init=-Q ;;
    S5.0b)    step S5.0b "${xvfb[@]}" python3 scripts/gui-daily-gate.py S5.0b --init=-Q ;;
    *) echo "unknown gate: $gate" >&2; exit 2 ;;
  esac || failed=$((failed + 1))
done
echo "gui-daily-verify: $failed failing gate(s); logs in $log"
[ $failed = 0 ]
