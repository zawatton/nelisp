#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
export TMPDIR="$PWD/probes/results"
export XDG_CACHE_HOME="$PWD/probes/results/cache"
if [[ ${1:-} != inside ]]; then
    exec xvfb-run -e "$PWD/probes/results/xvfb-replay.err" -a -s '-screen 0 1024x768x24 -ac -nolisten tcp -dpi 96' bash "$0" inside
fi
xdpyinfo -queryExtensions > probes/results/xvfb-environment.out
python3 probes/run.py ffi --label ffi-xvfb
python3 probes/run.py xrender --timeout 90 >probes/results/xrender-run.out 2>&1 &
task_pid=$!
trap 'kill "$task_pid" 2>/dev/null || true' EXIT
window=''
for ((i=0; i<200; i++)); do
    window=$(xdotool search --onlyvisible --name '^g1-xrender-probe$' 2>/dev/null | head -1 || true)
    if [[ -n "$window" ]] && rg -q WINDOW-READY probes/results/xrender.out; then break; fi
    if ! kill -0 "$task_pid" 2>/dev/null; then cat probes/results/xrender-run.out; exit 1; fi
    sleep 0.2
done
[[ -n "$window" ]]
rg -q WINDOW-READY probes/results/xrender.out
import -window "$window" probes/results/xrender.png
xdotool key --window "$window" a
wait "$task_pid"
trap - EXIT
cat probes/results/xrender-run.out
python3 probes/pixels.py probes/results/xrender.png
