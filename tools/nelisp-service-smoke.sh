#!/bin/sh
# Doc 213: end-to-end smokes for packages/nelisp-service on a standalone
# reader.  Usage: tools/nelisp-service-smoke.sh BIN [pool|daemon|idle|all]
#
#   pool    worker pool with NeLisp children (ceiling, recycle, crash, version)
#   daemon  client-started daemon (sharing, token, version replace, idle exit)
#   idle    a listening daemon with no client must stay under
#           NELISP_SERVICE_IDLE_MAX_CORES (default 0.25) of a core
#
# Exit 0 only when every selected smoke printed SMOKE-PASS / IDLE-PASS.
# Paths are resolved from this script's location.
bin=$1
what=${2:-all}
[ -n "$bin" ] && [ -x "$bin" ] || { echo "usage: $0 BIN [pool|daemon|idle|all]" >&2; exit 2; }
root=$(cd "$(dirname "$0")/.." && pwd)
pkg="$root/packages/nelisp-service"
fail=0

run_smoke() {  # $1 = pool|daemon
    out=$(timeout 600 "$bin" --load "$pkg/test/nelisp-service-$1-smoke.el" </dev/null 2>&1)
    printf '%s\n' "$out" | grep -E '^(ok|FAIL) ' | sed "s/^/[$1] /"
    if printf '%s\n' "$out" | grep -q '^SMOKE-PASS$'; then
        echo "[$1] SMOKE-PASS"
    else
        echo "[$1] SMOKE-FAIL"; printf '%s\n' "$out" | tail -5 | sed "s/^/[$1] | /"; fail=1
    fi
}

native() {  # print a path the binary understands (Windows on MSYS)
    if command -v cygpath >/dev/null 2>&1; then cygpath -m "$1"; else printf '%s' "$1"; fi
}

cpu_seconds() {  # $1 = pid; total CPU seconds of that process
    if [ -r "/proc/$1/stat" ]; then
        awk -v hz="$(getconf CLK_TCK)" '{ print ($14 + $15) / hz }' "/proc/$1/stat"
    else
        powershell -NoProfile -Command "(Get-Process -Id $1).TotalProcessorTime.TotalSeconds"
    fi
}

idle_smoke() {
    state=$(mktemp -d)
    boot="$state/boot.el"
    printf '(setq nelisp-service-state-directory "%s")\n' "$(native "$state")" > "$boot"
    printf '(setq nelisp-service-bootstrap-args (quote (:name "idle" :idle-timeout 600)))\n' >> "$boot"
    printf '(load "%s" nil t)\n' "$(native "$pkg/bin/nelisp-service-echo-daemon.el")" >> "$boot"
    "$bin" --load "$(native "$boot")" </dev/null >/dev/null 2>&1 &
    shell_pid=$!
    sleep 8
    if [ -r "/proc/$shell_pid/stat" ]; then
        pid=$shell_pid
    else
        # MSYS: map the shell's pid to the Windows pid of the reader.
        pid=$(ps -W 2>/dev/null | awk -v p="$shell_pid" '$1 == p { print $4 }')
        [ -n "$pid" ] || pid=$(cat "/proc/$shell_pid/winpid" 2>/dev/null)
    fi
    a=$(cpu_seconds "$pid"); sleep 5; b=$(cpu_seconds "$pid")
    kill "$shell_pid" 2>/dev/null
    [ -r "/proc/$shell_pid/stat" ] || powershell -NoProfile -Command "Stop-Process -Id $pid -Force -EA SilentlyContinue" >/dev/null 2>&1
    rm -rf "$state"
    max=${NELISP_SERVICE_IDLE_MAX_CORES:-0.25}
    cores=$(awk -v a="$a" -v b="$b" 'BEGIN { printf "%.3f", (b - a) / 5 }')
    if awk -v c="$cores" -v m="$max" 'BEGIN { exit !(c < m) }'; then
        echo "[idle] $cores cores < $max  IDLE-PASS"
    else
        echo "[idle] $cores cores >= $max  IDLE-FAIL"; fail=1
    fi
}

case $what in
    pool|daemon) run_smoke "$what" ;;
    idle) idle_smoke ;;
    all) run_smoke pool; run_smoke daemon; idle_smoke ;;
    *) echo "unknown smoke: $what" >&2; exit 2 ;;
esac
exit $fail
