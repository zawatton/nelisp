#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
GUI_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)

usage() {
  cat <<'EOF'
Usage: NEMACS_REAL_DISPLAY=:0 scripts/verify-gui-real.sh

Opt-in smoke for a real desktop X display. Without NEMACS_REAL_DISPLAY this
script exits before launching or manipulating the GUI.

Artifacts:
  tmp-diag/YYYY-MM-DD-real-machine/
    env.txt
    clean.png or clean.xwd
    typed-30chars.png or typed-30chars.xwd
    after-relaunch.png or after-relaunch.xwd
    transport-snapshot.txt
    launch.log

Environment:
  NEMACS_REAL_DISPLAY        required real DISPLAY, for example :0
  NEMACS_REAL_ARTIFACT_DIR   override artifact directory
  NEMACS_REAL_TRANSPORT_DIR  override isolated transport directory
  NEMACS_REAL_KEY_SETTLE     delay after key/type actions, default 1
EOF
}

case "${1:-}" in
  -h|--help|help)
    usage
    exit 0
    ;;
  "")
    ;;
  *)
    usage >&2
    exit 2
    ;;
esac

if [ -z "${NEMACS_REAL_DISPLAY:-}" ]; then
  printf 'NEMACS_REAL_DISPLAY is not set; refusing to touch a real desktop.\n' >&2
  printf 'Example: NEMACS_REAL_DISPLAY=:0 %s\n' "$0" >&2
  exit 2
fi

for t in xwininfo xdotool; do
  command -v "$t" >/dev/null 2>&1 || {
    printf 'missing %s -- install x11-utils/xdotool before running real-machine smoke\n' "$t" >&2
    exit 2
  }
done

if command -v import >/dev/null 2>&1; then
  screenshot_cmd=import
elif command -v xwd >/dev/null 2>&1; then
  screenshot_cmd=xwd
else
  printf 'missing screenshot tool: install imagemagick (import) or x11-apps (xwd)\n' >&2
  exit 2
fi

day=$(date +%F)
artifact_dir=${NEMACS_REAL_ARTIFACT_DIR:-$GUI_ROOT/tmp-diag/$day-real-machine}
transport_dir=${NEMACS_REAL_TRANSPORT_DIR:-$artifact_dir/transport}
mkdir -p "$artifact_dir" "$transport_dir"
artifact_dir=$(CDPATH= cd -- "$artifact_dir" && pwd)
transport_dir=$(CDPATH= cd -- "$transport_dir" && pwd)

test_file="$artifact_dir/real-smoke.org"
printf '* real machine smoke\ninitial\n' >"$test_file"

dnum=$(printf '%s' "$NEMACS_REAL_DISPLAY" | sed -n 's/^[0-9]$/&/p; s/.*:\([0-9]\).*/\1/p' | head -1)
dnum=${dnum:-0}
key_settle=${NEMACS_REAL_KEY_SETTLE:-1}
gui_pid=""
before_windows="$artifact_dir/windows-before.txt"
after_windows="$artifact_dir/windows-after.txt"

list_windows() {
  DISPLAY="$NEMACS_REAL_DISPLAY" xwininfo -root -children 2>/dev/null |
    awk '/0x[0-9a-f]+.*[0-9]+x[0-9]+\+/{print $1}' |
    LC_ALL=C sort -u
}

capture() {
  local name=$1
  if [ "$screenshot_cmd" = "import" ]; then
    DISPLAY="$NEMACS_REAL_DISPLAY" import -window root "$artifact_dir/$name.png"
  else
    DISPLAY="$NEMACS_REAL_DISPLAY" xwd -root -silent -out "$artifact_dir/$name.xwd"
  fi
}

snapshot_transport() {
  {
    printf 'transport_dir=%s\n' "$transport_dir"
    printf 'timestamp=%s\n' "$(date -Is)"
    find "$transport_dir" -maxdepth 2 -printf '%y %s %p\n' 2>/dev/null | sort
    printf '\n--- small file contents ---\n'
    find "$transport_dir" -maxdepth 1 -type f -size -4096c -print 2>/dev/null | sort |
      while IFS= read -r f; do
        printf '\n[%s]\n' "${f#$transport_dir/}"
        tr '\000' ' ' <"$f" | sed -n '1,20p'
      done
  } >"$artifact_dir/transport-snapshot.txt"
}

cleanup() {
  if [ -n "$gui_pid" ] && kill -0 "$gui_pid" 2>/dev/null; then
    kill "$gui_pid" 2>/dev/null || true
    for _ in $(seq 1 30); do
      kill -0 "$gui_pid" 2>/dev/null || break
      sleep 0.1
    done
  fi
  snapshot_transport || true
}
trap cleanup EXIT

{
  printf 'timestamp=%s\n' "$(date -Is)"
  printf 'gui_root=%s\n' "$GUI_ROOT"
  printf 'display=%s\n' "$NEMACS_REAL_DISPLAY"
  printf 'transport_dir=%s\n' "$transport_dir"
  printf 'artifact_dir=%s\n' "$artifact_dir"
  printf '\n--- environment ---\n'
  env | LC_ALL=C sort | grep -E '^(DISPLAY|XDG_|GDM|GNOME|GTK|QT|NEMACS|NELISP|SKK|LANG|LC_)=' || true
  printf '\n--- xprop RESOURCE_MANAGER ---\n'
  DISPLAY="$NEMACS_REAL_DISPLAY" xprop -root RESOURCE_MANAGER 2>&1 || true
  printf '\n--- xdpyinfo ---\n'
  command -v xdpyinfo >/dev/null 2>&1 && DISPLAY="$NEMACS_REAL_DISPLAY" xdpyinfo 2>&1 || true
  printf '\n--- xrandr ---\n'
  command -v xrandr >/dev/null 2>&1 && DISPLAY="$NEMACS_REAL_DISPLAY" xrandr --current 2>&1 || true
} >"$artifact_dir/env.txt"

printf '== real display smoke ==\n'
printf 'display:   %s\n' "$NEMACS_REAL_DISPLAY"
printf 'artifacts: %s\n' "$artifact_dir"
printf 'transport: %s\n' "$transport_dir"

list_windows >"$before_windows"

DISPLAY="$NEMACS_REAL_DISPLAY" \
NEMACS_TRANSPORT_DIR="$transport_dir" \
NEMACS_X_DISPLAY_NUM="$dnum" \
NEMACS_SKIP_GUI_PKILL=1 \
NEMACS_BUILD_SKIP_GUI_PKILL=1 \
setsid bash "$GUI_ROOT/bin/nemacs" -Q "$test_file" >"$artifact_dir/launch.log" 2>&1 &
gui_pid=$!

wid=""
for _ in $(seq 1 90); do
  if grep -q 'real-smoke.org' "$transport_dir/nemacs-modeline" 2>/dev/null; then
    list_windows >"$after_windows"
    wid=$(comm -13 "$before_windows" "$after_windows" | head -1)
    [ -n "$wid" ] && break
  fi
  sleep 0.5
done

if [ -z "$wid" ]; then
  printf 'GUI did not map a new window on %s; see %s/launch.log\n' "$NEMACS_REAL_DISPLAY" "$artifact_dir" >&2
  exit 1
fi

DISPLAY="$NEMACS_REAL_DISPLAY" xdotool windowmove "$wid" 40 40 2>/dev/null || true
DISPLAY="$NEMACS_REAL_DISPLAY" xdotool windowsize "$wid" 820 540 2>/dev/null || true
DISPLAY="$NEMACS_REAL_DISPLAY" xdotool windowfocus "$wid" 2>/dev/null || true
sleep "$key_settle"
capture clean

DISPLAY="$NEMACS_REAL_DISPLAY" xdotool type --window "$wid" --clearmodifiers ' real-machine-smoke-30chars-ok'
sleep "$key_settle"
capture typed-30chars
snapshot_transport

kill "$gui_pid" 2>/dev/null || true
for _ in $(seq 1 30); do
  kill -0 "$gui_pid" 2>/dev/null || break
  sleep 0.1
done
gui_pid=""

DISPLAY="$NEMACS_REAL_DISPLAY" \
NEMACS_TRANSPORT_DIR="$transport_dir" \
NEMACS_X_DISPLAY_NUM="$dnum" \
NEMACS_SKIP_GUI_PKILL=1 \
NEMACS_BUILD_SKIP_GUI_PKILL=1 \
setsid bash "$GUI_ROOT/bin/nemacs" -Q "$test_file" >>"$artifact_dir/launch.log" 2>&1 &
gui_pid=$!

sleep 3
capture after-relaunch
snapshot_transport

printf 'PASS: real-machine smoke artifacts saved under %s\n' "$artifact_dir"
