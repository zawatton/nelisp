#!/usr/bin/env sh
set -eu

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../../.." && pwd -P)}
GUI="$ROOT/apps/nemacs-next/frontends/gui/nemacs-next-gui"

need() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "nemacs-next-gui-smoke: missing required command: $1" >&2
    exit 77
  fi
}

need Xvfb
need xdotool
need xwininfo
need xwd
need convert
need xterm

tmp_dir=${TMPDIR:-/tmp}/nemacs-next-gui-smoke.$$
init_dir=$tmp_dir/init
out=$tmp_dir/gui.out
xvfb_log=$tmp_dir/xvfb.log
xvfb_display_file=$tmp_dir/xvfb.display
xwd_file=$tmp_dir/window.xwd
png_file=$tmp_dir/window.png
target_file=$tmp_dir/saved.txt

cleanup() {
  if [ -n "${gui_pid:-}" ]; then
    kill "$gui_pid" 2>/dev/null || true
    wait "$gui_pid" 2>/dev/null || true
  fi
  if [ -n "${xvfb_pid:-}" ]; then
    kill "$xvfb_pid" 2>/dev/null || true
    wait "$xvfb_pid" 2>/dev/null || true
  fi
  rm -rf "$tmp_dir"
}
trap cleanup EXIT HUP INT TERM

rm -rf "$tmp_dir"
mkdir -p "$init_dir"
: >"$target_file"

if [ -n "${NEMACS_NEXT_GUI_SMOKE_DISPLAY:-}" ]; then
  display=":$NEMACS_NEXT_GUI_SMOKE_DISPLAY"
  Xvfb "$display" -screen 0 1024x768x24 -listen tcp -nolisten unix >"$xvfb_log" 2>&1 &
  xvfb_pid=$!
else
  Xvfb -displayfd 3 -screen 0 1024x768x24 -listen tcp -nolisten unix >"$xvfb_log" 2>&1 3>"$xvfb_display_file" &
  xvfb_pid=$!
  i=0
  while [ "$i" -lt 50 ] && [ ! -s "$xvfb_display_file" ]; do
  if ! kill -0 "$xvfb_pid" 2>/dev/null; then
    break
  fi
    i=$((i + 1))
    sleep 0.1
  done
  display="localhost:$(cat "$xvfb_display_file" 2>/dev/null || true)"
fi
sleep 0.5

if ! kill -0 "$xvfb_pid" 2>/dev/null; then
  sed -n '1,12p' "$xvfb_log" >&2
  echo "nemacs-next-gui-smoke: skip Xvfb could not create a listening socket in this environment" >&2
  exit 77
fi
if [ "$display" = "localhost:" ] || [ "$display" = ":" ]; then
  sed -n '1,12p' "$xvfb_log" >&2
  echo "nemacs-next-gui-smoke: skip Xvfb did not report a display" >&2
  exit 77
fi

DISPLAY=$display \
NEMACS_USER_EMACS_DIRECTORY="$init_dir" \
NEMACS_NEXT_GUI_WIDTH=100 \
NEMACS_NEXT_GUI_HEIGHT=28 \
timeout 90s "$GUI" >"$out" 2>&1 &
gui_pid=$!

window_id=
i=0
while [ "$i" -lt 100 ]; do
  window_id=$(DISPLAY=$display xdotool search --sync --onlyvisible --class NemacsNextGUI 2>/dev/null | tail -1 || true)
  [ -n "$window_id" ] && break
  if ! kill -0 "$gui_pid" 2>/dev/null; then
    cat "$out" >&2 || true
    echo "nemacs-next-gui-smoke: GUI exited before mapping a window" >&2
    exit 1
  fi
  i=$((i + 1))
  sleep 0.1
done

if [ -z "$window_id" ]; then
  cat "$out" >&2 || true
  echo "nemacs-next-gui-smoke: no mapped GUI window found" >&2
  exit 1
fi

DISPLAY=$display xwininfo -id "$window_id" >/dev/null
echo "nemacs-next-gui-smoke: window-map ok window=$window_id"

DISPLAY=$display xwd -silent -id "$window_id" -out "$xwd_file"
convert "$xwd_file" "$png_file"
colors=$(convert "$png_file" -format %k info:)
if [ "$colors" -le 1 ]; then
  echo "nemacs-next-gui-smoke: screenshot had no rendered glyph contrast" >&2
  exit 1
fi
echo "nemacs-next-gui-smoke: screenshot-nonempty ok colors=$colors"

DISPLAY=$display xdotool windowfocus "$window_id"
sleep 0.2
DISPLAY=$display xdotool key --window "$window_id" ctrl+x ctrl+f
DISPLAY=$display xdotool type --window "$window_id" --delay 2 "$target_file"
DISPLAY=$display xdotool key --window "$window_id" Return
sleep 0.5
DISPLAY=$display xdotool type --window "$window_id" --delay 2 "abc"
DISPLAY=$display xdotool key --window "$window_id" ctrl+x ctrl+s
sleep 1
DISPLAY=$display xdotool key --window "$window_id" ctrl+x ctrl+c

i=0
while [ "$i" -lt 50 ]; do
  if ! kill -0 "$gui_pid" 2>/dev/null; then
    break
  fi
  i=$((i + 1))
  sleep 0.1
done

if [ "$(cat "$target_file")" != "abc" ]; then
  cat "$out" >&2 || true
  echo "nemacs-next-gui-smoke: scripted save content mismatch" >&2
  exit 1
fi
echo "nemacs-next-gui-smoke: scripted-save ok file=$target_file content=abc"
