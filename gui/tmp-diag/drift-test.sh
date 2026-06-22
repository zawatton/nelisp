#!/bin/bash
# Drift diagnostic: launch clean, type a known count of identical chars,
# screenshot.  Run on the real display:  bash tmp-diag/drift-test.sh
set -u
GUI=~/Cowork/Notes/dev/nelisp-gui
OUT="$GUI/tmp-diag/drift.png"
N=20   # number of 'a' to type

echo "1) cleaning old GUI + bridge sessions..."
pkill -x nemacs-win.bin 2>/dev/null
pkill -f nemacs-gui-file-bridge 2>/dev/null
sleep 1
: > /tmp/skk-test.org

echo "2) launching nemacs on :0 (first launch may rebuild ~30s)..."
cd "$GUI" || exit 1
DISPLAY=:0 ./bin/nemacs /tmp/skk-test.org >/tmp/nemacs-drift.log 2>&1 &
for _ in $(seq 1 60); do
  grep -q skk-test /tmp/nemacs-modeline 2>/dev/null && break
  sleep 1
done
sleep 3

echo "3) focusing window + typing $N 'a'..."
WID=$(DISPLAY=:0 wmctrl -l 2>/dev/null | grep -iv 'mutter\|x11-frames' | tail -1 | awk '{print $1}')
[ -z "$WID" ] && WID=$(DISPLAY=:0 xdotool search --onlyvisible --name . 2>/dev/null | tail -1)
echo "   window=$WID"
DISPLAY=:0 xdotool windowactivate --sync "$WID" 2>/dev/null
DISPLAY=:0 xdotool windowfocus "$WID" 2>/dev/null
sleep 1
DISPLAY=:0 xdotool type --delay 100 "$(printf 'a%.0s' $(seq 1 $N))"
sleep 2

echo "4) capturing screenshot -> $OUT"
DISPLAY=:0 import -window root "$OUT" 2>/dev/null \
  || DISPLAY=:0 import -window "$WID" "$OUT" 2>/dev/null

echo "---- report ----"
echo "cfg font : $(tail -c 6 /tmp/nemacs.cfg 2>/dev/null | tr -cd 'a-z0-9x')"
echo "modeline : $(cat /tmp/nemacs-modeline 2>/dev/null)"
echo "view     : $(head -1 /tmp/nemacs-view 2>/dev/null)"
echo "saved    : $OUT  ($(identify "$OUT" 2>/dev/null | awk '{print $3}'))"
echo "----------------"
echo "Done. Tell Claude it's saved (and paste the report lines above)."
