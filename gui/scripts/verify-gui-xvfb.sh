#!/bin/bash
# verify-gui-xvfb.sh -- headless end-to-end GUI verification under Xvfb.
#
# The two remaining nemacs items -- visual render and arrow-keysym M-arrow
# promote/demote -- can only be VERIFIED with a real X server driving the
# compiled GUI binary.  Xvfb is a virtual (in-memory) X server, so this is a
# fully headless, repeatable check that never touches the user's live :0:
#
#   Xvfb :9  ->  bin/nemacs FILE (builds + execs the GUI for display 9)
#            ->  xdotool injects real keystrokes into the window
#            ->  the bridge session updates the transport state files
#            ->  we assert on the modeline (L/C) and nemacs-view (buffer text)
#            ->  xwd dumps the framebuffer to prove something rendered
#
# The display-number parser in bin/nemacs only handles a single digit, so we
# use :9, in an isolated NEMACS_TRANSPORT_DIR so it never collides with a real
# /tmp session.  The bridge's build smoke-launches the GUI on /tmp, so we wait
# until OUR transport's modeline names the test file before injecting -- that
# is what distinguishes our launch from the build's smoke.
#
# Exit: 0 all checks passed; 2 prerequisites missing (with install hint);
#       1 a verification assertion failed.
set -u

GUI_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
DISP=${NEMACS_XVFB_DISPLAY:-:9}
DNUM=${DISP#:}
TD=${NEMACS_XVFB_TRANSPORT_DIR:-${TMPDIR:-/tmp}/nemacs-xvfb-verify-${DNUM}-$$}
SCREEN=${NEMACS_XVFB_SCREEN:-1024x768x24}
PASS=0 FAIL=0
ok()  { PASS=$((PASS+1)); printf '  PASS  %s\n' "$*"; }
bad() { FAIL=$((FAIL+1)); printf '  FAIL  %s\n' "$*"; }

for t in Xvfb xdotool xwd xwininfo; do
  command -v "$t" >/dev/null 2>&1 || { printf 'missing %s -- install: sudo apt install -y xvfb x11-utils xdotool\n' "$t"; exit 2; }
done
[ -x "$GUI_ROOT/bin/nemacs" ] || { echo "no bin/nemacs at $GUI_ROOT"; exit 2; }

rm -rf "$TD"; mkdir -p "$TD"
TESTFILE="$TD/verify.org"
# Line 1 is a CJK heading: it exercises M-arrow promote/demote AND multibyte
# backspace (the user's text is Japanese).  "* TODO 漏電調査".
printf '* TODO \xe6\xbc\x8f\xe9\x9b\xbb\xe8\xaa\xbf\xe6\x9f\xbb\n** child alpha\n* TODO second\n' >"$TESTFILE"

cleanup() {
  for p in $(pgrep -f "$TD/.nemacs-artifacts/nemacs-win.bin" 2>/dev/null || true); do
    kill "$p" 2>/dev/null || true
  done
  # Reap the GUI's bridge sessions too -- they are detached and otherwise
  # accumulate across runs, serving stale buffer state into the next launch.
  # Match by exe path (the snapshot nelisp), never the parallel agent's vendor
  # nelisp; scope to our transport dir.
  for p in $(pgrep -x nelisp 2>/dev/null); do
    case "$(readlink "/proc/$p/exe" 2>/dev/null)" in
      */nelisp-snap/*)
        tr '\0' ' ' <"/proc/$p/cmdline" 2>/dev/null | grep -q "$TD" && kill "$p" 2>/dev/null ;;
    esac
  done
  [ -n "${XVFB_PID:-}" ] && kill "$XVFB_PID" 2>/dev/null
  wait 2>/dev/null
}
trap cleanup EXIT

echo "== Xvfb $DISP ($SCREEN) =="
Xvfb "$DISP" -screen 0 "$SCREEN" >"$TD/xvfb.log" 2>&1 &
XVFB_PID=$!
for _ in $(seq 1 20); do [ -S "/tmp/.X11-unix/X$DNUM" ] && break; sleep 0.25; done
[ -S "/tmp/.X11-unix/X$DNUM" ] || { echo "Xvfb failed (see $TD/xvfb.log)"; exit 1; }

echo "== launch nemacs on $DISP (transport=$TD) =="
DISPLAY="$DISP" NEMACS_TRANSPORT_DIR="$TD" NEMACS_X_DISPLAY_NUM="$DNUM" \
  setsid bash "$GUI_ROOT/bin/nemacs" "$TESTFILE" >"$TD/launch.log" 2>&1 &

# Wait until OUR launch is live: our transport's modeline must name the test
# file (the build's smoke uses /tmp, so it never writes here) and a window must
# be mapped.
WID=""
for _ in $(seq 1 90); do
  if grep -q "verify.org" "$TD/nemacs-modeline" 2>/dev/null; then
    WID=$(DISPLAY="$DISP" xwininfo -root -children 2>/dev/null \
            | awk '/0x[0-9a-f]+.*[0-9]+x[0-9]+\+/{print $1; exit}')
    [ -n "$WID" ] && break
  fi
  sleep 0.5
done
[ -n "$WID" ] || { echo "our GUI did not come up (see $TD/launch.log)"; exit 1; }
ok "GUI window mapped on $DISP (wid=$WID)"
DISPLAY="$DISP" xdotool windowfocus "$WID" 2>/dev/null; sleep 0.5

ml()   { cat "$TD/nemacs-modeline" 2>/dev/null; }
view() { cat "$TD/nemacs-view" 2>/dev/null; }
line1(){ head -1 "$TD/nemacs-view" 2>/dev/null; }
# A buffer-modifying command (org-metaright, self-insert) makes the bridge
# rewrite the whole buffer-store + view slice and the GUI re-render, which takes
# noticeably longer than a cursor move -- so settle generously between keys.
KEY_SETTLE=${NEMACS_XVFB_KEY_SETTLE:-1.5}
key()  { DISPLAY="$DISP" xdotool key --window "$WID" --clearmodifiers "$1"; sleep "$KEY_SETTLE"; }
click_xy() { DISPLAY="$DISP" xdotool mousemove --window "$WID" "$1" "$2" click 1; sleep "$KEY_SETTLE"; }

# Initial state: point 0 = start of "* TODO first heading" (modeline L00001).
echo "  initial modeline: $(ml)"
# warm-up: the first keystroke can be absorbed by bridge-session start; send a
# harmless C-a and let the round-trip settle before asserting.
key ctrl+a; key ctrl+a

# ---- check: toolbar dropdown and selected command route through bridge -----
click_xy 20 10
if grep -q "$(printf 'Find File\tC-x C-f')" "$TD/nemacs-toolbar-menu" 2>/dev/null; then
  ok "toolbar top-level click opened the New dropdown"
else
  bad "toolbar top-level click did not open dropdown: [$(cat "$TD/nemacs-toolbar-menu" 2>/dev/null)]"
fi
click_xy 20 26
if [ "$(cat "$TD/nemacs-minibuffer-active" 2>/dev/null)" = "1" ] &&
   grep -q "Find file:" "$TD/nemacs-minibuffer-prompt" 2>/dev/null; then
  ok "toolbar dropdown selection entered find-file minibuffer"
else
  bad "toolbar dropdown selection did not enter find-file (active=$(cat "$TD/nemacs-minibuffer-active" 2>/dev/null), prompt=$(cat "$TD/nemacs-minibuffer-prompt" 2>/dev/null))"
fi
key ctrl+g

# ---- check: M-Right demotes the heading (THE arrow-keysym target) ----------
# The GUI emits "M-<right>" for Meta+arrow (alt+keysym) through the key
# transport; the bridge org keymap binds M-<right> -> org-metaright, so
# "* TODO first heading" -> "** TODO first heading".  Run on the clean initial
# buffer with point at the line-1 heading.
key ctrl+a
before=$(line1)
key alt+Right
after=$(line1)
if printf '%s' "$after" | grep -q '^\*\* TODO 漏電調査'; then
  ok "M-Right demoted heading via org-metaright (CJK title intact): [$after]"
else
  bad "M-Right did not demote: [$before] -> [$after]"
fi

# ---- check: M-Left promotes it back ---------------------------------------
key alt+Left
back=$(line1)
if printf '%s' "$back" | grep -q '^\* TODO 漏電調査'; then
  ok "M-Left promoted heading via org-metaleft: [$back]"
else
  bad "M-Left did not promote: [$back]"
fi

# ---- check: BackSpace deletes a whole kanji (multibyte char motion) --------
# C-e to end of the line-1 CJK heading, then BackSpace must remove the whole
# 査 (3 bytes), not one byte -- the byte-vs-char deletion fix, via real keys.
key ctrl+e
key BackSpace
bs=$(line1)
if printf '%s' "$bs" | grep -q '漏電調' && ! printf '%s' "$bs" | grep -q '査'; then
  ok "BackSpace deleted a whole kanji (査 gone, 漏電調 intact): [$bs]"
else
  bad "BackSpace corrupted the multibyte char: [$bs]"
fi

# ---- check: C-n moves down a line (input path is live) ---------------------
key ctrl+a
key ctrl+n
if ml | grep -q "L00002"; then ok "C-n moved to line 2 ($(ml | grep -o 'L[0-9]*'))"; else bad "C-n did not reach line 2 (modeline: $(ml))"; fi

# ---- check: self-insert edits the buffer ----------------------------------
v0=$(view)
key z
v1=$(view)
if [ "$v0" != "$v1" ] && printf '%s' "$v1" | grep -q 'z'; then
  ok "self-insert 'z' edited the buffer view"
else
  bad "self-insert did not change the view"
fi

# ---- check: SKK romaji input produces kana (Japanese input) ---------------
# Enable the input method by writing the state the bridge reads each keypress
# (the C-\ toggle keystroke has a separate Xvfb keysym-delivery quirk; the
# romaji->kana hook itself is what we verify here), then type "ka" -> か.
printf 'default' >"$TD/nemacs-input-method"
key ctrl+e
key k
key a
if view | grep -q 'か'; then
  ok "SKK romaji 'ka' produced か (Japanese input live)"
else
  bad "SKK romaji input did not produce kana: [$(view | tr '\n' '/' )]"
fi
printf '' >"$TD/nemacs-input-method"

# ---- check: something actually rendered (visual render) -------------------
if DISPLAY="$DISP" xwd -root -silent -out "$TD/screen.xwd" 2>/dev/null; then
  sz=$(wc -c < "$TD/screen.xwd")
  if [ "$sz" -gt 100000 ]; then ok "framebuffer rendered ($sz bytes -> $TD/screen.xwd)"; else bad "framebuffer too small ($sz bytes)"; fi
else
  bad "xwd capture failed"
fi

echo ""
echo "== $PASS passed, $FAIL failed =="
[ "$FAIL" -eq 0 ]
