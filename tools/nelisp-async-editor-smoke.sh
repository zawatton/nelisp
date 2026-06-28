#!/usr/bin/env bash
# nelisp-async-editor-smoke.sh -- the async editor edits a nelisp-buffer from
# real stdin keystrokes, driven entirely by nelisp-async-run-tty.
#
# Feeds a key script (printable + control keys) and asserts the resulting
# buffer text / point.  Pure-pipe input is deterministic; interactive raw-mode
# use and timer multiplexing are covered elsewhere.
#
# Usage: tools/nelisp-async-editor-smoke.sh [path-to-nelisp]
set -euo pipefail
cd "$(dirname "$0")/.."

NELISP="${1:-./target/nelisp}"
GEN="${NELISP_GENERATOR:-../nemacs-next/vendor/emacs-lisp/emacs-lisp/generator.el}"

if [ ! -x "$NELISP" ]; then echo "SKIP: $NELISP not built"; exit 0; fi
if [ ! -f "$GEN" ]; then echo "SKIP: generator.el not found at $GEN"; exit 0; fi

LOADS="(load \"scripts/nelisp-stdlib-prelude.el\") \
(load \"src/nelisp-gc.el\") (load \"src/nelisp-buffer.el\") (load \"$GEN\") \
(load \"packages/nelisp-actor/src/nelisp-actor.el\") \
(load \"packages/nelisp-eventloop/src/nelisp-eventloop.el\") \
(load \"packages/nelisp-eventloop/src/nelisp-async.el\") \
(load \"packages/nelisp-redisplay/src/nelisp-redisplay.el\") \
(load \"examples/nelisp-async-editor.el\")"

run_case () { # $1 = key bytes (printf escapes), $2 = expected RESULT substring
  local keys="$1" want="$2"
  local out
  out="$(printf "$keys" | "$NELISP" --eval "(progn $LOADS (princ (nae-run-batch)))" 2>&1 | tail -1)"
  echo "[async-editor-smoke] keys=$(printf '%q' "$keys") -> $out"
  case "$out" in
    *"$want"*) ;;
    *) echo "FAIL: expected substring '$want'"; exit 1 ;;
  esac
}

# 'a' 'b' 'c'  C-b C-b  'Z'  C-q  -> "aZbc", point 3
run_case 'abc\002\002Z\021' 'text=aZbc point=3'
# 'h' 'i'  C-f(no-op at end)  DEL  'X'  C-q -> "hX", point 3
run_case 'hi\006\177X\021'   'text=hX point=3'
# plain typing + quit
run_case 'hello\021'         'text=hello point=6'
# Enter -> newline, C-a -> line start, insert -> "ab\nXcd"
run_case 'ab\rcd\001X\021'   'text=ab\nXcd point=5'
# Left-arrow (ESC [ D) x2 then 'Z' -> "aZbc"
run_case 'abc\033[D\033[DZ\021' 'text=aZbc point=3'
# Up-arrow (ESC [ A) from line 2 start to line 1, insert 'Q' -> "Qabc\ndef"
run_case 'abc\rdef\001\033[AQ\021' 'text=Qabc\ndef point=2'
# C-b then C-d (delete forward) on "hi" -> "h"
run_case 'hi\002\004\021'    'text=h point=2'
# C-a, C-f x2, C-k (kill "cde"), C-a, C-y (yank) -> "cdeab"
run_case 'abcde\001\006\006\013\001\031\021' 'text=cdeab point=4'
# C-a (line 2), C-b (onto the newline), C-k -> join lines "abcd"
run_case 'ab\rcd\001\002\013\021' 'text=abcd point=3'

# --- File load + edit + C-x C-s save roundtrip ---
# Load "hello\nworld", insert '!' at start (point is at buffer start after load),
# C-x C-s to save, C-q to quit; then verify the bytes on disk.
TMP="$(mktemp)"; trap 'rm -f "$TMP"' EXIT
printf 'hello\nworld' > "$TMP"
out="$(printf '!\030\023\021' | "$NELISP" --eval "(progn $LOADS (princ (nae-run-batch \"$TMP\")))" 2>&1)"
echo "[async-editor-smoke] file-roundtrip msg: $(printf '%s' "$out" | grep -o 'msg=wrote [0-9]*B' || true)"
disk="$(cat "$TMP")"
echo "[async-editor-smoke] on disk: $(printf '%q' "$disk")"
case "$out" in *"msg=wrote 12B"*) ;; *) echo "FAIL: save did not report writing 12 bytes"; exit 1 ;; esac
if [ "$disk" != "$(printf '!hello\nworld')" ]; then echo "FAIL: file content on disk is wrong"; exit 1; fi

# --- Redisplay render check: capture the frame for a 2-line buffer ---
rout="$("$NELISP" --eval "(progn $LOADS
  (nae--setup nil) (setq nae--draw t)
  (nelisp-insert \"abc\ndef\" nae--buf) (nelisp-goto-char 6 nae--buf)
  (let ((cap \"\"))
    (setq nelisp-redisplay--output-fn (lambda (s) (setq cap (concat cap s))))
    (nae--render)
    (princ (format \"RENDER[abc=%s def=%s status=%s cur=%s]\"
                   (and (string-match-p \"abc\" cap) t)
                   (and (string-match-p \"def\" cap) t)
                   (and (string-match-p \"async-editor\" cap) t)
                   (and (string-match-p \"\e\\\[2;2H\" cap) t)))))" 2>&1 | tail -1)"
echo "[async-editor-smoke] $rout"
case "$rout" in
  *"RENDER[abc=t def=t status=t cur=t]"*) ;;
  *) echo "FAIL: redisplay frame missing content/cursor"; exit 1 ;;
esac

echo "PASS"
