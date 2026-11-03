#!/bin/sh
# Compare one timer-shape probe between stock Emacs and a standalone binary.
# Usage: timer-shape-check.sh ITEM [NELISP_BIN]
# ITEM is one of: shape cancel timerp-vectors run-with-timer deferred
# Time slots are never printed: they differ between runs.
item=$1
bin=${2:-${NELISP_BIN:-./target/nelisp}}
emacs=${EMACS:-emacs}
case "$item" in
  shape) form='(let ((tm (run-at-time 100 nil (function ignore) 1 2))) (list (type-of tm) (length tm) (timerp tm) (timer--function tm) (timer--args tm) (timer--repeat-delay tm)))' ;;
  cancel) form='(let ((tm (run-at-time 100 nil (function ignore)))) (list (cancel-timer tm) (condition-case e (cancel-timer "x") (error e)) (condition-case e (cancel-timer nil) (error e))))' ;;
  timerp-vectors) form='(list (timerp (make-vector 10 nil)) (timerp [1 2]) (timerp (make-vector 9 nil)) (timerp (make-vector 11 nil)) (timerp nil) (timerp "x") (timerp (list 1 2)))' ;;
  run-with-timer) form='(let ((tm (run-with-timer 100 nil (function ignore) 3))) (list (fboundp (quote run-with-timer)) (timerp tm) (length tm) (timer--function tm) (timer--args tm)))' ;;
  deferred) form='(let ((hit nil)) (run-at-time 0 nil (lambda (a b) (setq hit (list a b))) 1 2) (list hit (progn (sit-for 0.05) hit)))' ;;
  *) echo "unknown item: $item" >&2; exit 2 ;;
esac
case $bin in /*) ;; *) bin=$PWD/$bin ;; esac  # the probe runs after a cd
d=$(mktemp -d) || exit 2
trap 'rm -rf "$d"' EXIT
printf ';;; -*- lexical-binding: t; -*-\n(princ (format "%%S\\n" (condition-case err %s (error (list (quote ERR) err)))))\n' "$form" > "$d/p.el"
(cd "$d" && "$emacs" -Q --batch -l p.el) > "$d/gnu.txt" 2>/dev/null
(cd "$d" && timeout 120 "$bin" --load p.el) > "$d/nl.txt" 2>/dev/null
[ -s "$d/gnu.txt" ] || { echo "$item: Emacs produced no output" >&2; exit 2; }
# --load also echoes the file's last value on a second line; compare the
# printed answer only.
head -n 1 "$d/gnu.txt" > "$d/gnu1.txt"; head -n 1 "$d/nl.txt" > "$d/nl1.txt"
if cmp -s "$d/gnu1.txt" "$d/nl1.txt"; then echo "$item: PASS"; exit 0; fi
echo "$item: DIFF"; echo " gnu: $(cat "$d/gnu1.txt")"; echo " nl:  $(head -c 600 "$d/nl1.txt")"; exit 1
