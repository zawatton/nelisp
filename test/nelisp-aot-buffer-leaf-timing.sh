#!/bin/bash
# Timing of the step D lowering (NOT RUN until the batch build).
# Native loops of the gg-driver point / goto-char / char-after / trivial-builtin
# rows, compiled with the flag off / leaf (bridged) / inline (point, point-min, point-max read inline), each timed in its own cold process of
# a binary built with nl_native_buf_leaf.  Usage:
#   NELISP_AOT_BUFLEAF_HOME=<checkout with target/nelisp + build/nemacs-runtime.nlri> \
#   CPUS=9,25 test/nelisp-aot-buffer-leaf-timing.sh [OUTDIR]
# Never pin to 11,13,14,15 (benchmark CPUs) unless the coordinator says so.
set -u
ROOT=$(cd "$(dirname "$0")/.." && pwd)
HOME_NL=${NELISP_AOT_BUFLEAF_HOME:?set NELISP_AOT_BUFLEAF_HOME}
CPUS=${CPUS:-9,25}
OUT=${1:-$(mktemp -d)}
mkdir -p "$OUT"
cat > "$OUT/rows.el" <<'EOR'
(defun blt-point (n) (let ((i 0) (s 0)) (while (< i n) (setq s (+ s (point))) (setq i (1+ i))) s))
(defun blt-goto-char (n) (let ((i 0) (s 0)) (while (< i n) (goto-char 7) (setq s (+ s 1)) (setq i (1+ i))) s))
(defun blt-char-after (n) (let ((i 0) (s 0)) (while (< i n) (setq s (+ s (char-after 3))) (setq i (1+ i))) s))
(defun blt-trivial (n) (let ((i 0) (s 0)) (while (< i n) (setq s (+ s (1+ i))) (setq i (1+ i))) s))
EOR
for flag in off leaf inline; do
  F=nil; I=nil; [ $flag = leaf ] && F=t; [ $flag = inline ] && I=t
  ( ulimit -v 8388608; timeout 600 emacs -Q --batch -L "$ROOT/lisp" -L "$ROOT/src" \
      --eval "(progn (require 'nelisp-artifact) (require 'nelisp-aot-compiler) (require 'nelisp-native-load)
        (let ((nelisp-aot-compiler--dynamic-user-calls t) (nelisp-aot-compiler-buffer-leaves $F) (nelisp-aot-compiler-buffer-leaves-inline $I))
          (nelisp-artifact-compile-file \"$OUT/rows.el\" \"$OUT/$flag.neln\" nil nil nil nil nil 'neln 'opportunistic)
          (with-temp-file \"$OUT/settings-$flag.el\" (prin1 (nelisp-native-load-codegen-settings) (current-buffer)))))" \
      </dev/null > "$OUT/compile-$flag.log" 2>&1 )
  cat > "$OUT/run-$flag.el" <<EOR
(setq load-path (append (list "$ROOT/lisp" "$ROOT/src") load-path))
(require 'nelisp-native-load)
;; the artifact records the flags it was built with; the image loader (no compiler) must expect them
(setq nelisp-native-load-expected-codegen-settings (car (read-from-string (nelisp--syscall-read-file "$OUT/settings-$flag.el"))))
(defun blt-time (name n)
  (let* ((h (nelisp-native-load-artifact "$OUT/$flag.neln" name)))
    (with-temp-buffer
      (insert "hello world, a text for the leaves\n") (goto-char 5)
      (nelisp-native-load-call h (list 1000))
      (let* ((t0 (float-time)) (r (nelisp-native-load-call h (list n))))
        (princ (format "TIME $flag %s %d %.6f %S\n" name n (- (float-time) t0) r))))))
(dolist (r '("blt-point" "blt-goto-char" "blt-char-after" "blt-trivial"))
  (blt-time r 1000000) (blt-time r 4000000))
(princ "TIME-DONE\n")
EOR
  # cold process per flag; the paired order is on then off, repeat by calling the script again
  ( cd "$HOME_NL" && ulimit -v 8388608 && timeout 900 taskset -c "$CPUS" target/nelisp exec-runtime-image build/nemacs-runtime.nlri \
      "(load \"$OUT/run-$flag.el\" nil t t)" </dev/null > "$OUT/time-$flag.log" 2>&1 )
done
grep -h "^TIME " "$OUT"/time-off.log "$OUT"/time-leaf.log "$OUT"/time-inline.log
echo "ns/iter = (t4M - t1M)/3M; results (last field) must be equal between on and off"
echo "outdir: $OUT"
