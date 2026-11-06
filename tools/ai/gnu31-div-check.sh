#!/bin/sh
# Compare one GNU Emacs 31.1 divergence probe between stock Emacs and a
# standalone binary.  Usage: gnu31-div-check.sh ITEM [NELISP_BIN]
# ITEM is one of: format-hash-B sort-one-arg-copy sort-keywords signal-invalid
#   error-symbols-registered
item=$1
bin=${2:-${NELISP_BIN:-./target/nelisp}}
emacs=${EMACS:-emacs}
case "$item" in
  format-hash-B) form='(list (format "%#B" 5) (format "%#b" 5) (format "%B" 5) (format "%#X" 255) (format "%#B" -5) (format "%#8B" 5) (format "%#-8B|" 5) (format "%#08B" 5))' ;;
  sort-one-arg-copy) form='(let* ((v (vector 3 1 2)) (l (list 3 1 2)) (s (list "b" "a")) (rv (sort v)) (rl (sort l)) (rs (sort s))) (list v rv (eq v rv) l rl (eq l rl) s rs))' ;;
  sort-keywords) form='(list (let ((l (list 3 1 2))) (let ((r (sort l :in-place t))) (list r (eq r l)))) (sort (list "b" "A" "c") :key (function downcase)) (sort (list 1 3 2) :reverse t) (sort (list 1 3 2) :lessp (function >)) (let ((v (vector 3 1 2))) (list (sort v (function <)) v)) (let ((v (vector 3 1 2))) (list (sort v :in-place t) v)) (sort (list (cons 2 (quote a)) (cons 1 (quote b))) :key (function car)) (let ((l (list 1 2))) (list (sort l) (eq (sort l) l))) (condition-case e (sort (list 1 2) :bogus 1) (error (car e))))' ;;
  signal-invalid) form='(list (condition-case e (signal (quote undefined-error-symbol) nil) (error e)) (condition-case e (signal (quote undefined-error-symbol) (list 1 2)) (error e)) (condition-case e (signal (quote undefined-error-symbol) nil) (undefined-error-symbol (quote own)) (error (car e))) (condition-case e (signal (quote arith-error) nil) (error e)) (condition-case e (signal (quote wrong-type-argument) (list (quote stringp) 1)) (wrong-type-argument e)) (condition-case e (signal 42 nil) (error (car e))))' ;;
  error-symbols-registered) form='(list (delq nil (mapcar (lambda (s) (and (null (get s (quote error-conditions))) s)) (quote (invalid-read-syntax end-of-buffer beginning-of-buffer buffer-read-only text-read-only json-parse-error cyclic-function-indirection excessive-lisp-nesting file-date-error user-search-failed mark-inactive)))) (get (quote end-of-buffer) (quote error-conditions)) (get (quote invalid-read-syntax) (quote error-message)) (condition-case e (signal (quote end-of-buffer) nil) (end-of-buffer (quote own))))' ;;
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
