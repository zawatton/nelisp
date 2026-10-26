#!/bin/sh
# crash1: peak RSS and wall time of the large cases.  usage: rss.sh BINARY [LABEL]
# Writes target/crash1/rss-LABEL.txt; exits 1 if any case failed (rc != 0 or wrong output).
BIN=$1; L=${2:-nl}; W=${CRASH1_WORK:-target/crash1/w}; DATA=${CRASH1_DATA:-$W/data}
OUT=target/crash1/rss-$L.txt; : > "$OUT"; bad=0
row() { # NAME EXPECT EXPR
  r=$(python3 -I tools/ai/crash1/rss.py "$BIN" "$3" 600)
  echo "$1 $r" >> "$OUT"
  case $r in *"out='$2'"*"rc=0 "*) ;; *) bad=1 ;; esac
}
mk='(let ((x 12345) (acc nil)) (dotimes (_ N) (setq x (% (+ (* x 1103515245) 12345) 2147483648)) (push (% (/ x 16) 1000000) acc)) acc)'
for n in 560000 2000000; do
  e=$(echo "$mk" | sed "s/N/$n/")
  row "sort-list-$n" "$n" "(princ (let ((r (sort $e (function <)))) (length r)))"
  row "sort-vec-$n"  "$n" "(princ (let ((r (sort (vconcat $e) (function >)))) (length r)))"
done
row "string-replace-2M-matches" "4000000" '(princ (length (string-replace "a" "bb" (make-string 2000000 ?a))))'
row "ifc-50MB" "50000000" "(princ (with-temp-buffer (insert-file-contents \"$DATA/ascii-50000000.txt\") (buffer-size)))"
row "callproc-50MB" "50000000" "(princ (with-temp-buffer (call-process \"cat\" nil t nil \"$DATA/ascii-50000000.txt\") (buffer-size)))"
row "literal-bin-20MB" "20000000" "(princ (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally \"$DATA/bin-20000000.bin\") (buffer-size)))"
cat "$OUT"; exit $bad
