#!/bin/bash
# check-a2.sh BINARY : the A2 loop (with-temp-buffer + setq-local) at N and 4N is linear and bounded
B=$1; HERE=$(cd "$(dirname "$0")" && pwd); F=${TMPDIR:-/tmp}/buflocal1-a2.el
cat > $F <<'EOS'
;;; -*- lexical-binding: t; -*-
(defun a2-run (n) (dotimes (k n) (with-temp-buffer (insert "abc") (setq-local fill-column k))))
(let* ((n (string-to-number (getenv "N"))) (t0 (float-time)))
  (a2-run n)
  (princ (format "%.6f %d %d\n" (- (float-time) t0) (length (buffer-list))
                 (if (boundp 'nelisp-buffer-local--serial) nelisp-buffer-local--serial 0))))
EOS
t1=$( (ulimit -v 8388608; N=2000 timeout 300 "$B" --load $F -- 2>&1 | head -1) )
t4=$( (ulimit -v 8388608; N=8000 timeout 300 "$B" --load $F -- 2>&1 | head -1) )
echo "N=2000: $t1"; echo "N=8000: $t4"
python3 - "$t1" "$t4" <<'EOP'
import sys
a=sys.argv[1].split(); b=sys.argv[2].split()
ta,tb=float(a[0]),float(b[0])
ratio=tb/ta
print("ratio(4N/N)=%.2f live-buffers=%s cells=%s" % (ratio,b[1],b[2]))
# linear: well under 4*1.8; bounded: no buffers left, at most one cell symbol ever created
sys.exit(0 if ratio < 7.2 and int(b[1]) <= 3 and int(b[2]) <= 2 else 1)
EOP
