;;; char-table-bridge-perf-probe.el --- standalone array bridge timings  -*- lexical-binding: t; -*-

(let* ((mode (or (getenv "CHAR_TABLE_PERF_MODE") "base"))
       (rounds 5)
       (iterations 5000)
       (vector (make-vector 32 0))
       (string "あいうえおかきくけこ")
       (table (emacs-char-table-make 'perf 'fallback)))
  (emacs-char-table-set table ?a 'ascii)
  (emacs-char-table-set table #x3042 'unicode)
  (let ((round 0))
    (while (< round rounds)
      (let ((i 0)
            (sum 0)
            (start (float-time)))
        (while (< i iterations)
          (aset vector 17 i)
          (setq sum (+ sum (aref vector 17)))
          (setq i (1+ i)))
        (princ (format "PERF mode=%s case=vector round=%d iterations=%d index=17 checksum=%d seconds=%.6f\n"
                       mode round iterations sum (- (float-time) start))))
      (let ((i 0)
            (sum 0)
            (start (float-time)))
        (while (< i iterations)
          (setq sum (+ sum (aref string 4)))
          (setq i (1+ i)))
        (princ (format "PERF mode=%s case=multibyte-string round=%d iterations=%d chars=%d index=4 checksum=%d seconds=%.6f\n"
                       mode round iterations (length string) sum
                       (- (float-time) start))))
      (let ((i 0)
            (hits 0)
            (start (float-time)))
        (while (< i iterations)
          (when (if (equal mode "bridge")
                    (aref table #x3042)
                  (emacs-char-table-ref table #x3042))
            (setq hits (1+ hits)))
          (setq i (1+ i)))
        (princ (format "PERF mode=%s case=char-table-ref round=%d iterations=%d index=%d hits=%d seconds=%.6f\n"
                       mode round iterations #x3042 hits
                       (- (float-time) start))))
      (setq round (1+ round)))))

;;; char-table-bridge-perf-probe.el ends here
