;;; standalone-split-string-parity.el --- split-string fast path parity -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-split-string-parity.el
;;
;; Compares the runtime's `nelisp--split-on-char' with the plain
;; character walk it replaced, over separator edge cases, Japanese,
;; emoji and raw bytes, then times a 32K-character split.  Prints one
;; SPLIT-PARITY line and exits non-zero on any difference or when the
;; split exceeds its time bound.

(defun split-parity--oracle (string char omit-empty)
  (let ((start 0) (idx 0) (len (length string)) (parts nil))
    (while (<= idx len)
      (if (or (= idx len) (= (aref string idx) char))
          (let ((part (substring string start idx)))
            (unless (and omit-empty (= (length part) 0))
              (setq parts (cons part parts)))
            (setq start (1+ idx))))
      (setq idx (1+ idx)))
    (nreverse parts)))

(let ((cases (list "" "/" "//" "a" "/a" "a/" "a//b" "/a/b/" "abc/def/ghi"
                   "あ/い" "/あい/" "x/あ//う/" "日本語" "a b  c" " "
                   (string 233 47 97) (concat "a/" (string 4194200) "/b")
                   "漢字/かな/ｶﾅ/😀/end"))
      (n 0) (bad 0))
  (dolist (s cases)
    (dolist (c '(?/ ?\s ?a ?あ))
      (dolist (o '(nil t))
        (setq n (1+ n))
        (let ((want (split-parity--oracle s c o))
              (got (nelisp--split-on-char s c o)))
          (unless (and (equal want got)
                       (equal (mapcar #'multibyte-string-p want)
                              (mapcar #'multibyte-string-p got)))
            (setq bad (1+ bad))
            (princ (format "DIFF %S %c %S: %S vs %S\n" s c o want got)))))))
  (let* ((s (make-string 32768 ?x))
         ;; Collect the parity cases' garbage first so a GC does not land
         ;; inside the timed split.
         (_ (garbage-collect))
         (t0 (float-time))
         (parts (split-string s "/" t))
         (secs (- (float-time) t0))
         (ok (and (= bad 0) (equal parts (list s)) (< secs 0.05))))
    (princ (format "SPLIT-PARITY cases=%d diffs=%d split32k=%.4fs %s\n"
                   n bad secs (if ok "PASS" "FAIL")))
    (kill-emacs (if ok 0 1))))

;;; standalone-split-string-parity.el ends here
