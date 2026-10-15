;;; standalone-string-search-parity.el --- string-search fast path parity -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-string-search-parity.el
;;
;; Compares `string-search' with the plain character walk over ASCII,
;; Japanese, emoji, raw bytes and unibyte/multibyte pairings at every
;; valid start position, then checks that a match at the end of a
;; 4000-character multibyte haystack costs under a tenth of the walk.
;; Prints one SEARCH-PARITY line and exits non-zero on any difference or
;; when the search is not that much faster than the walk.

(defun search-parity--oracle (needle haystack start)
  (let ((nl (length needle)) (hl (length haystack)) (i start) (found nil))
    (while (and (not found) (<= (+ i nl) hl))
      (if (string= needle (substring haystack i (+ i nl)))
          (setq found i)
        (setq i (1+ i))))
    found))

(let ((haystacks (list "" "abc" "a;b;c" "感じ;feeling" "漢字12" "x感じ感じy"
                       "😀;😀" "かな;カナ;ｶﾅ" (string 200 59 200)
                       (string 4194200 59 97) (string-to-multibyte "abc;def")
                       (string-to-unibyte "abc;def")))
      (needles (list ";" "感じ" "" "c" "じ;f" "😀" (string 200) "ｶﾅ"
                     (string 4194200) (string-to-multibyte ";")
                     (string-to-unibyte ";") "zz"))
      (n 0) (bad 0))
  (dolist (h haystacks)
    (dolist (s needles)
      (let ((start 0))
        (while (<= start (length h))
          (setq n (1+ n))
          (let ((want (search-parity--oracle s h start))
                (got (string-search s h start)))
            (unless (equal want got)
              (setq bad (1+ bad))
              (princ (format "DIFF %S in %S from %d: %S vs %S\n" s h start want got))))
          (setq start (1+ start))))))
  (let* ((h (concat (make-string 4000 ?あ) ";x"))
         (_ (garbage-collect))
         (t0 (float-time))
         (walk (search-parity--oracle ";" h 0))
         (t1 (float-time))
         (got (string-search ";" h))
         (t2 (float-time))
         (ratio (/ (- t2 t1) (max 1e-6 (- t1 t0))))
         (ok (and (= bad 0) (eql walk 4000) (eql got 4000) (< ratio 0.1))))
    (princ (format "SEARCH-PARITY cases=%d diffs=%d walk=%.4fs search=%.4fs ratio=%.3f %s\n"
                   n bad (- t1 t0) (- t2 t1) ratio (if ok "PASS" "FAIL")))
    (kill-emacs (if ok 0 1))))

;;; standalone-string-search-parity.el ends here
