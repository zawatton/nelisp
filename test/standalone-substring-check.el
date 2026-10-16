;;; standalone-substring-check.el --- `substring' as in GNU, at O(slice) cost -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-substring-check.el
;;        emacs -Q --batch -l test/standalone-substring-check.el
;;
;; The native `substring' arm used to walk the whole source on every call:
;; `m5_observably_unibyte_p' scanned every byte (twice) to decide the
;; result's multibyteness, and an all-ASCII string still converted char
;; indices to byte offsets by walking.  A 512-byte slice of a 246 KB source
;; cost 17 ms, and `load' slices each file that way for the reader.
;;
;; Part 1 checks values, multibyteness and errors against GNU Emacs 31.1
;; (the same file passes there): sources below and above the 64-byte limit
;; under which the char count is not cached, ASCII, multibyte, forced
;; multibyte and raw unibyte sources, negative and omitted indices.
;; Part 2 times a 512-byte slice of 256 KB sources (`mapconcat' and
;; `make-string' results) against the same slice of 4 KB ones, interleaved
;; in one process; the old walk made the ratio about 60.  Prints one SUBSTRING line and exits non-zero on failure.

(defvar substring-check--failures nil)

(defun substring-check--call (s from to)
  (condition-case err
      (let ((r (if to (substring s from to) (substring s from))))
        (list r (multibyte-string-p r)))
    (error (list 'error (car err)))))

(defun substring-check--expect (label got expected)
  (unless (equal got expected)
    (push (list label got expected) substring-check--failures)))

(let* ((ascii-short "abcdefghij")
       (ascii-long (concat (make-string 70 ?x) "0123456789"))
       (multi-long (concat (make-string 70 ?x) "éあyz" (make-string 10 ?q)))
       (forced (string-to-multibyte (make-string 80 ?m)))
       (raw (unibyte-string 97 200 98 255 99)))
  ;; (SOURCE FROM TO EXPECTED) with EXPECTED = (STRING MULTIBYTE-P) or (error SYMBOL).
  (dolist (c (list (list ascii-short 2 5 '("cde" nil))
                   (list ascii-short 0 nil '("abcdefghij" nil))
                   (list ascii-short -3 nil '("hij" nil))
                   (list ascii-short 10 10 '("" nil))
                   (list ascii-short 0 11 '(error args-out-of-range))
                   (list ascii-short 5 4 '(error args-out-of-range))
                   (list ascii-short -11 nil '(error args-out-of-range))
                   (list ascii-long 70 75 '("01234" nil))
                   (list ascii-long 75 nil '("56789" nil))
                   (list ascii-long -4 -1 '("678" nil))
                   (list ascii-long 80 80 '("" nil))
                   (list ascii-long 79 81 '(error args-out-of-range))
                   (list ascii-long 81 nil '(error args-out-of-range))
                   (list multi-long 68 72 '("xxéあ" t))
                   (list multi-long 0 3 '("xxx" t))
                   (list multi-long 72 76 '("yzqq" t))
                   (list multi-long -2 nil '("qq" t))
                   (list multi-long 84 85 '(error args-out-of-range))
                   (list forced 3 6 '("mmm" t))
                   (list forced -2 nil '("mm" t))
                   (list raw 1 4 (list (unibyte-string 200 98 255) nil))
                   (list raw -1 nil '("c" nil))))
    (substring-check--expect (list 'substring (length (nth 0 c)) (nth 1 c) (nth 2 c))
                             (substring-check--call (nth 0 c) (nth 1 c) (nth 2 c))
                             (nth 3 c))))

;; The same 512-byte window cut from a 256 KB and from a 4 KB ASCII source.
(let* ((numbers (lambda (n)
                  (let ((parts nil) (i 0))
                    (while (< i n) (push (format "%05d" i) parts) (setq i (1+ i)))
                    ;; `mapconcat' and `make-string' build the general string
                    ;; that walked; `concat' of ASCII parts builds a unibyte
                    ;; one, which never did.
                    (mapconcat #'identity (nreverse parts) ""))))
       (big (funcall numbers 52429))
       (small (funcall numbers 820))
       (window (lambda (s) (substring s 1000 1512)))
       (expected (let ((parts nil) (i 200))
                   (while (< i 303) (push (format "%05d" i) parts) (setq i (1+ i)))
                   (substring (apply #'concat (nreverse parts)) 0 512)))
       (time-of (lambda (s)
                  (let ((start (float-time)) (i 0))
                    (while (< i 20) (funcall window s) (setq i (1+ i)))
                    (- (float-time) start))))
       (tb 0.0) (ts 0.0) (round 0))
  (substring-check--expect 'window-content (funcall window big) expected)
  (substring-check--expect 'window-content-small (funcall window small) expected)
  ;; Warm the char-count cache, then interleave so load affects both alike.
  (funcall window big)
  (let ((big-x (make-string 262144 ?x)) (small-x (make-string 4096 ?x)))
    (funcall window big-x)
    (while (< round 5)
      (setq tb (+ tb (funcall time-of big) (funcall time-of big-x))
            ts (+ ts (funcall time-of small) (funcall time-of small-x))
            round (1+ round))))
  (let ((ratio (/ tb (max ts 1e-6))))
    (princ (format "SUBSTRING timing big=%.3f ms small=%.3f ms ratio=%.2f\n"
                   (/ (* 1000 tb) 200) (/ (* 1000 ts) 200) ratio))
    (when (> ratio 4.0)
      (push (list 'cost-depends-on-source-length ratio) substring-check--failures))))

(dolist (f (reverse substring-check--failures))
  (princ (format "  MISMATCH %S\n" f)))
(princ (format "SUBSTRING failures=%d %s\n" (length substring-check--failures)
               (if substring-check--failures "FAIL" "PASS")))
(kill-emacs (if substring-check--failures 1 0))

;;; standalone-substring-check.el ends here
