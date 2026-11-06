;;; large-buffer-regex-bench.el --- large-buffer regexp/search benchmarks  -*- lexical-binding: nil; -*-
;; Runs on the NeLisp standalone binary and on host `emacs --batch'.
;; Env: NELISP_ORG_EL (path of a large org.el), BENCH (A1|A2|A3|A4|A8).
;; Prints one line:  BENCH <id> elapsed=<seconds> value=<printed value>

(defvar lbr-org-file (getenv "NELISP_ORG_EL"))
(defvar lbr-which (getenv "BENCH"))

(defun lbr-org-text ()
  (with-temp-buffer
    (insert-file-contents lbr-org-file)
    (buffer-string)))

(defun lbr-report (id t0 value)
  (princ (format "BENCH %s elapsed=%.3f value=%S\n" id (- (float-time) t0) value)))

(cond
 ((equal lbr-which "A1")
  (let ((s (substring (lbr-org-text) 0 160000)) (t0 (float-time)))
    (lbr-report "A1" t0 (string-match "^(zzqq " s))))
 ((equal lbr-which "A2")
  (let ((s (substring (lbr-org-text) 0 160000)) (t0 (float-time)))
    (lbr-report "A2" t0 (string-search "zzqqxx-nomatch" s))))
 ((equal lbr-which "A3")
  (with-temp-buffer
    (insert (lbr-org-text))
    (goto-char (point-min))
    (let ((t0 (float-time)))
      (lbr-report "A3" t0 (re-search-forward "^(defun org-agenda-files " nil t)))))
 ((equal lbr-which "A4")
  (with-temp-buffer
    (insert (lbr-org-text))
    (goto-char (point-min))
    (let ((t0 (float-time)) (n 0))
      (while (re-search-forward "^(defun " nil t) (setq n (1+ n)))
      (lbr-report "A4" t0 n))))
 ((equal lbr-which "A8")
  ;; Interpreted string-match costs 2-6 ms/call on the standalone binary, so
  ;; the default trip count is 5000 pairs; LBR_A8_N overrides it.  The ratio
  ;; criterion does not depend on the count.
  (let ((s "The quick brown fox jumps over the lazy dog 0123456789 and more")
        (n (string-to-number (or (getenv "LBR_A8_N") "5000")))
        (i 0) (hits 0) (t0 (float-time)))
    (while (< i n)
      (when (string-match "fox" s) (setq hits (1+ hits)))
      (when (string-match "l.zy" s) (setq hits (1+ hits)))
      (setq i (1+ i)))
    (lbr-report "A8" t0 hits)))
 (t (princ "BENCH usage: BENCH=A1|A2|A3|A4|A8 NELISP_ORG_EL=path\n")))
