;;; standalone-skk-shaped-bench.el --- SKK-shaped string merge workload -*- lexical-binding: t -*-

;; Usage: SKK_BENCH_LIMIT=RATIO nelisp --load test/standalone-skk-shaped-bench.el
;;
;; Four string merge functions shaped like the Windows SKK IME's
;; dictionary merge (split candidate lists, strip annotations, drop
;; duplicates, test okurigana prefixes, rebuild the entry) run over 80
;; records.  The time is divided by a native SHA-256 of 8 MiB timed in
;; the same run, so the ratio holds across machines and load.
;; Prints one SKK-BENCH line; with SKK_BENCH_LIMIT set, exits non-zero
;; when the ratio exceeds it.

(defun skk-bench--candidates (entry)
  "Split ENTRY \"/c1/c2;note/\" into candidate strings."
  (split-string entry "/" t))

(defun skk-bench--strip (cand)
  "Drop CAND's \";annotation\" suffix."
  (let ((pos (string-search ";" cand)))
    (if pos (substring cand 0 pos) cand)))

(defun skk-bench--merge (left right)
  "Merge two candidate entries, keeping LEFT's order, without duplicates."
  (let ((out nil))
    (dolist (c (append (skk-bench--candidates left)
                       (skk-bench--candidates right)))
      (let ((bare (skk-bench--strip c)))
        (unless (member bare out)
          (setq out (cons bare out)))))
    (concat "/" (mapconcat #'identity (nreverse out) "/") "/")))

(defun skk-bench--okuri-match (key okuri)
  "Non-nil when KEY's reading ends in the okurigana prefix OKURI."
  (and (string-prefix-p (substring okuri 0 1) (substring key -1))
       (string= (substring key -1) (substring okuri 0 1))))

(defun skk-bench--records ()
  (let ((recs nil) (i 0))
    (while (< i 80)
      (setq recs
            (cons (list (format "%dかんじ" i)
                        (format "/漢字%d/感じ;feeling/幹事%d/" i i)
                        (format "/感じ;feeling/監事%d/漢字%d/" i i)
                        "じ")
                  recs))
      (setq i (1+ i)))
    recs))

(defun skk-bench--run (recs)
  (let ((n 0))
    (dolist (r recs)
      (let ((merged (skk-bench--merge (nth 1 r) (nth 2 r))))
        (when (skk-bench--okuri-match (nth 0 r) (nth 3 r))
          (setq n (+ n (length merged))))))
    n))

(defvar skk-bench--reference-input (make-string (* 8 1024 1024) ?x))

(defun skk-bench--reference ()
  ;; One native call over 8 MiB: it tracks machine speed and load but not
  ;; the interpreter, so interpreter-wide speedups show in the ratio.
  (nelisp--sha256 skk-bench--reference-input))

(defun skk-bench--time (fn &rest args)
  (garbage-collect)
  (let ((t0 (float-time)))
    (apply fn args)
    (- (float-time) t0)))

(let* ((recs (skk-bench--records))
       (check (skk-bench--run recs))
       (work (min (skk-bench--time #'skk-bench--run recs)
                  (skk-bench--time #'skk-bench--run recs)))
       (ref (min (skk-bench--time #'skk-bench--reference)
                 (skk-bench--time #'skk-bench--reference)))
       (ratio (/ work ref))
       (limit (and (getenv "SKK_BENCH_LIMIT")
                   (string-to-number (getenv "SKK_BENCH_LIMIT"))))
       (ok nil))
  (setq ok (and (> check 0) (or (null limit) (<= ratio limit))))
  (princ (format "SKK-BENCH work=%.3fs ref=%.3fs ratio=%.2f check=%d limit=%s %s\n"
                 work ref ratio check limit (if ok "PASS" "FAIL")))
  (kill-emacs (if ok 0 1)))

;;; standalone-skk-shaped-bench.el ends here
