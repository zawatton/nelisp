;;; bytecode-bench.el --- GNU bytecode interpreter speed -*- lexical-binding: t -*-

;; Usage: BENCH=NAME [LIMIT=N] nelisp --load test/perf/bytecode-bench.el
;;
;; Runs GNU 31.1 bytecode from test/bytecode-word-corpus.eld (see
;; test/bytecode-word-corpus-gen.el).  Prints "BYTECODE-BENCH NAME VALUE"
;; with the best of three runs; with LIMIT, exits non-zero when VALUE is
;; above it.  NAME is one of:
;;   loop  -- seconds for (bwc-loop 1000000), a bytecode
;;            (while (< i n) (setq i (1+ i)))
;;   call  -- microseconds per iteration of a defun loop calling the
;;            3-argument bytecode function bwc-add3 (call + loop step)
;;   regex -- microseconds per (string-match "\\`\\(.*\\)\\(\\.el\\)\\'"
;;            NAME), the regexp engine being baked GNU bytecode

(let* ((file (expand-file-name "../bytecode-word-corpus.eld"
                               (file-name-directory load-file-name)))
       (data (with-temp-buffer (insert-file-contents file)
                               (car (read-from-string (buffer-string))))))
  (dolist (p (nth 2 data)) (fset (car p) (cdr p))))

(defun bytecode-bench-time (thunk)
  (let ((t0 (float-time))) (funcall thunk) (- (float-time) t0)))
(defun bytecode-bench-best (thunk)
  (let ((best nil) (k 0))
    (while (< k 3)
      (let ((v (funcall thunk))) (when (or (null best) (< v best)) (setq best v)))
      (setq k (1+ k)))
    best))
(defun bytecode-bench-drive (n)
  (let ((i 0) (s 0)) (while (< i n) (setq s (bwc-add3 i 1 2)) (setq i (1+ i))) s))

(let* ((name (intern (or (getenv "BENCH") "")))
       (limit (and (getenv "LIMIT") (string-to-number (getenv "LIMIT"))))
       (names (mapcar (lambda (k) (format "some-library-%d.el" k)) (number-sequence 1 300)))
       (value
        (pcase name
          ('loop (unless (= (bwc-loop 1000) 1000) (error "bwc-loop answers wrong"))
                 (bytecode-bench-best (lambda () (bytecode-bench-time (lambda () (bwc-loop 1000000))))))
          ('call (unless (= (bytecode-bench-drive 10) 12) (error "bwc-add3 answers wrong"))
                 (bytecode-bench-best
                  (lambda () (/ (* 1e6 (bytecode-bench-time (lambda () (bytecode-bench-drive 200000)))) 200000))))
          ('regex (bytecode-bench-best
                   (lambda () (/ (* 1e6 (bytecode-bench-time
                                         (lambda () (dolist (s names)
                                                      (string-match "\\`\\(.*\\)\\(\\.el\\)\\'" s)))))
                                 300))))
          (_ (error "BENCH must be loop, call or regex"))))
       (ok (or (null limit) (<= value limit))))
  (princ (format "BYTECODE-BENCH %s %.4g%s\n" name value
                 (if limit (format " limit %s %s" limit (if ok "PASS" "FAIL")) "")))
  (kill-emacs (if ok 0 1)))
