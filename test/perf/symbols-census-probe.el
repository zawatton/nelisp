;;; symbols-census-probe.el --- Doc 226 S1.2 dynamic census workload -*- lexical-binding: t; -*-

;; SYMC_PATH selects one path and SYMC_N its iterations; symbols-check.py
;; census runs it under callgrind with N iterations and with 0, and counts
;; the mirror's hashed walks and entry insertions per iteration.  Every
;; symbol used is set up before the loop, so the loop itself only reads,
;; writes and binds existing variables, as GNU's find_symbol_value,
;; set_internal and specbind do (data.c:1619, data.c:1678, eval.c:3656).
(defvar symc-plain 1)
(defvar symc-base 2)
(defvaralias 'symc-alias 'symc-base)
(defvar symc-n (string-to-number (or (getenv "SYMC_N") "0")))
(defvar symc-path (intern (or (getenv "SYMC_PATH") "plain-read")))

(defun symc-run (n)
  (let ((i 0) (acc nil) (s-plain 'symc-plain) (s-alias 'symc-alias)
        (s-fwd 'max-lisp-eval-depth) (depth max-lisp-eval-depth))
    (pcase symc-path
      ('plain-read
       (while (< i n)
         (setq acc (list (symbol-value s-plain) (boundp s-plain) (default-value s-plain) symc-plain))
         (setq i (1+ i))))
      ('plain-write
       (while (< i n)
         (set s-plain i) (setq symc-plain i) (set-default s-plain i)
         (setq i (1+ i))))
      ('plain-bind
       (while (< i n)
         (setq acc (let ((symc-plain i)) (let* ((symc-plain (1+ i))) symc-plain)))
         (setq i (1+ i))))
      ('alias-read
       (while (< i n)
         (setq acc (list (symbol-value s-alias) (boundp s-alias) (default-value s-alias) symc-alias))
         (setq i (1+ i))))
      ('alias-write
       (while (< i n)
         (set s-alias i) (setq symc-alias i) (set-default s-alias i)
         (setq i (1+ i))))
      ('alias-bind
       (while (< i n)
         (setq acc (let ((symc-alias i)) symc-base))
         (setq i (1+ i))))
      ('fwd-read
       (while (< i n)
         (setq acc (list (symbol-value s-fwd) (boundp s-fwd) max-lisp-eval-depth))
         (setq i (1+ i))))
      ('fwd-write
       (while (< i n)
         (set s-fwd depth) (setq max-lisp-eval-depth depth)
         (setq i (1+ i))))
      ('fwd-bind
       (while (< i n)
         (setq acc (let ((max-lisp-eval-depth (+ depth 1))) max-lisp-eval-depth))
         (setq i (1+ i))))
      ('uninterned-read
       (let ((u (make-symbol "symc-u")))
         (set u 1)
         (while (< i n)
           (setq acc (list (symbol-value u) (boundp u)))
           (setq i (1+ i))))))
    acc))

(symc-run 3)
(princ (format "SYMC %s %d %S\n" symc-path symc-n (symc-run symc-n)))
