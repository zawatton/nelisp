;;; standalone-native-cache-load-bench.el --- native cache load timing -*- lexical-binding: t -*-

;; Usage (from the repo root, after byte-compiling the fixture):
;;   NCL_MODE=compile|load NCL_CACHE=DIR NCL_ELC=FILE.elc \
;;     nelisp -L lisp -L src -L scripts --load test/standalone-native-cache-load-bench.el
;;
;; compile: in-house-compile the fixture's functions into NCL_CACHE.
;; load:    load them from the cache in this fresh process and print one
;;          NCL-BENCH line with the total load time divided by a native
;;          SHA-256 of 8 MiB timed in the same run, so the ratio tracks
;;          the loader rather than machine load.  With NCL_LIMIT set,
;;          exit non-zero when the ratio exceeds it.

(require 'nelisp-native-cache)
(require 'nelisp-bytecode-native-consumer)
(setenv "NELISP_NATIVE_CACHE" (getenv "NCL_CACHE"))

(defun ncl-bench--holders ()
  (mapcar (lambda (sym)
            (let ((holder (make-symbol (symbol-name sym))))
              (fset holder (nelisp-bytecode-native-consumer-read-elc-function
                            (getenv "NCL_ELC") sym))
              holder))
          '(ncl-eq ncl-split ncl-dedup)))

(let ((mode (getenv "NCL_MODE")) (ok t) (total 0.0))
  (dolist (holder (ncl-bench--holders))
    (let ((t0 (float-time))
          (res (condition-case err
                   (let ((nelisp-native-cache-backend 'in-house))
                     (if (equal mode "compile")
                         (nelisp-native-cache-compile holder)
                       (nelisp-native-cache-load holder)))
                 (error (list 'error err)))))
      (setq total (+ total (- (float-time) t0)))
      (when (or (null res) (eq (car-safe res) 'error))
        (setq ok nil)
        (princ (format "NCL-BENCH %s %s failed: %S\n" mode holder res)))))
  (if (equal mode "compile")
      (princ (format "NCL-BENCH compile %.2fs %s\n" total (if ok "PASS" "FAIL")))
    (let* ((input (make-string (* 8 1024 1024) ?x))
           (t0 (progn (garbage-collect) (float-time)))
           (ref (progn (nelisp--sha256 input) (- (float-time) t0)))
           (ratio (/ total ref))
           (limit (let ((l (getenv "NCL_LIMIT"))) (and l (> (length l) 0) (string-to-number l)))))
      (when (and limit (> ratio limit)) (setq ok nil))
      (princ (format "NCL-BENCH load=%.2fs ref=%.2fs ratio=%.2f limit=%s %s\n"
                     total ref ratio limit (if ok "PASS" "FAIL")))))
  (kill-emacs (if ok 0 1)))

;;; standalone-native-cache-load-bench.el ends here
