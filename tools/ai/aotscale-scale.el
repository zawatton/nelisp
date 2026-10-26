;;; aotscale-scale.el --- AOT compile time of one function at sizes N, 2N, 4N, ...  -*- lexical-binding: t; -*-
;; AOTSCALE_DATA=row-data.el AOTSCALE_BASE=ROW-NAME AOTSCALE_KS="1 2 4 8" \
;;   emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/aotscale-scale.el
;; The probe function of size K is ROW's fxir body repeated K times in one
;; `:seq' (its callee list repeated K times, as the generator consumes callees
;; in walk order), compiled alone as the build compiles the fxir-native unit.
;; Prints  SCALE K SECS TEXT-BYTES  per size and the ratio per doubling.
(setq load-prefer-newer t max-lisp-eval-depth 200000)
(when (getenv "AOTSCALE_GCPCT") (setq gc-cons-percentage (string-to-number (getenv "AOTSCALE_GCPCT"))))
(require 'nelisp-standalone-build)
(require 'nelisp-cc-fxir-native)
(when (getenv "AOTSCALE_DATA")
  (setq nelisp-cc-fxir-native--data-files
        (list (cons (expand-file-name (getenv "AOTSCALE_DATA")) 'nelisp-fxir-native-data))))
(load (expand-file-name "aotscale-prof" (file-name-directory load-file-name)) nil t)
(defvar aotscale--maps nil)
(advice-add 'nelisp-aot-compiler--stackmap-finish :before
            (lambda (records &rest _) (setq aotscale--maps (append records aotscale--maps))))
(let* ((rows (cl-remove-if (lambda (r) (nth 6 r)) (nelisp-cc-fxir-native-all-rows)))
       (base (or (cl-find (intern (getenv "AOTSCALE_BASE")) rows :key #'car)
                 (error "no row %s" (getenv "AOTSCALE_BASE"))))
       (ks (mapcar #'string-to-number (split-string (or (getenv "AOTSCALE_KS") "1 2 4 8") " " t)))
       (nelisp-cc-fxir-native--row-index (make-hash-table :test 'eq))
       (nelisp-cc-fxir-native--direct-ids nil)
       (prev nil))
  (aotscale-prof-start)
  (dolist (r rows) (puthash (car r) r nelisp-cc-fxir-native--row-index))
  (dolist (k ks)
    (let* ((body (nth 1 (nth 4 base)))
           (row (list (intern (format "aotscale-probe-%d" k)) (+ 900 k) (nth 2 base)
                      (apply #'append (make-list k (nth 3 base)))
                      (list :fn (cons :seq (make-list k body)))
                      (nth 5 base)))
           (form (nelisp-cc-fxir-native--defun row))
           (aotscale--maps nil)
           (t0 (float-time))
           (r (let ((nelisp-standalone--inline-import-sources
                     (nelisp-standalone--unit-inline-imports "fxir-native.o")))
                (nelisp-standalone--compile-to-unit (format "aotscale-probe-%d.o" k) `(seq ,form))))
           (secs (- (float-time) t0))
           (text (apply #'+ 0 (mapcar (lambda (m) (- (plist-get m :end) (plist-get m :start))) aotscale--maps))))
      (message "SCALE %d %.2f %d%s" k secs text
               (if prev (format " ratio=%.2f" (/ secs (max 0.01 prev))) ""))
      (ignore r)
      (setq prev secs))))
(aotscale-prof-report)
