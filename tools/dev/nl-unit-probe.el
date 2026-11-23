;;; nl-unit-probe.el --- record unit inputs/outputs of the first units  -*- lexical-binding: t; -*-
(require 'nelisp-standalone-build)
(defvar nlup--out (getenv "C6_OUT"))
(defvar nlup--max (string-to-number (or (getenv "C6_MAX") "3")))
(defvar nlup--n 0)
(defun nlup--emit (&rest forms)
  (with-temp-buffer
    (let ((print-length nil) (print-level nil) (print-circle t))
      (dolist (f forms) (prin1 f (current-buffer)) (insert "\n")))
    (write-region (point-min) (point-max) nlup--out t 'silent)))
(defun nlup--state ()
  (list :arch nelisp-aot-compiler--arch :abi nelisp-aot-compiler--abi
        :checked nelisp-aot-compiler--checked-arith
        :runtime-entry nelisp-aot-compiler--runtime-entry-params
        :internal-entry nelisp-aot-compiler--lisp-internal-entry-names
        :native-emitter nelisp-aot-compiler--native-emit-enabled
        :jit-callout (fboundp 'nl-jit-call-out-2)
        :jump-tables nelisp-aot-compiler--x86-jump-tables-enabled
        :call-free nelisp-aot-compiler--x86-call-free-regions
        :affine nelisp-aot-compiler--x86-affine-addresses
        :inline (list (nelisp-standalone--inline-level) nelisp-standalone--inline-max-nodes)
        :native-load-loaded (featurep 'nelisp-native-load)
        :features (sort (mapcar #'symbol-name (seq-filter (lambda (f) (string-prefix-p "nelisp" (symbol-name f))) features)) #'string<)))
(advice-add 'nelisp-standalone--compile-to-unit :around
  (lambda (orig name source &rest r)
    (setq nlup--n (1+ nlup--n))
    (nlup--emit (list :unit name :state (nlup--state)))
    (let ((unit (apply orig name source r)))
      (nlup--emit (list :unit-sha name
                      (secure-hash 'sha1 (let ((print-length nil) (print-level nil) (print-circle t))
                                           (prin1-to-string (nelisp-standalone--unit-cache-encode unit))))))
      (when (>= nlup--n nlup--max) (kill-emacs 0))
      unit)))
