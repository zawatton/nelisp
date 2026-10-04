;;; nelisp-symbol-function-wiring-test.el --- Signal ABI qualification -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-standalone-build)
(require 'nelisp-cc-evalport-env-leaves-bind)

(defun nelisp-symbolfn-test--check (form)
  "Check signal calls in FORM against the actual imported DSL declaration."
  (let* ((definition
          (seq-find (lambda (item)
                      (and (consp item) (eq (car item) 'defun)
                           (eq (cadr item) 'nl_env_stash_signal)))
                    (cdr nelisp-cc-evalport-env-leaves-bind--source)))
         (arity (length (nth 2 definition)))
         (checked 0))
    (unless definition (error "Missing imported signal declaration"))
    (cl-labels ((walk (value)
                 (when (consp value)
                   (when (eq (car value) 'nl_env_stash_signal)
                     (setq checked (1+ checked))
                     (unless (= (1- (length value)) arity)
                       (error "Signal ABI requires %d arguments" arity)))
                   (mapc #'walk value))))
      (walk form))
    (unless (> checked 0) (error "No signal calls checked"))
    checked))

(ert-deftest nelisp-symbolfn-signal-abi-matches-import ()
  (should (= 1 (nelisp-symbolfn-test--check
                nelisp-standalone--reader-do-symbol-function-fixed))))

(ert-deftest nelisp-symbolfn-signal-abi-rejects-original-defect ()
  (should-error
   (nelisp-symbolfn-test--check '(seq (nl_env_stash_signal env symbol data)))
   :type 'error))

(ert-deftest nelisp-symbolfn-signal-abi-rejects-empty-scope ()
  (should-error (nelisp-symbolfn-test--check '(seq)) :type 'error))
