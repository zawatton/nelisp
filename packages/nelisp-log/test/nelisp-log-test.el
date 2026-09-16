;;; nelisp-log-test.el --- ERT tests for nelisp-log -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Coverage for `src/nelisp-log.el': level filtering, the default
;; formatter, pluggable sinks (stderr and a plain-function sink used
;; here to capture output without touching the real standard error),
;; the single default-logger mutable-state point, and invalid-level
;; rejection.

;;; Code:

(require 'ert)
(require 'nelisp-log)

(defun nelisp-log-test--collector ()
  "Return (SINK . GET) where SINK records (LEVEL . FORMATTED) and GET
returns the recorded list, oldest first.  Demonstrates the \"function
sink\" mechanism: an ordinary function, nothing else, used as a sink.

GET uses non-destructive `reverse', not `nreverse': it may be called
more than once per test, and `nreverse'-ing the same shared list twice
mangles it after the first call."
  (let ((seen nil))
    (cons (lambda (level formatted) (push (cons level formatted) seen))
          (lambda () (reverse seen)))))

(defmacro nelisp-log-test--with-default-logger (logger &rest body)
  "Run BODY with `nelisp-log-default-logger' bound to LOGGER, then restore it."
  (declare (indent 1))
  `(let ((nelisp-log-test--saved nelisp-log-default-logger))
     (unwind-protect
         (progn (nelisp-log-set-default-logger ,logger) ,@body)
       (nelisp-log-set-default-logger nelisp-log-test--saved))))

;;; Levels ---------------------------------------------------------------

(ert-deftest nelisp-log-levels-are-five-in-order ()
  (should (equal nelisp-log-levels '(trace debug info warn error))))

(ert-deftest nelisp-log-log-filters-below-threshold ()
  (let* ((collector (nelisp-log-test--collector))
         (logger (nelisp-log-make-logger :level 'warn :sink (car collector))))
    (nelisp-log-log logger 'info "should be filtered")
    (nelisp-log-log logger 'debug "should be filtered too")
    (should (null (funcall (cdr collector))))))

(ert-deftest nelisp-log-log-passes-at-or-above-threshold ()
  (let* ((collector (nelisp-log-test--collector))
         (logger (nelisp-log-make-logger :level 'warn :sink (car collector))))
    (nelisp-log-log logger 'warn "at threshold")
    (nelisp-log-log logger 'error "above threshold")
    (should (= (length (funcall (cdr collector))) 2))
    (should (equal (mapcar #'car (funcall (cdr collector))) '(warn error)))))

(ert-deftest nelisp-log-log-returns-formatted-or-nil ()
  (let ((logger (nelisp-log-make-logger :level 'info :sink #'ignore)))
    (should (equal (nelisp-log-log logger 'info "hi %s" "there") "[INFO] hi there"))
    (should (null (nelisp-log-log logger 'debug "not emitted")))))

(ert-deftest nelisp-log-make-logger-rejects-unknown-level ()
  (should-error (nelisp-log-make-logger :level 'verbose)
                :type 'nelisp-log-invalid-level))

(ert-deftest nelisp-log-log-rejects-unknown-level ()
  (let ((logger (nelisp-log-make-logger)))
    (should-error (nelisp-log-log logger 'verbose "x")
                  :type 'nelisp-log-invalid-level)))

;;; Default formatter ------------------------------------------------------

(ert-deftest nelisp-log-default-formatter-shape ()
  (should (equal (nelisp-log-default-formatter 'info "hello %s" '("world"))
                 "[INFO] hello world"))
  (should (equal (nelisp-log-default-formatter 'error "no args" nil)
                 "[ERROR] no args")))

(ert-deftest nelisp-log-default-formatter-uppercases-every-level ()
  (dolist (level nelisp-log-levels)
    (should (equal (nelisp-log-default-formatter level "x" nil)
                   (format "[%s] x" (upcase (symbol-name level)))))))

;;; Sinks ------------------------------------------------------------------

(ert-deftest nelisp-log-sink-stderr-does-not-signal ()
  "Smoke: the built-in sink must not itself raise on ordinary input.
`message' returns the string it wrote, not nil, so this checks that the
call completes rather than asserting a particular return value."
  (should (progn (nelisp-log-sink-stderr 'info "message text") t)))

(ert-deftest nelisp-log-custom-formatter-is-honored ()
  (let* ((collector (nelisp-log-test--collector))
         (logger (nelisp-log-make-logger
                  :level 'trace
                  :sink (car collector)
                  :formatter (lambda (level fmt args)
                               (format "<%s|%s>" level (apply #'format fmt args))))))
    (nelisp-log-log logger 'debug "n=%d" 3)
    (should (equal (cdr (car (funcall (cdr collector)))) "<debug|n=3>"))))

;;; Default logger: the one piece of mutable state -------------------------

(ert-deftest nelisp-log-convenience-functions-use-default-logger ()
  (let* ((collector (nelisp-log-test--collector))
         (logger (nelisp-log-make-logger :level 'trace :sink (car collector))))
    (nelisp-log-test--with-default-logger logger
      (nelisp-log-trace "t")
      (nelisp-log-debug "d")
      (nelisp-log-info "i")
      (nelisp-log-warn "w")
      (nelisp-log-error "e"))
    (should (equal (mapcar #'car (funcall (cdr collector)))
                   '(trace debug info warn error)))))

(ert-deftest nelisp-log-set-default-logger-replaces-it-wholesale ()
  (let ((original nelisp-log-default-logger)
        (replacement (nelisp-log-make-logger :level 'error)))
    (unwind-protect
        (progn
          (nelisp-log-set-default-logger replacement)
          (should (eq nelisp-log-default-logger replacement)))
      (nelisp-log-set-default-logger original))))

(ert-deftest nelisp-log-set-default-logger-rejects-non-logger ()
  (should-error (nelisp-log-set-default-logger "not a logger")
                :type 'wrong-type-argument))

(ert-deftest nelisp-log-set-level-mutates-in-place ()
  (let ((logger (nelisp-log-make-logger :level 'info)))
    (nelisp-log-set-level 'debug logger)
    (should (eq (nelisp-log-logger-level logger) 'debug))))

(ert-deftest nelisp-log-set-level-defaults-to-default-logger ()
  (nelisp-log-test--with-default-logger (nelisp-log-make-logger :level 'info)
    (nelisp-log-set-level 'trace)
    (should (eq (nelisp-log-logger-level nelisp-log-default-logger) 'trace))))

(ert-deftest nelisp-log-set-level-rejects-unknown-level ()
  (should-error (nelisp-log-set-level 'verbose (nelisp-log-make-logger))
                :type 'nelisp-log-invalid-level))

(provide 'nelisp-log-test)

;;; nelisp-log-test.el ends here
