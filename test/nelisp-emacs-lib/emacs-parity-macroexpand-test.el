;;; emacs-parity-macroexpand-test.el --- macroexpand-all parity tests -*- lexical-binding: t; -*-

;; These tests exercise `src/emacs-parity-macroexpand.el' on host Emacs even
;; though its definitions are normally gated behind
;; `(fboundp 'nelisp--write-stdout-bytes)' (standalone-only): they fake that
;; predicate for the dynamic extent of loading the file and restore the host
;; `macroexpand-1'/`macroexpand'/`macroexpand-all'/`macrop' subrs via
;; `cl-letf' afterwards, so the test never leaves the running Emacs session
;; with its macro expander replaced.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst emacs-parity-macroexpand-test--source-path
  (expand-file-name "../src/emacs-parity-macroexpand.el"
                     (file-name-directory (or load-file-name buffer-file-name)))
  "Path to src/emacs-parity-macroexpand.el, captured at load time.
`load-file-name' is only bound while this file itself is being loaded, so it
cannot be read lazily from inside a test body.")

(defun emacs-parity-macroexpand-test--source-forms ()
  "Read all top-level forms from src/emacs-parity-macroexpand.el."
  (with-temp-buffer
    (insert-file-contents emacs-parity-macroexpand-test--source-path)
    (goto-char (point-min))
    (let ((acc nil))
      (condition-case nil
          (while t (push (read (current-buffer)) acc))
        (end-of-file nil))
      (nreverse acc))))

(defmacro emacs-parity-macroexpand-test--with-standalone-defs (&rest body)
  "Run BODY with this library's standalone `macroexpand-*' shims installed.
Restores the host originals when BODY finishes, including on a non-local
exit, so no other test in the same batch run sees the substitute expander."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'macroexpand-1) (symbol-function 'macroexpand-1))
             ((symbol-function 'macroexpand) (symbol-function 'macroexpand))
             ((symbol-function 'macroexpand-all) (symbol-function 'macroexpand-all))
             ((symbol-function 'macrop) (symbol-function 'macrop))
             ((symbol-function 'macroexpand-all--rec)
              (if (fboundp 'macroexpand-all--rec)
                  (symbol-function 'macroexpand-all--rec)
                (lambda (&rest _) (error "unset"))))
             ((symbol-function 'fboundp)
              (let ((real-fboundp (symbol-function 'fboundp)))
                (lambda (sym)
                  (or (eq sym 'nelisp--write-stdout-bytes)
                      (funcall real-fboundp sym))))))
     (dolist (form (emacs-parity-macroexpand-test--source-forms))
       (eval form t))
     ,@body))

;; A real, globally-defined macro used as the probe: its expansion is
;; distinctive enough that we can tell whether `macroexpand-all--rec'
;; actually descended into it.
(defmacro emacs-parity-macroexpand-test--probe (x)
  `(1+ ,x))

(ert-deftest emacs-parity-macroexpand-test/rec-expands-inside-function-quote-lambda-body ()
  "`macroexpand-all--rec' must expand macros inside `#\\='(lambda ...)' BODY,
matching GNU's macroexp.el `(function . REST)' case (see the `(function
,(and f `(lambda . ,_)))' clause there, which recurses into the lambda via
`macroexp--all-forms F 2')."
  (emacs-parity-macroexpand-test--with-standalone-defs
    (should
     (equal
      '(function (lambda (x) (1+ x)))
      (macroexpand-all
       '(function (lambda (x) (emacs-parity-macroexpand-test--probe x))))))))

(ert-deftest emacs-parity-macroexpand-test/rec-leaves-bare-function-quote-symbol-alone ()
  "`#\\='SYMBOL' (a plain function-cell reference, not a lambda) is untouched."
  (emacs-parity-macroexpand-test--with-standalone-defs
    (should (equal '(function car) (macroexpand-all '(function car))))))

(ert-deftest emacs-parity-macroexpand-test/rec-preserves-arglist-and-expands-every-body-form ()
  "The ARGLIST is copied as-is (GNU starts its `macroexp--all-forms' walk at
index 2, i.e. after `lambda' and ARGLIST) while every BODY form is expanded,
not just the first one."
  (emacs-parity-macroexpand-test--with-standalone-defs
    (should
     (equal
      '(function (lambda (x y) (1+ x) (1+ y)))
      (macroexpand-all
       '(function (lambda (x y)
                    (emacs-parity-macroexpand-test--probe x)
                    (emacs-parity-macroexpand-test--probe y))))))))

(provide 'emacs-parity-macroexpand-test)
;;; emacs-parity-macroexpand-test.el ends here
