;;; emacs-cl-lib-setf-variable-places-test.el --- variable-cell places -*- lexical-binding: t; -*-

;; The standalone `setf' in src/cl-lib.el ends in a fallback that builds
;; `(funcall 'FN--setter ARG VALUE)' for an unrecognized place.  That is right
;; for accessors whose setter really is named that way, and wrong for places
;; the runtime can set directly: `(setf (default-value 'x) v)' became a call
;; to `default-value--setter', a name nothing defines.
;;
;; Measured 2026-09-12 against doom-modeline-core.el's
;;
;;   (setf (if default (default-value 'mode-line-format) mode-line-format)
;;         (list "%e" modeline))
;;
;; which is the form behind the real-init audit's `void-function' defect --
;; one of the two standalone-only defects in its first 306 init forms.
;;
;; `setf' here is installed only under the standalone marker, so these cases
;; fake it and load the source, the way
;; `emacs-parity-setf-places-test.el' does: without that, a host run would be
;; grading GNU's `setf' against itself.

(require 'ert)
(require 'cl-lib)

(defconst emacs-cl-lib-setf-variable-places-test--source
  (expand-file-name "../src/cl-lib.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defvar emacs-cl-lib-setf-variable-places-test--var "original")

(defmacro emacs-cl-lib-setf-variable-places-test--with-standalone (&rest body)
  "Install the standalone `setf' for BODY and put the host macro back after."
  (declare (indent 0))
  `(let ((host-setf (symbol-function 'setf))
         (had-marker (fboundp 'nl-write-file)))
     (unwind-protect
         (progn
           (unless had-marker (fset 'nl-write-file (lambda (&rest _) nil)))
           (load emacs-cl-lib-setf-variable-places-test--source nil t)
           (should-not (eq host-setf (symbol-function 'setf)))
           ,@body)
       (fset 'setf host-setf)
       (unless had-marker (fmakunbound 'nl-write-file)))))

(ert-deftest emacs-cl-lib-setf-variable-places/default-value-and-symbol-value ()
  (emacs-cl-lib-setf-variable-places-test--with-standalone
    (should
     (equal "by-default"
            (eval '(progn
                     (setf (default-value
                            'emacs-cl-lib-setf-variable-places-test--var)
                           "by-default")
                     (default-value
                      'emacs-cl-lib-setf-variable-places-test--var))
                  t)))
    (should
     (equal "by-symbol-value"
            (eval '(progn
                     (setf (symbol-value
                            'emacs-cl-lib-setf-variable-places-test--var)
                           "by-symbol-value")
                     emacs-cl-lib-setf-variable-places-test--var)
                  t)))))

(ert-deftest emacs-cl-lib-setf-variable-places/doom-modeline-if-shape ()
  "The exact place shape that produced the audit's `void-function'."
  (emacs-cl-lib-setf-variable-places-test--with-standalone
    (should
     (equal "then-branch"
            (eval '(progn
                     (setf (if t
                               (default-value
                                'emacs-cl-lib-setf-variable-places-test--var)
                             emacs-cl-lib-setf-variable-places-test--var)
                           "then-branch")
                     (default-value
                      'emacs-cl-lib-setf-variable-places-test--var))
                  t)))
    (should
     (equal "else-branch"
            (eval '(progn
                     (setf (if nil
                               (default-value
                                'emacs-cl-lib-setf-variable-places-test--var)
                             emacs-cl-lib-setf-variable-places-test--var)
                           "else-branch")
                     emacs-cl-lib-setf-variable-places-test--var)
                  t)))))

(ert-deftest emacs-cl-lib-setf-variable-places/control-flow-places ()
  "`progn' and `cond' places recurse like `if' rather than synthesizing a name."
  (emacs-cl-lib-setf-variable-places-test--with-standalone
    (should
     (equal "via-progn"
            (eval '(progn
                     (setf (progn emacs-cl-lib-setf-variable-places-test--var)
                           "via-progn")
                     emacs-cl-lib-setf-variable-places-test--var)
                  t)))
    (should
     (equal "via-cond"
            (eval '(progn
                     (setf (cond (t emacs-cl-lib-setf-variable-places-test--var))
                           "via-cond")
                     emacs-cl-lib-setf-variable-places-test--var)
                  t)))))

(ert-deftest emacs-cl-lib-setf-variable-places/accessor-fallback-still-reached ()
  "A place with no rule still routes to its ACCESSOR--setter, as before."
  (emacs-cl-lib-setf-variable-places-test--with-standalone
    (fset 'emacs-cl-lib-setf-variable-places-test--slot--setter
          (lambda (obj value) (list 'called obj value)))
    (unwind-protect
        (should
         (equal '(called holder 5)
                (eval '(setf (emacs-cl-lib-setf-variable-places-test--slot
                              'holder)
                             5)
                      t)))
      (fmakunbound 'emacs-cl-lib-setf-variable-places-test--slot--setter))))

(provide 'emacs-cl-lib-setf-variable-places-test)

;;; emacs-cl-lib-setf-variable-places-test.el ends here
