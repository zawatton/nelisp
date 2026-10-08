;;; nelisp-load-gnu-elc-test.el --- GNU .elc load tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-load)

(ert-deftest nelisp-load-gnu-elc/source-free-bytecode-and-effects-once ()
  "GNU .elc top-level effects run once and byte-code functions survive."
  (let* ((dir (make-temp-file "nelisp-gnu-elc" t))
         (source (expand-file-name "fixture.el" dir))
         (elc (concat source "c"))
         (count 'nelisp-load-gnu-elc-count)
         (function 'nelisp-load-gnu-elc-identity)
         (feature 'nelisp-load-gnu-elc-fixture)
         (object (cons 'identity nil)))
    (unwind-protect
        (progn
          (when (boundp count) (makunbound count))
          (when (memq feature features) (setq features (delq feature features)))
          (with-temp-file source
            (insert (format
                     "(defvar %s 0)\n(setq %s (1+ %s))\n(defun %s (x) x)\n(provide '%s)\n"
                     count count count function feature)))
          (unless (byte-compile-file source)
            (ert-fail "GNU byte compiler did not produce fixture"))
          (delete-file source)
          (should (file-exists-p elc))
          (should-error (nelisp-load-file elc) :type 'nelisp-load-error)
          (should-not (boundp count))
          (should-error (nelisp-eval count) :type 'nelisp-unbound-variable)
          (nelisp-load-gnu-elc-host elc)
          (should (= (symbol-value count) 1))
          (should (byte-code-function-p (symbol-function function)))
          (should (eq (funcall function object) object))
          (should-error (nelisp-eval (list function object))
                        :type 'nelisp-void-function)
          (should (memq feature features)))
      (when (boundp count) (makunbound count))
      (when (fboundp function) (fmakunbound function))
      (when (memq feature features) (setq features (delq feature features)))
      (when (file-exists-p source) (delete-file source))
      (when (file-exists-p elc) (delete-file elc))
      (delete-directory dir t))))

(ert-deftest nelisp-load-gnu-elc/truncated-file-runs-no-top-level-form ()
  "A malformed final form is rejected before any earlier form runs."
  (let* ((dir (make-temp-file "nelisp-gnu-elc-truncated" t))
         (elc (expand-file-name "broken.elc" dir))
         (counter 'nelisp-load-gnu-elc-truncated-counter)
         (header (concat ";ELC" (string 31) "\n"))
         (texts (list (concat header "(setq " (symbol-name counter)
                             " 1)\n(provide")
                      (concat header "(setq " (symbol-name counter)
                              " 1)\n(defalias 'truncated-bytecode #[nil \"\\207\" [] 1)"))))
    (unwind-protect
        (progn
          (dolist (text texts)
            (when (boundp counter) (makunbound counter))
            (with-temp-file elc (insert text))
            (should-error (nelisp-load-gnu-elc-host elc)
                          :type 'nelisp-load-error)
            (should-not (boundp counter))))
      (when (boundp counter) (makunbound counter))
      (delete-directory dir t))))

(provide 'nelisp-load-gnu-elc-test)
;;; nelisp-load-gnu-elc-test.el ends here
