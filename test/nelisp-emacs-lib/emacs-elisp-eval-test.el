;;; emacs-elisp-eval-test.el --- ERT for interactive Elisp eval UI  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 02 v0.1 daily-driver §3.2.4 M2.4 tests for the eval command layer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-elisp-eval)

(defvar emacs-elisp-eval-test--counter 0)

(defun emacs-elisp-eval-test--symbol (prefix)
  "Return a fresh interned symbol starting with PREFIX."
  (setq emacs-elisp-eval-test--counter
        (1+ emacs-elisp-eval-test--counter))
  (intern (format "emacs-elisp-eval-test--%s-%d"
                  prefix
                  emacs-elisp-eval-test--counter)))

(defmacro emacs-elisp-eval-test--capture-message (&rest body)
  "Run BODY and return `(RESULT . MESSAGES)'."
  (declare (indent 0) (debug (body)))
  `(let ((messages nil))
     (cons
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (let ((text (apply #'format fmt args)))
                     (push text messages)
                     text))))
        ,@body)
      (nreverse messages))))

(ert-deftest eval-last-sexp-evaluates-and-prints ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(+ 1 2)")
    (goto-char (point-max))
    (pcase-let ((`(,result . ,messages)
                 (emacs-elisp-eval-test--capture-message
                   (call-interactively #'eval-last-sexp))))
      (should (= 3 result))
      (should (equal '("3") messages)))))

(ert-deftest eval-last-sexp-handles-error ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo)")
    (goto-char (point-max))
    (pcase-let ((`(,result . ,messages)
                 (emacs-elisp-eval-test--capture-message
                   (call-interactively #'eval-last-sexp))))
      (should-not result)
      (should (= 1 (length messages)))
      (should (string-match-p "foo" (car messages))))))

(ert-deftest eval-defun-defines-function ()
  (let ((name (emacs-elisp-eval-test--symbol "bar")))
    (unwind-protect
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert (format "(defun %s () 42)\n" name))
          (goto-char (point-max))
          (backward-char 3)
          (pcase-let ((`(,result . ,messages)
                       (emacs-elisp-eval-test--capture-message
                         (call-interactively #'eval-defun))))
            (should (eq name result))
            (should (equal (list (symbol-name name)) messages))
            (should (= 42 (funcall name)))))
      (when (fboundp name)
        (fmakunbound name)))))

(ert-deftest eval-region-evaluates-multiple-forms ()
  (let ((a (emacs-elisp-eval-test--symbol "a"))
        (b (emacs-elisp-eval-test--symbol "b")))
    (unwind-protect
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert (format "(setq %s 1)\n(setq %s (+ %s 2))\n"
                          a b a))
          (pcase-let ((`(,result . ,messages)
                       (emacs-elisp-eval-test--capture-message
                         (eval-region (point-min) (point-max)))))
            (should (= 3 result))
            (should (equal '("3") messages))
            (should (= 1 (symbol-value a)))
            (should (= 3 (symbol-value b)))))
      (makunbound a)
      (makunbound b))))

(ert-deftest eval-buffer-evaluates-all-toplevel-forms ()
  (let ((a (emacs-elisp-eval-test--symbol "buf-a"))
        (b (emacs-elisp-eval-test--symbol "buf-b")))
    (unwind-protect
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert (format "(setq %s 10)\n(setq %s (+ %s 5))\n"
                          a b a))
          (pcase-let ((`(,result . ,messages)
                       (emacs-elisp-eval-test--capture-message
                         (eval-buffer))))
            (should (= 15 result))
            (should (equal '("15") messages))
            (should (= 10 (symbol-value a)))
            (should (= 15 (symbol-value b)))))
      (makunbound a)
      (makunbound b))))

(ert-deftest emacs-elisp-eval-global-key-bindings-installed ()
  (let ((map (current-global-map)))
    (should (eq #'eval-last-sexp (lookup-key map (kbd "C-x C-e"))))
    (should (eq #'eval-defun (lookup-key map (kbd "C-M-x"))))))

(provide 'emacs-elisp-eval-test)

;;; emacs-elisp-eval-test.el ends here
