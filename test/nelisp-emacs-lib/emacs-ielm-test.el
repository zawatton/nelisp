;;; emacs-ielm-test.el --- ERT tests for emacs-ielm  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Code:

(require 'ert)
(require 'emacs-ielm)

(defmacro emacs-ielm-test--with-clean-ielm (&rest body)
  "Run BODY with a fresh `*ielm*' buffer and empty module state."
  (declare (indent 0) (debug (body)))
  `(let ((emacs-ielm--state (make-hash-table :test 'eq :weakness nil)))
     (when (get-buffer ielm-buffer-name)
       (kill-buffer (get-buffer ielm-buffer-name)))
     (unwind-protect
         (progn ,@body)
       (when (get-buffer ielm-buffer-name)
         (kill-buffer (get-buffer ielm-buffer-name))))))

(defun emacs-ielm-test--buffer-string ()
  "Return the full contents of the active ielm buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun emacs-ielm-test--current-input ()
  "Return the current editable input in the active ielm buffer."
  (buffer-substring-no-properties (emacs-ielm--input-start) (point-max)))

(ert-deftest ielm-creates-buffer-with-prompt ()
  (emacs-ielm-test--with-clean-ielm
    (let ((buffer (ielm)))
      (should (buffer-live-p buffer))
      (should (equal ielm-buffer-name (buffer-name buffer)))
      (with-current-buffer buffer
        (should (eq major-mode 'inferior-emacs-lisp-mode))
        (should (string-suffix-p ielm-prompt
                                 (emacs-ielm-test--buffer-string)))))))

(ert-deftest ielm-evaluates-and-prints-result ()
  (emacs-ielm-test--with-clean-ielm
    (with-current-buffer (ielm)
      (goto-char (point-max))
      (insert "(+ 1 2)\n")
      (ielm-input-handler)
      (should (equal (concat ielm-prompt "(+ 1 2)\n3\n" ielm-prompt)
                     (emacs-ielm-test--buffer-string)))
      (should (equal '("(+ 1 2)") (emacs-ielm--history))))))

(ert-deftest ielm-handles-error-and-returns-to-prompt ()
  (emacs-ielm-test--with-clean-ielm
    (with-current-buffer (ielm)
      (goto-char (point-max))
      (insert "(foo)\n")
      (ielm-input-handler)
      (let ((contents (emacs-ielm-test--buffer-string)))
        (should (string-match-p (regexp-quote "(foo)\n") contents))
        (should (string-match-p "foo" contents))
        (should (string-suffix-p ielm-prompt contents))))))

(ert-deftest ielm-history-recall-with-m-p ()
  (emacs-ielm-test--with-clean-ielm
    (with-current-buffer (ielm)
      (goto-char (point-max))
      (insert "(+ 1 2)\n")
      (ielm-input-handler)
      (insert "(* 3 4)\n")
      (ielm-input-handler)
      (ielm-previous-input)
      (should (equal "(* 3 4)" (emacs-ielm-test--current-input)))
      (ielm-previous-input)
      (should (equal "(+ 1 2)" (emacs-ielm-test--current-input))))))

(ert-deftest ielm-reset-clears-history ()
  (emacs-ielm-test--with-clean-ielm
    (with-current-buffer (ielm)
      (goto-char (point-max))
      (insert "(+ 1 2)\n")
      (ielm-input-handler)
      (should (equal '("(+ 1 2)") (emacs-ielm--history)))
      (ielm-clear-buffer)
      (should (equal ielm-prompt (emacs-ielm-test--buffer-string)))
      (should (null (emacs-ielm--history)))
      (ielm-previous-input)
      (should (equal "" (emacs-ielm-test--current-input))))))

(ert-deftest ielm-history-m-n-returns-to-empty-input ()
  (emacs-ielm-test--with-clean-ielm
    (with-current-buffer (ielm)
      (goto-char (point-max))
      (insert "(+ 1 2)\n")
      (ielm-input-handler)
      (ielm-previous-input)
      (should (equal "(+ 1 2)" (emacs-ielm-test--current-input)))
      (ielm-next-input)
      (should (equal "" (emacs-ielm-test--current-input))))))

(provide 'emacs-ielm-test)

;;; emacs-ielm-test.el ends here
