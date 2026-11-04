;;; emacs-eshell-test.el --- ERT for emacs-eshell  -*- lexical-binding: t; -*-

;;; Commentary:

;; eshell dispatch tests: Lisp-form evaluation, the cd/pwd/echo built-ins, an
;; external command via /bin/sh, and the comint input ring.

;;; Code:

(require 'ert)
(require 'emacs-eshell)

(defmacro emacs-eshell-test--with-eshell (&rest body)
  "Run BODY in a fresh `*eshell*' buffer, killing it afterwards."
  (declare (indent 0) (debug (body)))
  `(progn
     (when (get-buffer eshell-buffer-name)
       (kill-buffer eshell-buffer-name))
     (let ((buf (eshell)))
       (unwind-protect
           (with-current-buffer buf ,@body)
         (when (buffer-live-p buf) (kill-buffer buf))))))

(defun emacs-eshell-test--send (line)
  "Type LINE at the eshell prompt and submit it."
  (goto-char (point-max))
  (insert line)
  (eshell-send-input))

(ert-deftest emacs-eshell-test/creates-buffer-with-prompt ()
  (emacs-eshell-test--with-eshell
    (should (eq major-mode 'eshell-mode))
    (should (string-suffix-p eshell-prompt-string (buffer-string)))))

(ert-deftest emacs-eshell-test/evaluates-lisp-form ()
  (emacs-eshell-test--with-eshell
    (emacs-eshell-test--send "(+ 2 3)")
    (should (string-match-p "^5$" (buffer-string)))))

(ert-deftest emacs-eshell-test/echo-builtin ()
  (emacs-eshell-test--with-eshell
    (emacs-eshell-test--send "echo hi there")
    (should (string-match-p "^hi there$" (buffer-string)))))

(ert-deftest emacs-eshell-test/pwd-cd-builtins ()
  (let ((dir (file-name-as-directory (make-temp-file "emacs-eshell-test-" t))))
    (unwind-protect
        (emacs-eshell-test--with-eshell
          (emacs-eshell-test--send (concat "cd " dir))
          (emacs-eshell-test--send "pwd")
          (should (string-match-p (regexp-quote (directory-file-name dir))
                                  (buffer-string))))
      (delete-directory dir t))))

(ert-deftest emacs-eshell-test/external-command ()
  (skip-unless (file-executable-p "/bin/sh"))
  (let ((dir (file-name-as-directory (make-temp-file "emacs-eshell-test-" t))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "marker-eshell.txt" dir) (insert "m\n"))
          (emacs-eshell-test--with-eshell
            (emacs-eshell-test--send (concat "cd " dir))
            (emacs-eshell-test--send "ls")
            (should (string-match-p "marker-eshell" (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest emacs-eshell-test/input-ring-records ()
  (emacs-eshell-test--with-eshell
    (emacs-eshell-test--send "echo one")
    (emacs-eshell-test--send "(+ 1 1)")
    (should (equal '("(+ 1 1)" "echo one") (emacs-comint-input-ring)))))

(provide 'emacs-eshell-test)

;;; emacs-eshell-test.el ends here
