;;; emacs-shell-test.el --- ERT for emacs-shell  -*- lexical-binding: t; -*-

;;; Commentary:

;; comint-based shell tests.  Buffer/prompt/ring are pure buffer units; the
;; command path drives a real `/bin/sh -c' through `call-process'.

;;; Code:

(require 'ert)
(require 'emacs-shell)

(defmacro emacs-shell-test--with-shell (&rest body)
  "Run BODY in a fresh `*shell*' buffer, killing it afterwards."
  (declare (indent 0) (debug (body)))
  `(progn
     (when (get-buffer emacs-shell-buffer-name)
       (kill-buffer emacs-shell-buffer-name))
     (let ((buf (emacs-shell)))
       (unwind-protect
           (with-current-buffer buf ,@body)
         (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest emacs-shell-test/creates-buffer-with-prompt ()
  (emacs-shell-test--with-shell
    (should (eq major-mode 'shell-mode))
    (should (string-suffix-p emacs-shell-prompt-string (buffer-string)))))

(ert-deftest emacs-shell-test/runs-command-and-prints-output ()
  (skip-unless (file-executable-p "/bin/sh"))
  (emacs-shell-test--with-shell
    (goto-char (point-max))
    (insert "echo shell-works")
    (emacs-shell-send-input)
    (should (string-match-p "shell-works" (buffer-string)))
    ;; a fresh prompt follows the output
    (should (string-suffix-p emacs-shell-prompt-string (buffer-string)))))

(ert-deftest emacs-shell-test/cd-changes-working-directory ()
  (skip-unless (file-executable-p "/bin/sh"))
  (let ((dir (file-name-as-directory (make-temp-file "emacs-shell-test-" t))))
    (unwind-protect
        (emacs-shell-test--with-shell
          (goto-char (point-max)) (insert (concat "cd " dir))
          (emacs-shell-send-input)
          (goto-char (point-max)) (insert "pwd")
          (emacs-shell-send-input)
          (should (string-match-p (regexp-quote (directory-file-name dir))
                                  (buffer-string))))
      (delete-directory dir t))))

(ert-deftest emacs-shell-test/input-ring-records-commands ()
  (emacs-shell-test--with-shell
    (goto-char (point-max)) (insert "echo one") (emacs-shell-send-input)
    (goto-char (point-max)) (insert "echo two") (emacs-shell-send-input)
    (should (equal '("echo two" "echo one") (emacs-comint-input-ring)))))

(ert-deftest emacs-shell-test/blank-input-just-reprompts ()
  (emacs-shell-test--with-shell
    (let ((before (buffer-string)))
      (goto-char (point-max))
      (emacs-shell-send-input)
      ;; nothing ran; a new prompt was appended
      (should (string-suffix-p emacs-shell-prompt-string (buffer-string)))
      (should (> (length (buffer-string)) (length before))))))

(provide 'emacs-shell-test)

;;; emacs-shell-test.el ends here
