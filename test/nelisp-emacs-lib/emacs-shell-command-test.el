;;; emacs-shell-command-test.el --- ERT for emacs-shell-command  -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused coverage for the interactive shell-command layer in
;; docs/design/02-v01-daily-driver.org §3.4.1.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-shell-command)

(defmacro emacs-shell-command-test--skip-unless-shell (&rest body)
  "Run BODY only when /bin/sh is available."
  (declare (indent 0) (debug (body)))
  `(if (file-executable-p shell-file-name)
       (progn ,@body)
     (ert-skip "shell not available")))

(defun emacs-shell-command-test--wait-for-process (process &optional timeout)
  "Wait until PROCESS exits, for at most TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 2.0))))
    (while (and (process-live-p process)
                (< (float-time) deadline))
      (accept-process-output process 0.05))
    (should-not (process-live-p process))
    process))

(ert-deftest shell-command-runs-and-displays-output ()
  (emacs-shell-command-test--skip-unless-shell
    (let ((buffer (get-buffer-create " *shell-command-test-out*")))
      (unwind-protect
          (progn
            (should (= 0 (shell-command "printf 'hello\\n'" buffer)))
            (with-current-buffer buffer
              (should (equal (buffer-string) "hello\n"))))
        (kill-buffer buffer)))))

(ert-deftest shell-command-handles-non-zero-exit ()
  (emacs-shell-command-test--skip-unless-shell
    (let ((buffer (get-buffer-create " *shell-command-test-fail*"))
          message-log)
      (unwind-protect
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-log (apply #'format fmt args)))))
            (should (= 7 (shell-command "exit 7" buffer)))
            (should (string-match "status 7" message-log)))
        (kill-buffer buffer)))))

(ert-deftest shell-command-on-region-pipes-stdin ()
  (emacs-shell-command-test--skip-unless-shell
    (let ((buffer (get-buffer-create " *shell-command-test-region*")))
      (unwind-protect
          (with-temp-buffer
            (insert "alpha\nbeta\n")
            (should (= 0 (shell-command-on-region
                          (point-min) (point-max) "cat" buffer nil)))
            (with-current-buffer buffer
              (should (equal (buffer-string) "alpha\nbeta\n"))))
        (kill-buffer buffer)))))

(ert-deftest shell-command-on-region-with-replace-flag ()
  (emacs-shell-command-test--skip-unless-shell
    (with-temp-buffer
      (insert "alpha\nbeta\n")
      (should (= 0 (shell-command-on-region
                    (point-min) (point-max) "tr a-z A-Z" nil t)))
      (should (equal (buffer-string) "ALPHA\nBETA\n")))))

(ert-deftest async-shell-command-spawns-non-blocking-process ()
  (emacs-shell-command-test--skip-unless-shell
    (let ((buffer (get-buffer-create " *shell-command-test-async*")))
      (unwind-protect
          (let* ((start (float-time))
                 (process (async-shell-command "sleep 1; printf done\\n" buffer))
                 (elapsed (- (float-time) start)))
            (should (processp process))
            (should (< elapsed 0.5))
            (should (process-live-p process))
            (emacs-shell-command-test--wait-for-process process 2.5))
        (kill-buffer buffer)))))

(ert-deftest async-shell-command-output-buffer-receives-data ()
  (emacs-shell-command-test--skip-unless-shell
    (let ((buffer (get-buffer-create " *shell-command-test-async-out*")))
      (unwind-protect
          (let ((process (async-shell-command "printf async-data\\n" buffer)))
            (emacs-shell-command-test--wait-for-process process 2.0)
            (with-current-buffer buffer
              (should (string-match "async-data" (buffer-string)))))
        (kill-buffer buffer)))))

(ert-deftest emacs-shell-command-install-bindings ()
  (let ((map (current-global-map)))
    (should (eq (lookup-key map (kbd "M-!")) #'shell-command))
    (should (eq (lookup-key map (kbd "M-|")) #'shell-command-on-region))
    (should (eq (lookup-key map (kbd "M-&")) #'async-shell-command))))

(provide 'emacs-shell-command-test)

;;; emacs-shell-command-test.el ends here
