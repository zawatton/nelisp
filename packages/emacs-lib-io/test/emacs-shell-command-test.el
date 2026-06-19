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
  ;; Re-install bindings explicitly so this test is robust against other
  ;; modules' fixtures (e.g. *--with-fresh-world*) that swap current-global-map.
  (emacs-shell-command--install-bindings)
  (let ((map (current-global-map)))
    (should (eq (lookup-key map (kbd "M-!")) #'shell-command))
    (should (eq (lookup-key map (kbd "M-|")) #'shell-command-on-region))
    (should (eq (lookup-key map (kbd "M-&")) #'async-shell-command))))

(defmacro emacs-shell-command-test--with-gui-backend (&rest body)
  "Run BODY with a mock GUI shell backend."
  (declare (indent 0) (debug t))
  `(let ((emacs-shell-command-gui-backend nil)
         (gui-arg "")
         (gui-buffer-name "")
         (gui-buffer-string "")
         (gui-compilation-string "")
         (gui-status "ok")
         (gui-files nil)
         (gui-project-directory "/tmp/project/sub")
         (gui-call-process-function nil)
         (gui-saved 0))
     (emacs-shell-command-gui-register-backend
      :arg (lambda () gui-arg)
      :set-arg (lambda (arg) (setq gui-arg arg))
      :set-status (lambda (status) (setq gui-status status))
      :transport-path (lambda (name) name)
      :write-file (lambda (path text)
                    (let ((cell (assoc path gui-files)))
                      (if cell
                          (setcdr cell text)
                        (push (cons path text) gui-files))))
      :read-file (lambda (path) (or (cdr (assoc path gui-files)) ""))
      :save-current-buffer-state (lambda () (setq gui-saved (1+ gui-saved)))
      :select-buffer (lambda (name _read-only)
                       (setq gui-buffer-name name)
                       (setq gui-buffer-string ""))
      :buffer-string (lambda () gui-buffer-string)
      :set-buffer-string (lambda (text) (setq gui-buffer-string text))
      :set-compilation-buffer-string
      (lambda (text) (setq gui-compilation-string text))
      :show-compilation-buffer
      (lambda ()
        (setq gui-buffer-name "*compilation*")
        (setq gui-buffer-string gui-compilation-string))
      :project-command-directory (lambda () gui-project-directory)
      :set-point (lambda (_point) nil)
      :apply-display-prefix-same-window (lambda () nil)
      :async-native-available-p (lambda () nil)
      :call-process (lambda (&rest args)
                      (if gui-call-process-function
                          (apply gui-call-process-function args)
                        127)))
     ,@body))

(ert-deftest emacs-shell-command-gui-shell-command-core ()
  (emacs-shell-command-test--with-gui-backend
    (setq gui-arg "printf shell-ok")
    (setq gui-call-process-function
          (lambda (&rest args)
            (let ((form (mapconcat #'identity (last args 2) " ")))
              (ignore form))
            (let ((script (car (last args))))
              (when (string-match "exec > \\([^ ]+\\)" script)
                (let ((path (match-string 1 script)))
                  (setcdr (assoc path gui-files) "shell-ok")))
              0)))
    (should (= 0 (emacs-shell-command-gui-shell-command)))
    (should (equal "*Shell Command Output*" gui-buffer-name))
    (should (equal "shell-ok" gui-buffer-string))
    (should (equal "ok" gui-status))
    (should (> gui-saved 0))))

(ert-deftest emacs-shell-command-gui-project-shell-command-prefixes-output ()
  (emacs-shell-command-test--with-gui-backend
    (setq gui-arg "pwd")
    (setq gui-call-process-function
          (lambda (&rest args)
            (let ((script (car (last args))))
              (when (string-match "exec > \\([^ ]+\\)" script)
                (setcdr (assoc (match-string 1 script) gui-files)
                        "/tmp/project/sub\n"))
              0)))
    (should (= 0 (emacs-shell-command-gui-project-shell-command)))
    (should (equal "pwd" gui-arg))
    (should (string-match-p
             "Project directory: /tmp/project/sub"
             gui-buffer-string))
    (should (string-match-p "/tmp/project/sub" gui-buffer-string))))

(ert-deftest emacs-shell-command-gui-project-compile-renders-compilation ()
  (emacs-shell-command-test--with-gui-backend
    (setq gui-arg "printf compile-ok")
    (setq gui-call-process-function
          (lambda (&rest args)
            (let ((script (car (last args))))
              (when (string-match "exec > \\([^ ]+\\)" script)
                (setcdr (assoc (match-string 1 script) gui-files)
                        "compile-ok"))
              0)))
    (should (= 0 (emacs-shell-command-gui-project-compile)))
    (should (equal "*compilation*" gui-buffer-name))
    (should (string-match-p "Compile command: printf compile-ok"
                            gui-buffer-string))
    (should (string-match-p "Exit status: 0" gui-buffer-string))
    (should (string-match-p "compile-ok" gui-buffer-string))))

(ert-deftest emacs-shell-command-gui-async-shell-command-fallback ()
  (emacs-shell-command-test--with-gui-backend
    (setq gui-arg "printf async-ok")
    (let (called)
      (setq gui-call-process-function
            (lambda (&rest args)
              (setq called args)
              (let ((destination (nth 2 args)))
                (let ((cell (assoc destination gui-files)))
                  (if cell
                      (setcdr cell "async-ok")
                    (push (cons destination "async-ok") gui-files))))
              0))
      (should (= 0 (emacs-shell-command-gui-async-shell-command)))
      (should called)
      (should (equal "*Async Shell Command*" gui-buffer-name))
      (should (equal "async-ok" gui-buffer-string))
      (should (equal "ok" gui-status)))))

(provide 'emacs-shell-command-test)

;;; emacs-shell-command-test.el ends here
