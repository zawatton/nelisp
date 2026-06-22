;;; emacs-buffer-ui-test.el --- ERT tests for emacs-buffer-ui.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-buffer-ui)
(require 'emacs-mode)

(defmacro emacs-buffer-ui-test--with-fresh-world (&rest body)
  "Run BODY with clean buffer/window/minibuffer/file state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (emacs-buffer--state (make-hash-table :test 'eq))
         (emacs-buffer--variable-buffer-local nil)
         (emacs-buffer--default-values (make-hash-table :test 'eq))
         (emacs-fileio--buffer-files nil)
         (emacs-window--id-counter 0)
         (emacs-window--root nil)
         (emacs-window--selected nil)
         (emacs-minibuffer--depth 0)
         (emacs-minibuffer--buffers nil)
         (emacs-minibuffer--prompts nil)
         (emacs-minibuffer--prompt-ends nil)
         (emacs-minibuffer--window nil)
         (emacs-minibuffer--saved-window nil)
         (emacs-minibuffer--input-queue nil)
         (emacs-minibuffer-history nil)
         (emacs-minibuffer-default nil)
         (minibuffer-completion-table nil)
         (minibuffer-completion-confirm nil))
     (emacs-mode-reset)
     ,@body))

(ert-deftest switch-to-buffer-completes-existing ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "alpha"))
          (b (nelisp-ec-generate-new-buffer "beta")))
      (emacs-window-set-window-buffer (emacs-window-selected-window) a)
      (nelisp-ec-set-buffer a)
      (emacs-minibuffer-feed-input "beta")
      (should (eq b (call-interactively #'switch-to-buffer-interactive)))
      (should (eq b (emacs-window-window-buffer)))
      (should (eq b (nelisp-ec-current-buffer))))))

(ert-deftest find-buffer-public-wrapper ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "alpha")))
      (should (eq buf (emacs-buffer-ui-find-buffer "alpha")))
      (should-not (emacs-buffer-ui-find-buffer "missing")))))

(ert-deftest switch-to-buffer-plan-reports-statuses ()
  (emacs-buffer-ui-test--with-fresh-world
    (should (equal 'empty
                   (plist-get
                    (emacs-buffer-ui-switch-to-buffer-plan "")
                    :status)))
    (let ((missing (emacs-buffer-ui-switch-to-buffer-plan
                    "missing" (lambda (_name) nil))))
      (should (eq 'missing (plist-get missing :status)))
      (should (equal "No buffer: missing" (plist-get missing :message))))
    (let ((ok (emacs-buffer-ui-switch-to-buffer-plan
               "alpha" (lambda (name) (equal name "alpha")))))
      (should (eq 'ok (plist-get ok :status)))
      (should (equal "alpha" (plist-get ok :buffer-name)))
      (should (equal 0 (plist-get ok :scroll-offset)))
      (should (equal "Switched: alpha" (plist-get ok :message))))))

(ert-deftest kill-buffer-plan-reports-statuses ()
  (let ((refused (emacs-buffer-ui-kill-buffer-plan "*welcome*")))
    (should (eq 'refused (plist-get refused :status)))
    (should (equal "kill-buffer: refusing *welcome*"
                   (plist-get refused :message))))
  (let ((ok (emacs-buffer-ui-kill-buffer-plan "alpha")))
    (should (eq 'ok (plist-get ok :status)))
    (should (equal "alpha" (plist-get ok :buffer-name)))
    (should (equal "*welcome*" (plist-get ok :fallback-buffer)))
    (should (equal 0 (plist-get ok :scroll-offset)))
    (should (equal "Killed: alpha" (plist-get ok :message)))))

(ert-deftest confirm-kill-buffer-prompts-only-when-modified ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((buffer (nelisp-ec-generate-new-buffer "alpha"))
          prompts)
      (should
       (emacs-buffer-ui-confirm-kill-buffer
        buffer "alpha"
        (lambda (prompt)
          (push prompt prompts)
          "no")))
      (should-not prompts)
      (emacs-buffer-set-buffer-modified-p t buffer)
      (should
       (emacs-buffer-ui-confirm-kill-buffer
        buffer "alpha"
        (lambda (prompt)
          (push prompt prompts)
          "y")))
      (should (equal '("Buffer alpha modified; kill anyway? ") prompts))
      (should-not
       (emacs-buffer-ui-confirm-kill-buffer
        buffer "alpha"
        (lambda (_prompt) "no"))))))

(ert-deftest buffer-menu-entry-and-spec ()
  (should-not (emacs-buffer-ui-buffer-menu-entry " hidden"))
  (should-not (emacs-buffer-ui-buffer-menu-entry ""))
  (should (equal '("  alpha" . "switch-to-buffer:alpha")
                 (emacs-buffer-ui-buffer-menu-entry "alpha")))
  (should (equal '("* beta  (/tmp/beta.txt)" . "switch-to-buffer:beta")
                 (emacs-buffer-ui-buffer-menu-entry
                  "beta" "/tmp/beta.txt" t)))
  (let ((spec (emacs-buffer-ui-buffer-menu-spec
               '(:a :hidden :b)
               :name-function (lambda (buf)
                                (cond
                                 ((eq buf :a) "alpha")
                                 ((eq buf :hidden) " hidden")
                                 ((eq buf :b) "beta")))
               :file-function (lambda (buf)
                                (and (eq buf :b) "/tmp/beta.txt"))
               :modified-function (lambda (buf)
                                    (eq buf :b)))))
    (should (equal '(("  alpha" . "switch-to-buffer:alpha")
                    ("* beta  (/tmp/beta.txt)"
                     . "switch-to-buffer:beta"))
                   spec))))

(ert-deftest switch-to-buffer-creates-new-on-unknown-name ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "alpha")))
      (emacs-window-set-window-buffer (emacs-window-selected-window) a)
      (nelisp-ec-set-buffer a)
      (emacs-minibuffer-feed-input "gamma")
      (let ((buf (call-interactively #'switch-to-buffer-interactive)))
        (should (equal "gamma" (nelisp-ec-buffer-name buf)))
        (should (memq buf (emacs-buffer-buffer-list)))
        (should (eq buf (emacs-window-window-buffer)))))))

(ert-deftest run-switch-buffer-command-uses-frontend-hooks ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "alpha"))
          (b (nelisp-ec-generate-new-buffer "beta"))
          synced after prompts)
      (emacs-window-set-window-buffer (emacs-window-selected-window) a)
      (nelisp-ec-set-buffer a)
      (let ((result
             (emacs-buffer-ui-run-switch-buffer-command
              :read-string (lambda (prompt)
                             (push prompt prompts)
                             "beta")
              :sync-window (lambda (buffer) (setq synced buffer))
              :after-success (lambda (buffer) (setq after buffer)))))
        (should (eq result b))
        (should (eq synced b))
        (should (eq after b))
        (should (eq (nelisp-ec-current-buffer) b))
        (should (member "Switch to buffer (default alpha): " prompts))))))

(ert-deftest run-switch-existing-command-uses-read-string-hooks ()
  (let (prompts applied status)
    (let ((plan
           (emacs-buffer-ui-run-switch-existing-command
            :read-string (lambda (prompt)
                           (push prompt prompts)
                           "target")
            :buffer-exists-p (lambda (name)
                               (equal name "target"))
            :apply-plan (lambda (plan)
                          (setq applied plan))
            :status-function (lambda (message)
                               (setq status message)))))
      (should (eq 'ok (plist-get plan :status)))
      (should (eq applied plan))
      (should (equal "Switched: target" status))
      (should (equal '("Switch to buffer: ") prompts)))))

(ert-deftest run-switch-existing-command-uses-begin-prompt-hooks ()
  (let (prompt callback applied status)
    (emacs-buffer-ui-run-switch-existing-command
     :begin-prompt (lambda (p cb)
                     (setq prompt p
                           callback cb)
                     :started)
     :buffer-exists-p (lambda (name)
                        (equal name "target"))
     :apply-plan (lambda (plan)
                   (setq applied plan))
     :status-function (lambda (message)
                        (setq status message)))
    (should (equal "Switch to buffer: " prompt))
    (let ((plan (funcall callback "target")))
      (should (eq 'ok (plist-get plan :status)))
      (should (eq applied plan))
      (should (equal "Switched: target" status)))))

(ert-deftest kill-buffer-removes-from-buffer-list ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "alpha"))
          (b (nelisp-ec-generate-new-buffer "beta")))
      (emacs-window-set-window-buffer (emacs-window-selected-window) a)
      (nelisp-ec-set-buffer a)
      (should (memq a (emacs-buffer-buffer-list)))
      (should (eq t (kill-buffer-interactive a)))
      (should-not (memq a (emacs-buffer-buffer-list)))
      (should (eq b (emacs-window-window-buffer))))))

(ert-deftest run-kill-buffer-command-uses-frontend-hooks ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "alpha"))
          (b (nelisp-ec-generate-new-buffer "beta"))
          synced after prompts)
      (emacs-window-set-window-buffer (emacs-window-selected-window) a)
      (nelisp-ec-set-buffer a)
      (should
       (emacs-buffer-ui-run-kill-buffer-command
        :read-string (lambda (prompt)
                       (push prompt prompts)
                       "")
        :sync-window (lambda (buffer) (setq synced buffer))
        :after-success (lambda (buffer) (setq after buffer))))
      (should (null synced))
      (should (eq after a))
      (should-not (memq a (emacs-buffer-buffer-list)))
      (should (eq b (emacs-window-window-buffer)))
      (should (member "Kill buffer (default alpha): " prompts)))))

(ert-deftest run-kill-buffer-plan-command-uses-frontend-hooks ()
  (let (killed applied status)
    (let ((plan
           (emacs-buffer-ui-run-kill-buffer-plan-command
            :current-name (lambda () "target")
            :kill-function (lambda (name)
                             (setq killed name))
            :apply-plan (lambda (plan)
                          (setq applied plan))
            :status-function (lambda (message)
                               (setq status message)))))
      (should (eq 'ok (plist-get plan :status)))
      (should (equal "target" killed))
      (should (eq applied plan))
      (should (equal "Killed: target" status)))))

(ert-deftest kill-buffer-prompts-on-modified ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "alpha")))
      (emacs-window-set-window-buffer (emacs-window-selected-window) a)
      (nelisp-ec-set-buffer a)
      (emacs-buffer-set-buffer-modified-p t a)
      (let ((prompt nil))
        (cl-letf (((symbol-function 'emacs-minibuffer-yes-or-no-p)
                   (lambda (message)
                     (setq prompt message)
                     t)))
          (should (eq t (kill-buffer-interactive a))))
        (should (string-match-p "modified; kill anyway" prompt))))))

(ert-deftest list-buffers-renders-four-columns ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "alpha"))
          (b (nelisp-ec-generate-new-buffer "beta")))
      (nelisp-ec-with-current-buffer a
        (nelisp-ec-insert "hello")
        (emacs-buffer-set-buffer-local-value 'major-mode a 'text-mode))
      (nelisp-ec-with-current-buffer b
        (emacs-buffer-set-buffer-local-value 'major-mode b 'emacs-lisp-mode))
      (setq emacs-fileio--buffer-files (list (cons b "/tmp/beta.txt")))
      (let ((buf (emacs-buffer-ui-list-buffers)))
        (should (equal "*Buffer List*" (nelisp-ec-buffer-name buf)))
        (let ((text (nelisp-ec-with-current-buffer buf
                      (nelisp-ec-buffer-string))))
          (should (string-match-p "^name[[:space:]]+size[[:space:]]+mode[[:space:]]+file$" text))
          (should (string-match-p "^alpha[[:space:]]+5[[:space:]]+text-mode[[:space:]]*$" text))
          (should (string-match-p "^beta[[:space:]]+0[[:space:]]+emacs-lisp-mode[[:space:]]+/tmp/beta.txt$" text)))
        (should (= (nelisp-ec-with-current-buffer buf
                     (nelisp-ec-point))
                   (nelisp-ec-with-current-buffer buf
                     (nelisp-ec-point-min))))))))

(ert-deftest move-to-buffer-start-reports-display-position ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "display")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "hello")
        (nelisp-ec-goto-char (nelisp-ec-point-max)))
      (let ((result (emacs-buffer-ui-move-to-buffer-start buf)))
        (should (eq 'moved (plist-get result :status)))
        (should (eq buf (plist-get result :buffer)))
        (should (= (nelisp-ec-with-current-buffer buf
                     (nelisp-ec-point-min))
                   (plist-get result :point)))
        (should (= 0 (plist-get result :scroll-offset)))
        (should (= (nelisp-ec-with-current-buffer buf
                     (nelisp-ec-point-min))
                   (nelisp-ec-with-current-buffer buf
                     (nelisp-ec-point))))))))

(ert-deftest replace-text-buffer-creates-and-replaces-content ()
  (emacs-buffer-ui-test--with-fresh-world
    (let* ((result (emacs-buffer-ui-replace-text-buffer
                    "*Output*" "hello" t))
           (buf (plist-get result :buffer)))
      (should (eq 'replaced (plist-get result :status)))
      (should (equal "*Output*" (plist-get result :buffer-name)))
      (should (equal "hello\n" (plist-get result :text)))
      (should (equal 6 (plist-get result :length)))
      (should (equal "hello\n"
                     (nelisp-ec-with-current-buffer buf
                       (nelisp-ec-buffer-string))))
      (should (= (nelisp-ec-with-current-buffer buf
                   (nelisp-ec-point-min))
                 (nelisp-ec-with-current-buffer buf
                   (nelisp-ec-point))))
      (emacs-buffer-ui-replace-text-buffer "*Output*" "bye\n")
      (should (eq buf (emacs-buffer-ui-find-buffer "*Output*")))
      (should (equal "bye\n"
                     (nelisp-ec-with-current-buffer buf
                       (nelisp-ec-buffer-string)))))))

(ert-deftest run-list-buffers-command-uses-frontend-hooks ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "alpha"))
          synced emitted after)
      (nelisp-ec-set-buffer a)
      (let ((out (emacs-buffer-ui-run-list-buffers-command
                  :sync-window (lambda (buffer)
                                 (setq synced buffer))
                  :emit-text (lambda (text)
                               (setq emitted text))
                  :after-success (lambda (buffer)
                                   (setq after buffer)))))
        (should (equal "*Buffer List*"
                       (nelisp-ec-buffer-name out)))
        (should (eq synced out))
        (should (eq after out))
        (should (string-match-p "^name[[:space:]]+size"
                                emitted))))))

(ert-deftest list-buffers-renders-files-standalone-visited-file ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "standalone.txt")))
      (cl-letf (((symbol-function 'files--buffer-file-name)
                 (lambda (buffer)
                   (and (eq buffer buf) "/tmp/standalone.txt"))))
        (let ((out (emacs-buffer-ui-list-buffers)))
          (let ((text (nelisp-ec-with-current-buffer out
                        (nelisp-ec-buffer-string))))
            (should (string-match-p
                     "^standalone\\.txt[[:space:]]+0[[:space:]]+fundamental-mode[[:space:]]+/tmp/standalone\\.txt$"
                     text))))))))

(provide 'emacs-buffer-ui-test)

;;; emacs-buffer-ui-test.el ends here
