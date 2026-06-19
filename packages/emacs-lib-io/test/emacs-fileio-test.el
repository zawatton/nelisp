;;; emacs-fileio-test.el --- ERT for emacs-fileio  -*- lexical-binding: t; -*-

;;; Commentary:

;; M1 interactive file I/O tests for the higher-level layer in
;; `emacs-fileio.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-fileio)

(defvar emacs-fileio-test--tmp-counter 0)

(defun emacs-fileio-test--tmp-path (suffix)
  "Return a unique tmp path ending with SUFFIX."
  (setq emacs-fileio-test--tmp-counter
        (1+ emacs-fileio-test--tmp-counter))
  (format "/tmp/emacs-fileio-test-%d-%d-%s"
          (emacs-pid)
          emacs-fileio-test--tmp-counter
          suffix))

(defmacro emacs-fileio-test--with-fresh-world (&rest body)
  "Run BODY with clean buffer/fileio/mode state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil)
         (emacs-fileio--buffer-files nil)
         (emacs-fileio--buffer-default-directories nil)
         (emacs-fileio--buffer-major-modes nil)
         (emacs-fileio--buffer-mode-names nil)
         (emacs-fileio-gui-backend nil)
         (emacs-fileio-gui-arg "")
         (emacs-fileio-gui-status "ok")
         (emacs-fileio-gui-current-file-name nil)
         (emacs-fileio-gui-buffer-name "")
         (emacs-fileio-gui-read-only-p nil)
         (emacs-fileio-gui-display-action "")
         (auto-mode-alist nil)
         (default-directory "/tmp/")
         (major-mode 'fundamental-mode)
         (mode-name "Fundamental"))
     (unwind-protect
         (progn
           (when (fboundp 'emacs-mode-reset)
             (emacs-mode-reset))
           ,@body)
       (when (fboundp 'emacs-mode-reset)
         (emacs-mode-reset)))))

(defmacro emacs-fileio-test--with-temp-file (var content &rest body)
  "Bind VAR to a temp file seeded with CONTENT, then run BODY."
  (declare (indent 2) (debug (symbolp form body)))
  `(let ((,var (emacs-fileio-test--tmp-path "tmp.txt")))
     (unwind-protect
         (progn
           (with-temp-file ,var
             (insert ,content))
           ,@body)
       (when (file-exists-p ,var)
         (delete-file ,var)))))

(ert-deftest find-file-creates-buffer-and-loads-content ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "alpha\nbeta\n"
      (let ((buf (find-file path)))
        (should (eq buf (current-buffer)))
        (should (equal "alpha\nbeta\n" (with-current-buffer buf (buffer-string))))))))

(ert-deftest find-file-existing-buffer-switches-to-it ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "same"
      (let* ((other (generate-new-buffer "other"))
             (first (find-file path)))
        (set-buffer other)
        (should (eq first (find-file path)))
        (should (eq first (current-buffer)))))))

(ert-deftest find-file-sets-buffer-file-name ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "x"
      (let ((buf (find-file path)))
        (should (equal (expand-file-name path)
                       (buffer-file-name buf)))
        (should (equal (file-name-as-directory
                        (file-name-directory (expand-file-name path)))
                       default-directory))))))

(ert-deftest find-file-dispatches-major-mode-via-auto-mode-alist ()
  (emacs-fileio-test--with-fresh-world
    (let ((calls nil))
      (cl-letf (((symbol-function 'org-mode)
                 (lambda ()
                   (interactive)
                   (setq major-mode 'org-mode
                         mode-name "Org")
                   (push 'org-mode calls)
                   nil)))
        (let ((path (emacs-fileio-test--tmp-path "notes.org")))
          (unwind-protect
              (progn
                (with-temp-file path
                  (insert "* heading\n"))
                (find-file path)
                (should (eq 'org-mode major-mode))
                (should (equal '(org-mode) calls)))
            (when (file-exists-p path)
              (delete-file path))))))))

(ert-deftest find-file-noselect-does-not-switch-to-buffer ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "stay"
      (let ((current (generate-new-buffer "current")))
        (set-buffer current)
        (let ((buf (find-file-noselect path)))
          (should (eq current (current-buffer)))
          (should (not (eq current buf)))
          (should (equal "stay" (with-current-buffer buf (buffer-string)))))))))

(ert-deftest save-buffer-writes-back-to-disk ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "old"
      (let ((buf (find-file path)))
        (with-current-buffer buf
          (erase-buffer)
          (insert "new text")
          (should (buffer-modified-p))
          (save-buffer))
        (should (equal "new text"
                       (with-temp-buffer
                         (insert-file-contents path)
                         (buffer-string))))))))

(ert-deftest save-buffer-respects-utf-8 ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "before"
      (let ((buf (find-file path))
            (payload "日本語 café\n"))
        (with-current-buffer buf
          (erase-buffer)
          (insert payload)
          (save-buffer))
        (should (equal payload
                       (with-temp-buffer
                         (insert-file-contents path)
                         (buffer-string))))))))

(ert-deftest save-buffer-no-op-when-not-modified ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "steady"
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (find-file path)
          (should-not (buffer-modified-p))
          (should-not (save-buffer))
          (should (equal '("(No changes need to be saved)") messages)))))))

(ert-deftest write-file-changes-buffer-file-name-and-saves ()
  (emacs-fileio-test--with-fresh-world
    (let ((src (emacs-fileio-test--tmp-path "src.txt"))
          (dst (emacs-fileio-test--tmp-path "dst.org")))
      (unwind-protect
          (progn
            (with-temp-file src
              (insert "source"))
            (let ((buf (find-file src)))
              (with-current-buffer buf
                (erase-buffer)
                (insert "* moved\n")
                (write-file dst)
                (should (equal (expand-file-name dst) (buffer-file-name)))
                (should (eq 'fundamental-mode major-mode))
                (should
                 (equal "* moved\n"
                        (with-temp-buffer
                          (insert-file-contents dst)
                          (buffer-string)))))))
        (when (file-exists-p src)
          (delete-file src))
        (when (file-exists-p dst)
          (delete-file dst))))))

(ert-deftest emacs-fileio-global-key-bindings-installed ()
  ;; Re-install bindings explicitly so this test is robust against other
  ;; modules' fixtures (e.g. *--with-fresh-world*) that swap current-global-map.
  (emacs-fileio--ensure-global-bindings)
  (let ((map (current-global-map)))
    (should (eq #'find-file (lookup-key map (kbd "C-x C-f"))))
    (should (eq #'save-buffer (lookup-key map (kbd "C-x C-s"))))
    (should (eq #'write-file (lookup-key map (kbd "C-x C-w"))))))

(ert-deftest emacs-fileio-switch-rename-kill-buffer-runtime ()
  (emacs-fileio-test--with-fresh-world
    (let ((original (current-buffer))
          (buffer nil)
          (renamed nil))
      (unwind-protect
          (progn
            (setq buffer (switch-to-buffer "runtime-a"))
            (should (eq buffer (current-buffer)))
            (should (equal "runtime-a" (buffer-name buffer)))
            (insert "payload")
            (setq renamed (rename-buffer "runtime-b"))
            (should (equal "runtime-b" renamed))
            (should (equal "runtime-b" (buffer-name (current-buffer))))
            (should (eq buffer (switch-to-buffer "runtime-b")))
            (should (kill-buffer "runtime-b"))
            (should-not (get-buffer "runtime-b")))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (when (buffer-live-p original)
          (set-buffer original))))))

(ert-deftest emacs-fileio-list-buffers-runtime ()
  (emacs-fileio-test--with-fresh-world
    (let ((original (current-buffer))
          (first nil)
          (second nil)
          (list-buffer nil))
      (unwind-protect
          (progn
            (setq first (switch-to-buffer "runtime-list-a"))
            (insert "a")
            (setq second (switch-to-buffer "runtime-list-b"))
            (insert "b")
            (setq list-buffer (list-buffers))
            (should (equal "*Buffer List*" (buffer-name list-buffer)))
            (let ((text (with-current-buffer list-buffer
                          (buffer-string))))
              (should (string-match-p "runtime-list-a" text))
              (should (string-match-p "runtime-list-b" text))))
        (dolist (buffer (list first second list-buffer))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
        (when (buffer-live-p original)
          (set-buffer original))))))

(ert-deftest emacs-fileio-gui-find-file-backend ()
  (emacs-fileio-test--with-fresh-world
    (let ((calls nil))
      (emacs-fileio-gui-register-backend
       :find-file-core
       (lambda (arg)
         (push (list :find arg) calls)
         "/tmp/demo.txt")
       :current-file-name
       (lambda ()
         "/tmp/demo.txt")
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls))
       :set-read-only
       (lambda (flag)
         (push (list :read-only flag) calls)))
      (emacs-fileio-gui-set-context :arg "/tmp/demo.txt" :status "ok")
      (should (equal "/tmp/demo.txt"
                     (emacs-fileio-gui-find-file "other" t)))
      (should (equal "/tmp/demo.txt" emacs-fileio-gui-current-file-name))
      (should emacs-fileio-gui-read-only-p)
      (should (equal '((:read-only t)
                       (:display "other")
                      (:find "/tmp/demo.txt"))
                     calls)))))

(ert-deftest emacs-fileio-gui-current-context-refresh ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-gui-register-backend
     :current-arg (lambda () "/tmp/current.txt")
     :current-status (lambda () "pending")
     :current-file-name (lambda () "/tmp/visited.txt")
     :buffer-name (lambda () "notes")
     :current-read-only-p (lambda () t)
     :current-display-action (lambda () "other"))
    (should (equal
             '(:arg "/tmp/current.txt"
               :status "pending"
               :current-file-name "/tmp/visited.txt"
               :buffer-name "notes"
               :read-only-p t
               :display-action "other")
             (emacs-fileio-gui-refresh-context-from-backend)))
    (should (equal "/tmp/current.txt" emacs-fileio-gui-arg))
    (should (equal "pending" emacs-fileio-gui-status))
    (should (equal "/tmp/visited.txt"
                   emacs-fileio-gui-current-file-name))
    (should (equal "notes" emacs-fileio-gui-buffer-name))
    (should emacs-fileio-gui-read-only-p)
    (should (equal "other" emacs-fileio-gui-display-action))))

(ert-deftest emacs-fileio-gui-command-spec-normalizes-variants ()
  (emacs-fileio-test--with-fresh-world
    (dolist (case '((find-file
                     (:command find-file :action "same" :read-only nil))
                    (find-file-other-window
                     (:command find-file :action "other" :read-only nil))
                    (find-file-other-frame
                     (:command find-file :action "frame" :read-only nil))
                    (find-file-other-tab
                     (:command find-file :action "tab" :read-only nil))
                    (find-file-read-only
                     (:command find-file :action "same" :read-only t))
                    (find-file-read-only-other-window
                     (:command find-file :action "other" :read-only t))
                    (basic-save-buffer
                     (:command save-buffer :action nil :read-only nil))
                    (revert-buffer-quick
                     (:command revert-buffer :action nil :read-only nil))
                    (project-switch-to-buffer
                     (:command switch-to-buffer :action "same" :read-only nil))
                    (switch-to-buffer-other-frame
                     (:command switch-to-buffer :action "frame" :read-only nil))
                    (switch-to-buffer-other-tab
                     (:command switch-to-buffer :action "tab" :read-only nil))
                    (display-buffer
                     (:command display-buffer :action "other" :read-only nil))
                    (display-buffer-other-frame
                     (:command display-buffer :action "frame" :read-only nil))
                    (project-kill-buffers
                     (:command project-kill-buffers
                      :action nil :read-only nil))))
      (should (equal (cadr case)
                     (emacs-fileio-gui-command-spec (car case)))))
    (should-not (emacs-fileio-gui-command-spec 'forward-char))))

(ert-deftest emacs-fileio-gui-current-context-command-helpers ()
  (emacs-fileio-test--with-fresh-world
    (let ((calls nil)
          (arg "alpha")
          (current-buffer "main"))
      (emacs-fileio-gui-register-backend
       :current-arg (lambda () arg)
       :current-status (lambda () "ok")
       :current-file-name (lambda () arg)
       :buffer-name (lambda () current-buffer)
       :find-file-core (lambda (file)
                         (push (list :find file) calls)
                         file)
       :save-buffer (lambda ()
                      (push (list :save emacs-fileio-gui-arg) calls)
                      emacs-fileio-gui-arg)
       :save-some-buffers (lambda ()
                             (push (list :save-some emacs-fileio-gui-arg) calls)
                             t)
       :write-file (lambda (file)
                     (push (list :write file) calls)
                     file)
       :insert-file (lambda (file)
                      (push (list :insert-file file) calls)
                      file)
       :insert-buffer (lambda (name)
                        (push (list :insert-buffer name) calls)
                        name)
       :revert-buffer (lambda ()
                        (push (list :revert emacs-fileio-gui-arg) calls)
                        emacs-fileio-gui-arg)
       :switch-to-buffer (lambda (name)
                           (push (list :switch name) calls)
                           (setq current-buffer name)
                           name)
       :display-buffer (lambda (name)
                         (push (list :display-buffer name) calls)
                         (setq current-buffer name)
                         name)
       :rename-buffer (lambda (name)
                        (push (list :rename name) calls)
                        (setq current-buffer name)
                        name)
       :kill-buffer (lambda (name)
                      (push (list :kill name) calls)
                      (setq current-buffer "main")
                      "main")
       :kill-buffer-and-window (lambda (name)
                                 (push (list :kill-window name) calls)
                                 (setq current-buffer "main")
                                 "main")
       :show-buffer-list (lambda (text)
                           (push (list :list text) calls)
                           (setq current-buffer "*Buffer List*")
                           "*Buffer List*")
       :project-kill-buffers (lambda ()
                               (push (list :project-kill emacs-fileio-gui-arg) calls)
                               (setq current-buffer "main")
                               "main")
       :buffer-list-source (lambda () "alpha\nmain\n")
       :buffer-file-name (lambda (_name) "")
       :apply-display-prefix (lambda (action)
                               (push (list :display action) calls)))
      (should (equal "alpha"
                     (emacs-fileio-gui-find-file-current-context-command
                      "same" nil)))
      (setq arg "beta")
      (should (equal "beta"
                     (emacs-fileio-gui-save-buffer-current-context-command)))
      (setq arg "gamma")
      (should (equal "gamma"
                     (emacs-fileio-gui-switch-to-buffer-current-context-command
                      "other")))
      (setq arg "delta")
      (should (equal "main"
                     (emacs-fileio-gui-kill-buffer-current-context-command)))
      (setq arg "epsilon")
      (should (equal "epsilon"
                     (emacs-fileio-gui-rename-buffer-current-context-command)))
      (setq arg "zeta")
      (should (equal "zeta"
                     (emacs-fileio-gui-revert-buffer-current-context-command)))
      (should (equal "*Buffer List*"
                     (emacs-fileio-gui-list-buffers-current-context-command)))
      (should (equal '((:list "Buffer\tFile\n  alpha\t\n  main\t\n")
                       (:revert "zeta")
                       (:rename "epsilon")
                       (:kill "delta")
                       (:display "other")
                       (:switch "gamma")
                       (:save "beta")
                       (:display "same")
                       (:find "alpha"))
                     calls)))))

(ert-deftest emacs-fileio-gui-current-context-command-dispatcher ()
  (emacs-fileio-test--with-fresh-world
    (let ((calls nil)
          (arg "alpha")
          (current-buffer "main"))
      (emacs-fileio-gui-register-backend
       :current-arg (lambda () arg)
       :current-status (lambda () "ok")
       :current-file-name (lambda () arg)
       :buffer-name (lambda () current-buffer)
       :find-file-core (lambda (file)
                         (push (list :find file) calls)
                         file)
       :save-buffer (lambda ()
                      (push (list :save emacs-fileio-gui-arg) calls)
                      emacs-fileio-gui-arg)
       :save-some-buffers (lambda ()
                             (push (list :save-some emacs-fileio-gui-arg) calls)
                             t)
       :write-file (lambda (file)
                     (push (list :write file) calls)
                     file)
       :insert-file (lambda (file)
                      (push (list :insert-file file) calls)
                      file)
       :insert-buffer (lambda (name)
                        (push (list :insert-buffer name) calls)
                        name)
       :revert-buffer (lambda ()
                        (push (list :revert emacs-fileio-gui-arg) calls)
                        emacs-fileio-gui-arg)
       :switch-to-buffer (lambda (name)
                           (push (list :switch name) calls)
                           (setq current-buffer name)
                           name)
       :rename-buffer (lambda (name)
                        (push (list :rename name) calls)
                        (setq current-buffer name)
                        name)
       :kill-buffer (lambda (name)
                      (push (list :kill name) calls)
                      (setq current-buffer "main")
                      "main")
       :kill-buffer-and-window (lambda (name)
                                 (push (list :kill-window name) calls)
                                 (setq current-buffer "main")
                                 "main")
       :show-buffer-list (lambda (text)
                           (push (list :list text) calls)
                           (setq current-buffer "*Buffer List*")
                           "*Buffer List*")
       :project-kill-buffers (lambda ()
                               (push (list :project-kill emacs-fileio-gui-arg) calls)
                               (setq current-buffer "main")
                               "main")
       :buffer-list-source (lambda () "alpha\nmain\n")
       :buffer-file-name (lambda (_name) "")
       :apply-display-prefix (lambda (action)
                               (push (list :display action) calls)))
      (should (equal "alpha"
                     (emacs-fileio-gui-current-context-command
                      'find-file "same" nil)))
      (setq arg "beta")
      (should (equal "beta"
                     (emacs-fileio-gui-current-context-command
                      'save-buffer)))
      (should (equal t
                     (emacs-fileio-gui-current-context-command
                      'save-some-buffers)))
      (setq arg "write.txt")
      (should (equal "write.txt"
                     (emacs-fileio-gui-current-context-command
                      'write-file)))
      (setq arg "insert.txt")
      (should (equal "insert.txt"
                     (emacs-fileio-gui-current-context-command
                      'insert-file)))
      (setq arg "alpha")
      (should (equal "alpha"
                     (emacs-fileio-gui-current-context-command
                      'insert-buffer)))
      (setq arg "revert.txt")
      (should (equal "revert.txt"
                     (emacs-fileio-gui-current-context-command
                      'revert-buffer)))
      (setq arg "gamma")
      (should (equal "gamma"
                     (emacs-fileio-gui-current-context-command
                      'switch-to-buffer "other")))
      (setq arg "display")
      (should (equal "display"
                     (emacs-fileio-gui-current-context-command
                      'display-buffer "frame")))
      (setq arg "renamed")
      (should (equal "renamed"
                     (emacs-fileio-gui-current-context-command
                      'rename-buffer)))
      (setq arg "delta")
      (should (equal "main"
                     (emacs-fileio-gui-current-context-command
                      'kill-buffer)))
      (setq arg "epsilon")
      (should (equal "main"
                     (emacs-fileio-gui-current-context-command
                      'kill-buffer-and-window)))
      (should (equal "*Buffer List*"
                     (emacs-fileio-gui-current-context-command
                      'list-buffers)))
      (should (equal "*Buffer List*"
                     (emacs-fileio-gui-current-context-command
                      'project-list-buffers)))
      (should (equal "main"
                     (emacs-fileio-gui-current-context-command
                      'project-kill-buffers)))
      (should (equal nil
                     (emacs-fileio-gui-current-context-command
                      'unknown-fileio-command)))
      (should (equal '((:project-kill "epsilon")
                       (:list "Buffer\tFile\n")
                       (:list "Buffer\tFile\n  alpha\t\n* main\t\n")
                       (:kill-window "epsilon")
                       (:kill "delta")
                       (:rename "renamed")
                       (:display "frame")
                       (:switch "display")
                       (:display "other")
                       (:switch "gamma")
                       (:revert "revert.txt")
                       (:insert-buffer "alpha")
                       (:insert-file "insert.txt")
                       (:write "write.txt")
                       (:save-some "beta")
                       (:save "beta")
                       (:display "same")
                       (:find "alpha"))
                     calls)))))

(ert-deftest emacs-fileio-gui-project-find-current-context-commands ()
  (emacs-fileio-test--with-fresh-world
    (let ((calls nil)
          (arg "nested/project.txt")
          (current-file nil)
          (project-dir "/tmp/project/sub")
          (existing "/tmp/project/sub/nested/project.txt")
          (external "/tmp/external.txt"))
      (emacs-fileio-gui-register-backend
       :current-arg (lambda () arg)
       :current-status (lambda () "ok")
       :current-file-name (lambda () current-file)
       :project-directory (lambda () project-dir)
       :file-exists-p (lambda (file) (equal file existing))
       :find-file-core
       (lambda (file)
         (setq current-file file)
         (push (list :find file) calls)
         file)
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (should (equal existing
                     (emacs-fileio-gui-current-context-command
                      'project-find-file)))
      (should (equal "nested/project.txt" emacs-fileio-gui-arg))
      (setq arg "nested/project.txt")
      (should (equal existing
                     (emacs-fileio-gui-current-context-command
                      'project-or-external-find-file)))
      (should (equal "nested/project.txt" emacs-fileio-gui-arg))
      (setq arg external)
      (should (equal external
                     (emacs-fileio-gui-current-context-command
                      'project-or-external-find-file)))
      (should (equal external emacs-fileio-gui-arg))
      (setq arg "/tmp/alternate.txt")
      (should (equal "/tmp/alternate.txt"
                     (emacs-fileio-gui-current-context-command
                      'find-alternate-file)))
      (should (equal '((:display "same")
                       (:find "/tmp/alternate.txt")
                       (:display "same")
                       (:find "/tmp/external.txt")
                       (:display "same")
                       (:find "/tmp/project/sub/nested/project.txt")
                       (:display "same")
                       (:find "/tmp/project/sub/nested/project.txt"))
                     calls)))))

(ert-deftest emacs-fileio-gui-save-and-write-backend ()
  (emacs-fileio-test--with-fresh-world
    (let ((calls nil)
          (current "/tmp/current.txt"))
      (emacs-fileio-gui-register-backend
       :save-buffer
       (lambda ()
         (push :save calls)
         current)
       :save-some-buffers
       (lambda ()
         (push :save-some calls)
         t)
       :write-file
       (lambda (arg)
         (push (list :write arg) calls)
         (setq current arg))
       :insert-file
       (lambda (arg)
         (push (list :insert arg) calls)
         arg)
       :insert-buffer
       (lambda (arg)
         (push (list :insert-buffer arg) calls)
         arg)
       :current-file-name
       (lambda ()
         current))
      (emacs-fileio-gui-set-context :arg "/tmp/new.txt"
                                    :current-file-name current)
      (should (equal "/tmp/current.txt" (emacs-fileio-gui-save-buffer)))
      (should (eq t (emacs-fileio-gui-save-some-buffers)))
      (should (equal "/tmp/new.txt" (emacs-fileio-gui-write-file)))
      (should (equal "/tmp/new.txt" (emacs-fileio-gui-insert-file)))
      (emacs-fileio-gui-set-context :arg "")
      (should (equal "main" (emacs-fileio-gui-insert-buffer)))
      (should (equal "/tmp/new.txt" emacs-fileio-gui-current-file-name))
      (should (equal '((:insert-buffer "main")
                       (:insert "/tmp/new.txt")
                       (:write "/tmp/new.txt")
                       :save-some
                       :save)
                     calls)))))

(ert-deftest emacs-fileio-gui-switch-to-buffer-backend ()
  (emacs-fileio-test--with-fresh-world
    (let ((calls nil))
      (emacs-fileio-gui-register-backend
       :switch-to-buffer
       (lambda (arg)
         (push (list :switch arg) calls)
         "*Messages*")
       :buffer-name
       (lambda ()
         "*Messages*")
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (emacs-fileio-gui-set-context :arg "*Messages*")
      (should (equal "*Messages*"
                     (emacs-fileio-gui-switch-to-buffer "same")))
      (should (equal "*Messages*" emacs-fileio-gui-buffer-name))
      (should (equal '((:display "same")
                       (:switch "*Messages*"))
                     calls)))))

(ert-deftest emacs-fileio-gui-buffer-lifecycle-backend ()
  (emacs-fileio-test--with-fresh-world
    (let ((calls nil)
          (buffer-name "main")
          (current-file "/tmp/main.txt"))
      (emacs-fileio-gui-register-backend
       :revert-buffer
       (lambda ()
         (push :revert calls)
         current-file)
       :rename-buffer
       (lambda (arg)
         (push (list :rename arg) calls)
         (setq buffer-name arg))
       :kill-buffer
       (lambda (arg)
         (push (list :kill arg) calls)
         (setq buffer-name "main"))
       :kill-buffer-and-window
       (lambda (arg)
         (push (list :kill-window arg) calls)
         (setq buffer-name "main"))
       :list-buffers
       (lambda ()
         (push :list calls)
         (setq buffer-name "*Buffer List*"))
       :project-list-buffers
       (lambda ()
         (push :project-list calls)
         (setq buffer-name "*Buffer List*"))
       :project-kill-buffers
       (lambda ()
         (push :project-kill calls)
         (setq buffer-name "outside"))
       :current-file-name
       (lambda ()
         current-file)
       :buffer-name
       (lambda ()
         buffer-name))
      (emacs-fileio-gui-set-context :arg "renamed"
                                    :buffer-name buffer-name
                                    :current-file-name current-file)
      (should (equal current-file (emacs-fileio-gui-revert-buffer)))
      (should (equal "renamed" (emacs-fileio-gui-rename-buffer)))
      (emacs-fileio-gui-set-context :arg "renamed")
      (should (equal "main" (emacs-fileio-gui-kill-buffer)))
      (should (equal "main" (emacs-fileio-gui-kill-buffer-and-window)))
      (should (equal "*Buffer List*" (emacs-fileio-gui-list-buffers)))
      (should (equal "*Buffer List*" (emacs-fileio-gui-project-list-buffers)))
      (should (equal "outside" (emacs-fileio-gui-project-kill-buffers)))
      (should (equal '(:project-kill
                       :project-list
                       :list
                       (:kill-window "renamed")
                       (:kill "renamed")
                       (:rename "renamed")
                       :revert)
                     calls)))))

(ert-deftest emacs-fileio-gui-command-variants ()
  (emacs-fileio-test--with-fresh-world
    (let ((calls nil)
          (buffer-name "main")
          (current-file "/tmp/main.txt"))
      (emacs-fileio-gui-register-backend
       :find-file-core
       (lambda (arg)
         (push (list :find arg) calls)
         (setq current-file arg))
       :save-buffer
       (lambda ()
         (push :save calls)
         current-file)
       :save-some-buffers
       (lambda ()
         (push :save-some calls)
         t)
       :write-file
       (lambda (arg)
         (push (list :write arg) calls)
         (setq current-file arg))
       :insert-file
       (lambda (arg)
         (push (list :insert arg) calls)
         arg)
       :insert-buffer
       (lambda (arg)
         (push (list :insert-buffer arg) calls)
         arg)
       :revert-buffer
       (lambda ()
         (push :revert calls)
         current-file)
       :switch-to-buffer
       (lambda (arg)
         (push (list :switch arg) calls)
         (setq buffer-name arg))
       :rename-buffer
       (lambda (arg)
         (push (list :rename arg) calls)
         (setq buffer-name arg))
       :kill-buffer
       (lambda (arg)
         (push (list :kill arg) calls)
         (setq buffer-name "main"))
       :kill-buffer-and-window
       (lambda (arg)
         (push (list :kill-window arg) calls)
         (setq buffer-name "main"))
       :list-buffers
       (lambda ()
         (push :list calls)
         (setq buffer-name "*Buffer List*"))
       :project-list-buffers
       (lambda ()
         (push :project-list calls)
         (setq buffer-name "*Buffer List*"))
       :project-kill-buffers
       (lambda ()
         (push :project-kill calls)
         (setq buffer-name "outside"))
       :current-file-name
       (lambda ()
         current-file)
       :buffer-name
       (lambda ()
         buffer-name)
       :set-read-only
       (lambda (flag)
         (push (list :read-only flag) calls))
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (emacs-fileio-gui-set-context :arg "/tmp/ro.txt"
                                    :status "ok"
                                    :current-file-name current-file)
      (should (equal "/tmp/ro.txt"
                     (emacs-fileio-gui-find-file-read-only-command
                      "frame")))
      (emacs-fileio-gui-set-context :arg "/tmp/write.txt")
      (should (equal current-file
                     (emacs-fileio-gui-save-buffer-command)))
      (should (eq t (emacs-fileio-gui-save-some-buffers-command)))
      (should (equal "/tmp/write.txt"
                     (emacs-fileio-gui-write-file-command)))
      (should (equal "/tmp/write.txt"
                     (emacs-fileio-gui-insert-file-command)))
      (emacs-fileio-gui-set-context :arg "main")
      (should (equal "main"
                     (emacs-fileio-gui-insert-buffer-command)))
      (should (equal "/tmp/write.txt"
                     (emacs-fileio-gui-revert-buffer-command)))
      (emacs-fileio-gui-set-context :arg "*Messages*")
      (should (equal "*Messages*"
                     (emacs-fileio-gui-switch-to-buffer-command "tab")))
      (emacs-fileio-gui-set-context :arg "*Warnings*")
      (should (equal "*Warnings*"
                     (emacs-fileio-gui-display-buffer-command)))
      (emacs-fileio-gui-set-context :arg "renamed")
      (should (equal "renamed"
                     (emacs-fileio-gui-rename-buffer-command)))
      (emacs-fileio-gui-set-context :arg "renamed")
      (should (equal "main"
                     (emacs-fileio-gui-kill-buffer-command)))
      (should (equal "main"
                     (emacs-fileio-gui-kill-buffer-and-window-command)))
      (should (equal "*Buffer List*"
                     (emacs-fileio-gui-list-buffers-command)))
      (should (equal "*Buffer List*"
                     (emacs-fileio-gui-project-list-buffers-command)))
      (should (equal "outside"
                     (emacs-fileio-gui-project-kill-buffers-command)))
      (should (equal '(:project-kill
                       :project-list
                       :list
                       (:kill-window "renamed")
                       (:kill "renamed")
                       (:rename "renamed")
                       (:display "other")
                       (:switch "*Warnings*")
                       (:display "tab")
                       (:switch "*Messages*")
                       :revert
                       (:insert-buffer "main")
                       (:insert "/tmp/write.txt")
                       (:write "/tmp/write.txt")
                       :save-some
                       :save
                       (:read-only t)
                       (:display "frame")
                       (:find "/tmp/ro.txt"))
                     calls)))))

(ert-deftest emacs-fileio-gui-buffer-candidates-and-project-filter ()
  (emacs-fileio-test--with-fresh-world
    (let ((files '(("main" . "/tmp/proj/main.txt")
                   ("other" . "/tmp/else/other.txt")
                   ("proj-notes" . "/tmp/proj/notes.org"))))
      (emacs-fileio-gui-register-backend
       :buffer-list-source
       (lambda ()
         "main\nother\nproj-notes\n")
       :buffer-file-name
       (lambda (name)
         (or (cdr (assoc name files)) ""))
       :project-directory
       (lambda ()
         "/tmp/proj"))
      (should (equal "main\nother\nproj-notes\n"
                     (emacs-fileio-gui-buffer-candidates)))
      (should (equal "main\nproj-notes\n"
                     (emacs-fileio-gui-project-buffer-candidates))))))

(ert-deftest emacs-fileio-gui-buffer-list-render-and-show ()
  (emacs-fileio-test--with-fresh-world
    (let ((shown nil)
          (buffer-name "main")
          (files '(("main" . "/tmp/proj/main.txt")
                   ("other" . "/tmp/else/other.txt")
                   ("proj-notes" . "/tmp/proj/notes.org"))))
      (emacs-fileio-gui-register-backend
       :buffer-list-source
       (lambda ()
         "main\nother\nproj-notes\n")
       :buffer-file-name
       (lambda (name)
         (or (cdr (assoc name files)) ""))
       :project-directory
       (lambda ()
         "/tmp/proj")
       :buffer-name
       (lambda ()
         buffer-name)
       :show-buffer-list
       (lambda (text)
         (setq shown text)
         (setq buffer-name "*Buffer List*")))
      (should (equal "*Buffer List*"
                     (emacs-fileio-gui-list-buffers-command)))
      (should (string-match-p "\\* main\t/tmp/proj/main.txt" shown))
      (should (string-match-p "  other\t/tmp/else/other.txt" shown))
      (setq shown nil
            buffer-name "main")
      (should (equal "*Buffer List*"
                     (emacs-fileio-gui-project-list-buffers-command)))
      (should (string-match-p "\\* main\t/tmp/proj/main.txt" shown))
      (should (string-match-p "  proj-notes\t/tmp/proj/notes.org" shown))
      (should-not (string-match-p "other\t/tmp/else/other.txt" shown)))))

(ert-deftest emacs-fileio-gui-writeback-spec ()
  (emacs-fileio-test--with-fresh-world
    (should (equal '(:buffer t :file t :read-only t :window t :point t)
                   (emacs-fileio-gui-writeback-spec 'find-file "ok")))
    (should (equal '(:buffer t :file t :read-only t
                     :window t :frame t :point t)
                   (emacs-fileio-gui-writeback-spec
                    "find-file-other-frame" "ok")))
    (should (equal '(:buffer t :file t :read-only t
                     :window t :tab t :point t)
                   (emacs-fileio-gui-writeback-spec
                    "find-file-read-only-other-tab" "ok")))
    (should (equal '(:buffer t :point t :mark t)
                   (emacs-fileio-gui-writeback-spec 'insert-file "ok")))
    (should (equal '(:file t :point t)
                   (emacs-fileio-gui-writeback-spec 'write-file "ok")))
    (should (equal '(:file t :point t)
                   (emacs-fileio-gui-writeback-spec 'save-buffer "ok")))
    (should-not
     (emacs-fileio-gui-writeback-spec 'save-buffer "permission-denied"))
    (should (equal '(:buffer t :file t :buffer-name t :window t
                     :point t :mark t :window-start t)
                   (emacs-fileio-gui-writeback-spec
                    'switch-to-buffer "ok")))
    (should (equal '(:buffer t :file t :buffer-name t :window t :frame t
                     :point t :mark t :window-start t)
                   (emacs-fileio-gui-writeback-spec
                    'display-buffer-other-frame "ok")))
    (should (equal '(:buffer t :file t :buffer-name t :window t :tab t
                     :point t :mark t :window-start t)
                   (emacs-fileio-gui-writeback-spec
                    'switch-to-buffer-other-tab "ok")))
    (should-not (emacs-fileio-gui-writeback-spec 'dired "ok"))))

(ert-deftest emacs-fileio-gui-writeback-spec-flag ()
  (emacs-fileio-test--with-fresh-world
    (let ((spec (emacs-fileio-gui-writeback-spec
                 'switch-to-buffer-other-frame "ok")))
      (should (emacs-fileio-gui-writeback-spec-flag spec :buffer))
      (should (emacs-fileio-gui-writeback-spec-flag spec :frame))
      (should-not (emacs-fileio-gui-writeback-spec-flag spec :tab))
      (should-not (emacs-fileio-gui-writeback-spec-flag nil :buffer)))))

(ert-deftest emacs-fileio-gui-writeback-state ()
  (emacs-fileio-test--with-fresh-world
    (let (calls)
      (emacs-fileio-gui-register-backend
       :write-buffer-state (lambda () (push :buffer calls))
       :write-file-state (lambda () (push :file calls))
       :write-buffer-name-state (lambda () (push :buffer-name calls))
       :write-read-only-state (lambda () (push :read-only calls))
       :write-window-state (lambda () (push :window calls))
       :write-frame-state (lambda () (push :frame calls))
       :write-tab-state (lambda () (push :tab calls))
       :write-point-state (lambda () (push :point calls))
       :write-mark-state (lambda () (push :mark calls))
       :write-window-start-state (lambda () (push :window-start calls))
       :mark-written-state (lambda () (push :written calls)))
      (should (emacs-fileio-gui-writeback-state
               'switch-to-buffer-other-frame "ok"))
      (should (equal '(:buffer :file :buffer-name :window :frame
                       :point :mark :window-start :written)
                     (nreverse calls)))
      (setq calls nil)
      (should-not (emacs-fileio-gui-writeback-state
                   'save-buffer "permission-denied"))
      (should-not calls))))

(ert-deftest emacs-fileio-gui-switch-buffer-low-level-backend ()
  (emacs-fileio-test--with-fresh-world
    (let ((calls nil)
          (buffer-name "main"))
      (emacs-fileio-gui-register-backend
       :low-level-buffer-backend-p
       (lambda () t)
       :save-current-buffer-state
       (lambda () (push :save-current calls))
       :load-buffer-state
       (lambda (name)
         (push (list :load name) calls)
         (setq buffer-name name)
         name)
       :buffer-name
       (lambda () buffer-name)
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (emacs-fileio-gui-set-context :arg "notes")
      (should (equal "notes"
                     (emacs-fileio-gui-switch-to-buffer-command "other")))
      (should (equal '((:display "other")
                       (:load "notes")
                       :save-current)
                     calls))
      (setq calls nil
            buffer-name "notes")
      (emacs-fileio-gui-set-context :arg "")
      (should (equal "main"
                     (emacs-fileio-gui-switch-to-buffer-command "same")))
      (should (equal '((:display "same")
                       (:load "main")
                       :save-current)
                     calls)))))

(ert-deftest emacs-fileio-gui-kill-buffer-low-level-backend ()
  (emacs-fileio-test--with-fresh-world
    (let ((calls nil)
          (buffer-name "notes")
          (buffers '("main" "notes")))
      (emacs-fileio-gui-register-backend
       :low-level-buffer-backend-p
       (lambda () t)
       :buffer-list-source
       (lambda ()
         (mapconcat #'identity buffers "\n"))
       :buffer-name
       (lambda () buffer-name)
       :remove-buffer
       (lambda (name)
         (push (list :remove name) calls)
         (setq buffers (delete name buffers)))
       :clear-buffer-state
       (lambda (name)
         (push (list :clear name) calls))
       :add-buffer
       (lambda (name)
         (push (list :add name) calls)
         (unless (member name buffers)
           (setq buffers (append buffers (list name)))))
       :load-buffer-state
       (lambda (name)
         (push (list :load name) calls)
         (setq buffer-name name)
         name))
      (emacs-fileio-gui-set-context :arg "")
      (should (equal "main" (emacs-fileio-gui-kill-buffer-command)))
      (should (equal '((:load "main")
                       (:clear "notes")
                       (:remove "notes"))
                     calls))
      (should (equal '("main") buffers)))))

(ert-deftest emacs-fileio-gui-project-kill-buffers-low-level-backend ()
  (emacs-fileio-test--with-fresh-world
    (let ((calls nil)
          (buffer-name "main")
          (buffers '("main" "other" "proj"))
          (files '(("main" . "/tmp/proj/main.txt")
                   ("other" . "/tmp/else/other.txt")
                   ("proj" . "/tmp/proj/proj.txt"))))
      (emacs-fileio-gui-register-backend
       :low-level-buffer-backend-p
       (lambda () t)
       :buffer-list-source
       (lambda ()
         (mapconcat #'identity buffers "\n"))
       :buffer-file-name
       (lambda (name)
         (or (cdr (assoc name files)) ""))
       :project-directory
       (lambda () "/tmp/proj")
       :buffer-name
       (lambda () buffer-name)
       :save-current-buffer-state
       (lambda () (push :save-current calls))
       :remove-buffer
       (lambda (name)
         (push (list :remove name) calls)
         (setq buffers (delete name buffers)))
       :clear-buffer-state
       (lambda (name)
         (push (list :clear name) calls))
       :load-buffer-state
       (lambda (name)
         (push (list :load name) calls)
         (setq buffer-name name)
         name))
      (should (equal "other"
                     (emacs-fileio-gui-project-kill-buffers-command)))
      (should (equal "other" buffer-name))
      (should (equal '("other") buffers))
      (should (equal '((:load "other")
                       (:clear "proj")
                       (:remove "proj")
                       (:clear "main")
                       (:remove "main")
                       :save-current)
                     calls)))))

(provide 'emacs-fileio-test)

;;; emacs-fileio-test.el ends here
