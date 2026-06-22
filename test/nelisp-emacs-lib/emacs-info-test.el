;;; emacs-info-test.el --- ERT for emacs-info  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-info)

(defmacro emacs-info-test--with-fresh-world (&rest body)
  "Run BODY with clean Info GUI backend state."
  (declare (indent 0) (debug (body)))
  `(let ((emacs-info-gui-backend nil)
         (emacs-info-gui-arg "")
         (emacs-info-gui-status "ok")
         (emacs-info-gui-buffer-name "")
         (emacs-info-gui-file "")
         (emacs-info-gui-node "")
         (emacs-info-gui-scan-cap 65536))
     ,@body))

(defconst emacs-info-test--fixture
  (concat
   "Preamble.\n"
   "\037\nFile: fixture.info,  Node: Top,  Next: First,  Up: (dir)\n"
   "\nTop node body line.\n"
   "\037\nFile: fixture.info,  Node: First,  Next: Second,  Prev: Top,  Up: Top\n"
   "\nFirst node body line.\n"
   "\037\nFile: fixture.info,  Node: Second,  Prev: First,  Up: Top\n"
   "\nSecond node body line.\n"))

(ert-deftest emacs-info-gui-header-field-parses-info-header ()
  (should (equal "First"
                 (emacs-info-gui-header-field
                  "File: f,  Node: Top,  Next: First,  Up: (dir)"
                  "Next: ")))
  (should (equal ""
                 (emacs-info-gui-header-field
                  "File: f,  Node: Top"
                  "Prev: "))))

(ert-deftest emacs-info-gui-info-renders-top-node ()
  (emacs-info-test--with-fresh-world
    (let ((calls nil)
          (rendered nil))
      (emacs-info-gui-register-backend
       :file-exists-p
       (lambda (path)
         (equal path "/tmp/fixture.info"))
       :read-file
       (lambda (_path)
         emacs-info-test--fixture)
       :show-info-buffer
       (lambda (title body)
         (setq rendered (list title body))
         "*info*")
       :buffer-name
       (lambda ()
         "*info*")
       :write-state
       (lambda (file node)
         (push (list :state file node) calls))
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (emacs-info-gui-set-context :arg "/tmp/fixture.info")
      (should (equal "*info*" (emacs-info-gui-info "same")))
      (should (equal "/tmp/fixture.info" emacs-info-gui-file))
      (should (equal "Top" emacs-info-gui-node))
      (should (string-match-p "Node: Top" (car rendered)))
      (should (string-match-p "Top node body line" (car rendered)))
      (should (equal '((:display "same")
                       (:state "/tmp/fixture.info" "Top"))
                     calls)))))

(ert-deftest emacs-info-gui-info-core-does-not-apply-display-prefix ()
  (emacs-info-test--with-fresh-world
    (let ((calls nil)
          (rendered nil))
      (emacs-info-gui-register-backend
       :file-exists-p
       (lambda (path)
         (equal path "/tmp/fixture.info"))
       :read-file
       (lambda (_path)
         emacs-info-test--fixture)
       :show-info-buffer
       (lambda (title body)
         (setq rendered (list title body))
         "*info*")
       :buffer-name
       (lambda ()
         "*info*")
       :write-state
       (lambda (file node)
         (push (list :state file node) calls))
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (emacs-info-gui-set-context :arg "/tmp/fixture.info")
      (should (equal "*info*" (emacs-info-gui-info-core)))
      (should (string-match-p "Node: Top" (car rendered)))
      (should (equal '((:state "/tmp/fixture.info" "Top")) calls)))))

(ert-deftest emacs-info-gui-refresh-context-from-backend ()
  (emacs-info-test--with-fresh-world
    (emacs-info-gui-register-backend
     :current-arg (lambda () "/tmp/fixture.info")
     :current-status (lambda () "ok")
     :buffer-name (lambda () "*info*")
     :current-file (lambda () "/tmp/current.info")
     :current-node (lambda () "Top"))
    (should (equal '(:arg "/tmp/fixture.info"
                     :status "ok"
                     :buffer-name "*info*"
                     :file "/tmp/current.info"
                     :node "Top")
                   (emacs-info-gui-refresh-context-from-backend)))
    (should (equal "/tmp/fixture.info" emacs-info-gui-arg))
    (should (equal "ok" emacs-info-gui-status))
    (should (equal "*info*" emacs-info-gui-buffer-name))
    (should (equal "/tmp/current.info" emacs-info-gui-file))
    (should (equal "Top" emacs-info-gui-node))))

(ert-deftest emacs-info-gui-current-context-command-variants ()
  (emacs-info-test--with-fresh-world
    (let ((calls nil)
          (header "File: fixture.info,  Node: Top,  Next: First,  Up: (dir)")
          (titles nil))
      (emacs-info-gui-register-backend
       :current-arg (lambda () "/tmp/fixture.info")
       :current-status (lambda () "ok")
       :current-file (lambda () emacs-info-gui-file)
       :current-node (lambda () emacs-info-gui-node)
       :file-exists-p
       (lambda (path)
         (equal path "/tmp/fixture.info"))
       :read-file
       (lambda (_path)
         emacs-info-test--fixture)
       :show-info-buffer
       (lambda (title _body)
         (push title titles)
         "*info*")
       :buffer-name
       (lambda ()
         "*info*")
       :current-header
       (lambda ()
         header)
       :write-state
       (lambda (file node)
         (push (list :state file node) calls))
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (should (equal "*info*"
                     (emacs-info-gui-current-context-command
                      'info "other")))
      (should (equal "Top" emacs-info-gui-node))
      (should (equal "*info*"
                     (emacs-info-gui-current-context-command
                      'Info-next)))
      (should (equal "First" emacs-info-gui-node))
      (emacs-info-gui-set-context :arg "find-file")
      (emacs-info-gui-register-backend
       :current-arg (lambda () "find-file")
       :current-status (lambda () "ok")
       :current-file (lambda () emacs-info-gui-file)
       :current-node (lambda () emacs-info-gui-node)
       :show-info-buffer
       (lambda (title _body)
         (push title titles)
         "*info*")
       :buffer-name
       (lambda ()
         "*info*"))
      (should (equal "*info*"
                     (emacs-info-gui-current-context-command
                      'Info-goto-emacs-command-node)))
      (should (member '(:display "other") calls))
      (should (member '(:state "/tmp/fixture.info" "First") calls))
      (should (member "Emacs Command: find-file" titles)))))

(ert-deftest emacs-info-run-current-context-command-uses-frontend-hooks ()
  (emacs-info-test--with-fresh-world
    (let ((prompts nil)
          (calls nil)
          (installed nil))
      (cl-letf (((symbol-function 'emacs-info-gui-current-context-command)
                 (lambda (command &optional action)
                   (push (list command action emacs-info-gui-arg) calls)
                   "*info*")))
        (should
         (equal
          "*info*"
          (emacs-info-run-current-context-command
           'info
           :install-function (lambda () (setq installed t))
           :read-string (lambda (prompt)
                          (push prompt prompts)
                          "/tmp/fixture.info")
           :prompt "Info file: ")))
        (should installed)
        (should (equal '("Info file: ") prompts))
        (should (member '(info "same" "/tmp/fixture.info") calls))
        (setq emacs-info-gui-arg "kept")
        (should
         (equal
          "*info*"
          (emacs-info-run-current-context-command 'Info-next)))
        (should (member '(Info-next "same" "kept") calls))
        (should-not
         (emacs-info-run-current-context-command
          'info
          :read-string (lambda (_prompt) "")
          :prompt "Info file: "))
        (should (= 2 (length calls)))))))

(ert-deftest emacs-info-gui-navigation-uses-current-header ()
  (emacs-info-test--with-fresh-world
    (let ((header "File: fixture.info,  Node: Top,  Next: First,  Up: (dir)")
          (rendered nil)
          (states nil))
      (emacs-info-gui-register-backend
       :read-file
       (lambda (_path)
         emacs-info-test--fixture)
       :show-info-buffer
       (lambda (title body)
         (setq rendered (list title body))
         "*info*")
       :buffer-name
       (lambda ()
         "*info*")
       :current-header
       (lambda ()
         header)
       :write-state
       (lambda (file node)
         (push (list file node) states)))
      (emacs-info-gui-set-context :buffer-name "*info*"
                                  :file "/tmp/fixture.info"
                                  :node "Top")
      (should (equal "*info*" (emacs-info-gui-next)))
      (should (equal "First" emacs-info-gui-node))
      (should (string-match-p "First node body line" (car rendered)))
      (setq header
            "File: fixture.info,  Node: First,  Next: Second,  Prev: Top,  Up: Top")
      (should (equal "*info*" (emacs-info-gui-prev)))
      (should (equal "Top" emacs-info-gui-node))
      (should (equal '(("/tmp/fixture.info" "Top")
                       ("/tmp/fixture.info" "First"))
                     states)))))

(ert-deftest emacs-info-gui-static-commands-render-through-backend ()
  (emacs-info-test--with-fresh-world
    (let ((titles nil))
      (emacs-info-gui-register-backend
       :show-info-buffer
       (lambda (title _body)
         (push title titles)
         "*info*")
       :buffer-name
       (lambda ()
         "*info*"))
      (emacs-info-gui-set-context :arg "elisp")
      (should (equal "*info*" (emacs-info-gui-display-manual)))
      (emacs-info-gui-set-context :arg "find-file")
      (should (equal "*info*" (emacs-info-gui-goto-emacs-command-node)))
      (emacs-info-gui-set-context :arg "C-x C-f")
      (should (equal "*info*" (emacs-info-gui-goto-emacs-key-command-node)))
      (emacs-info-gui-set-context :arg "message")
      (should (equal "*info*" (emacs-info-gui-lookup-symbol)))
      (should (equal '("Info Lookup Symbol: message"
                       "Emacs Key: C-x C-f"
                       "Emacs Command: find-file"
                       "Info Manual: elisp")
                     titles)))))

(ert-deftest emacs-info-gui-command-wrappers ()
  (emacs-info-test--with-fresh-world
    (let ((calls nil)
          (header "File: fixture.info,  Node: Top,  Next: First,  Up: (dir)")
          (titles nil))
      (emacs-info-gui-register-backend
       :file-exists-p
       (lambda (path)
         (equal path "/tmp/fixture.info"))
       :read-file
       (lambda (_path)
         emacs-info-test--fixture)
       :show-info-buffer
       (lambda (title _body)
         (push title titles)
         "*info*")
       :buffer-name
       (lambda ()
         "*info*")
       :current-header
       (lambda ()
         header)
       :write-state
       (lambda (file node)
         (push (list :state file node) calls))
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (emacs-info-gui-set-context :arg "/tmp/fixture.info")
      (should (equal "*info*" (emacs-info-gui-info-command "other")))
      (should (equal "Top" emacs-info-gui-node))
      (should (equal "*info*" (emacs-info-gui-next-command)))
      (should (equal "First" emacs-info-gui-node))
      (setq header
            "File: fixture.info,  Node: First,  Next: Second,  Prev: Top,  Up: Top")
      (should (equal "*info*" (emacs-info-gui-prev-command)))
      (should (equal "Top" emacs-info-gui-node))
      (should (equal "*info*" (emacs-info-gui-up-command)))
      (emacs-info-gui-set-context :arg "elisp")
      (should (equal "*info*"
                     (emacs-info-gui-display-manual-command)))
      (should (equal "*info*"
                     (emacs-info-gui-view-order-manuals-command)))
      (emacs-info-gui-set-context :arg "find-file")
      (should (equal "*info*"
                     (emacs-info-gui-goto-emacs-command-node-command)))
      (emacs-info-gui-set-context :arg "C-x C-f")
      (should (equal "*info*"
                     (emacs-info-gui-goto-emacs-key-command-node-command)))
      (emacs-info-gui-set-context :arg "message")
      (should (equal "*info*"
                     (emacs-info-gui-lookup-symbol-command)))
      (should (member '(:display "other") calls))
      (should (member '(:state "/tmp/fixture.info" "First") calls))
      (should (member "Info Lookup Symbol: message" titles))
      (should (member "Ordering GNU Manuals" titles)))))

(ert-deftest emacs-info-loader-provides-info-feature-and-runtime ()
  (let ((features (remove 'info (remove 'emacs-info features))))
    (require 'info)
    (should (featurep 'info))
    (should (featurep 'emacs-info))
    (should (fboundp 'Info-directory))
    (should (fboundp 'Info-goto-node))
    (should (fboundp 'Info-find-node))
    (should (fboundp 'Info-mode))
    (should (fboundp 'Info-next))
    (should (fboundp 'info-other-window))))

(ert-deftest emacs-info-host-interactive-mirror-renders-directory ()
  (emacs-info-test--with-fresh-world
    (let ((shown nil)
          (host-buffer nil))
      (unwind-protect
          (let ((noninteractive nil))
            (cl-letf (((symbol-function 'selected-window)
                       (lambda () :selected-window))
                      ((symbol-function 'set-window-buffer)
                       (lambda (_window buffer &optional _keep-margins)
                         (setq shown buffer))))
              (should (equal "*info*" (Info-directory)))
              (setq host-buffer (get-buffer "*info*"))
              (should host-buffer)
              (should (eq shown host-buffer))
              (with-current-buffer host-buffer
                (should (eq major-mode 'Info-mode))
                (should (equal mode-name "Info"))
                (should buffer-read-only)
                (should (save-excursion
                          (goto-char (point-min))
                          (search-forward "Info Directory" nil t)))
                (should (save-excursion
                          (goto-char (point-min))
                          (search-forward "Info directory navigation"
                                          nil t))))))
        (when (buffer-live-p host-buffer)
          (kill-buffer host-buffer))))))

(ert-deftest emacs-info-gui-writeback-spec ()
  (should (equal '(:buffer t :file t :buffer-name t :read-only t
                   :window t :point t :mark t :window-start t)
                 (emacs-info-gui-writeback-spec 'Info-next)))
  (should (equal '(:buffer t :file t :buffer-name t :read-only t
                   :window t :point t :mark t :window-start t)
                 (emacs-info-gui-writeback-spec
                  "info-display-manual")))
  (should-not (emacs-info-gui-writeback-spec 'describe-function)))

(ert-deftest emacs-info-gui-writeback-spec-flag ()
  (let ((spec (emacs-info-gui-writeback-spec 'Info-next)))
    (should (emacs-info-gui-writeback-spec-flag spec :buffer))
    (should (emacs-info-gui-writeback-spec-flag spec :read-only))
    (should-not (emacs-info-gui-writeback-spec-flag spec :modeline))
    (should-not (emacs-info-gui-writeback-spec-flag nil :buffer))))

(ert-deftest emacs-info-gui-writeback-state ()
  (emacs-info-test--with-fresh-world
    (let (calls)
      (emacs-info-gui-register-backend
       :write-buffer-state (lambda () (push :buffer calls))
       :write-file-state (lambda () (push :file calls))
       :write-buffer-name-state (lambda () (push :buffer-name calls))
       :write-read-only-state (lambda () (push :read-only calls))
       :write-window-state (lambda () (push :window calls))
       :write-point-state (lambda () (push :point calls))
       :write-mark-state (lambda () (push :mark calls))
       :write-window-start-state (lambda () (push :window-start calls))
       :mark-written-state (lambda () (push :written calls)))
      (should (emacs-info-gui-writeback-state 'Info-next))
      (should (equal '(:buffer :file :buffer-name :read-only :window
                       :point :mark :window-start :written)
                     (nreverse calls)))
      (setq calls nil)
      (should-not (emacs-info-gui-writeback-state 'describe-function))
      (should-not calls))))

(ert-deftest emacs-info-gui-needs-review-direct-commands ()
  (emacs-info-test--with-fresh-world
    (let ((titles nil)
          (header "File: fixture.info,  Node: First,  Prev: Top,  Up: Top"))
      (emacs-info-gui-register-backend
       :read-file
       (lambda (_path)
         emacs-info-test--fixture)
       :show-info-buffer
       (lambda (title _body)
         (push title titles)
         "*info*")
       :buffer-name
       (lambda ()
         "*info*")
       :current-header
       (lambda ()
         header))
      (emacs-info-gui-set-context :buffer-name "*info*"
                                  :file "/tmp/fixture.info"
                                  :node "First")
      (should (equal "*info*" (emacs-info-gui-up)))
      (should (equal "Top" emacs-info-gui-node))
      (should (equal "*info*" (emacs-info-gui-emacs-manual)))
      (should (equal "*info*" (emacs-info-gui-view-order-manuals)))
      (should (member "Emacs Manual" titles))
      (should (member "Ordering GNU Manuals" titles)))))

(ert-deftest emacs-info-gui-needs-review-current-context-wrappers ()
  (emacs-info-test--with-fresh-world
    (let ((arg "/tmp/fixture.info")
          (header "File: fixture.info,  Node: Top,  Next: First,  Up: (dir)")
          (titles nil)
          (calls nil))
      (emacs-info-gui-register-backend
       :current-arg (lambda () arg)
       :current-status (lambda () "ok")
       :buffer-name (lambda () "*info*")
       :current-file (lambda () emacs-info-gui-file)
       :current-node (lambda () emacs-info-gui-node)
       :file-exists-p
       (lambda (path)
         (equal path "/tmp/fixture.info"))
       :read-file
       (lambda (_path)
         emacs-info-test--fixture)
       :show-info-buffer
       (lambda (title _body)
         (push title titles)
         "*info*")
       :current-header
       (lambda ()
         header)
       :write-state
       (lambda (file node)
         (push (list :state file node) calls))
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (should (equal "*info*"
                     (emacs-info-gui-info-current-context-command
                      "other")))
      (should (equal "Top" emacs-info-gui-node))
      (should (equal "*info*"
                     (emacs-info-gui-next-current-context-command)))
      (should (equal "First" emacs-info-gui-node))
      (setq header
            "File: fixture.info,  Node: First,  Next: Second,  Prev: Top,  Up: Top")
      (should (equal "*info*"
                     (emacs-info-gui-prev-current-context-command)))
      (should (equal "Top" emacs-info-gui-node))
      (setq emacs-info-gui-node "First")
      (should (equal "*info*"
                     (emacs-info-gui-up-current-context-command)))
      (should (equal "Top" emacs-info-gui-node))
      (setq arg "elisp")
      (should (equal "*info*"
                     (emacs-info-gui-emacs-manual-current-context-command)))
      (should (equal "*info*"
                     (emacs-info-gui-display-manual-current-context-command)))
      (should (equal "*info*"
                     (emacs-info-gui-view-order-manuals-current-context-command)))
      (setq arg "find-file")
      (should (equal
               "*info*"
               (emacs-info-gui-goto-emacs-command-node-current-context-command)))
      (setq arg "C-x C-f")
      (should
       (equal
        "*info*"
        (emacs-info-gui-goto-emacs-key-command-node-current-context-command)))
      (setq arg "message")
      (should (equal "*info*"
                     (emacs-info-gui-lookup-symbol-current-context-command)))
      (should (member '(:display "other") calls))
      (should (member "Emacs Manual" titles))
      (should (member "Info Manual: elisp" titles))
      (should (member "Ordering GNU Manuals" titles))
      (should (member "Emacs Command: find-file" titles))
      (should (member "Emacs Key: C-x C-f" titles))
      (should (member "Info Lookup Symbol: message" titles)))))

(provide 'emacs-info-test)

;;; emacs-info-test.el ends here
