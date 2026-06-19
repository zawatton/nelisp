;;; emacs-help-test.el --- ERT for emacs-help -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-help)

(defvar emacs-help-test--sample-variable '(alpha beta)
  "Sample variable docstring for help tests.")

(defun emacs-help-test--sample-function (required &optional optional)
  "Sample function docstring for help tests."
  (list required optional))

(defun emacs-help-test--fresh-help-state ()
  "Reset mutable help state used by tests."
  (setq emacs-help--state (make-hash-table :test 'eq :weakness nil))
  (when (get-buffer emacs-help--buffer-name)
    (kill-buffer emacs-help--buffer-name))
  (let ((map (make-sparse-keymap)))
    (use-global-map map)
    (emacs-help--ensure-global-bindings)
    map))

(defmacro emacs-help-test--with-fresh-world (&rest body)
  "Run BODY with clean help/buffer/keymap state."
  (declare (indent 0) (debug (body)))
  `(let ((major-mode 'fundamental-mode)
         (mode-name "Fundamental"))
     (unwind-protect
         (progn
           (when (fboundp 'emacs-mode-reset)
             (emacs-mode-reset))
           (emacs-help-test--fresh-help-state)
           ,@body)
       (when (get-buffer emacs-help--buffer-name)
         (kill-buffer emacs-help--buffer-name))
       (when (fboundp 'emacs-mode-reset)
         (emacs-mode-reset)))))

(defun emacs-help-test--help-string ()
  "Return the current `*Help*' buffer string."
  (with-current-buffer (get-buffer emacs-help--buffer-name)
    (buffer-string)))

(ert-deftest describe-function-renders-signature-and-docstring ()
  (emacs-help-test--with-fresh-world
    (describe-function 'emacs-help-test--sample-function)
    (let ((text (emacs-help-test--help-string)))
      (should (string-match-p
               "(emacs-help-test--sample-function required &optional optional)"
               text))
      (should (string-match-p "Sample function docstring for help tests\\." text))
      (should (string-match-p "Defined in:" text)))))

(ert-deftest describe-function-handles-undefined-function ()
  (emacs-help-test--with-fresh-world
    (should-error (describe-function 'emacs-help-test--missing-function)
                  :type 'user-error)))

(ert-deftest describe-variable-renders-value-and-docstring ()
  (emacs-help-test--with-fresh-world
    (describe-variable 'emacs-help-test--sample-variable)
    (let ((text (emacs-help-test--help-string)))
      (should (string-match-p "emacs-help-test--sample-variable is a variable\\." text))
      (should (string-match-p "(alpha beta)" text))
      (should (string-match-p "Sample variable docstring for help tests\\." text)))))

(ert-deftest describe-variable-handles-unbound-variable ()
  (emacs-help-test--with-fresh-world
    (makunbound 'emacs-help-test--temporary-unbound)
    (should-error (describe-variable 'emacs-help-test--temporary-unbound)
                  :type 'user-error)))

(ert-deftest describe-symbol-dispatches-to-function-or-variable ()
  (emacs-help-test--with-fresh-world
    (describe-symbol 'emacs-help-test--sample-function)
    (should (string-match-p "is a function" (emacs-help-test--help-string)))
    (describe-symbol 'emacs-help-test--sample-variable)
    (should (string-match-p "is a variable" (emacs-help-test--help-string)))))

(ert-deftest help-mode-history-commands-report-unavailable ()
  (emacs-help-test--with-fresh-world
    (should-error (help-go-back) :type 'user-error)
    (should-error (help-go-forward) :type 'user-error)))

(ert-deftest describe-key-resolves-binding ()
  (emacs-help-test--with-fresh-world
    (define-key (current-global-map) (kbd "C-c h")
                #'emacs-help-test--sample-function)
    (describe-key (kbd "C-c h"))
    (let ((text (emacs-help-test--help-string)))
      (should (string-match-p "C-c h runs the command emacs-help-test--sample-function\\." text))
      (should (string-match-p "Sample function docstring for help tests\\." text)))))

(ert-deftest emacs-help-gui-backend-renders-describe-function ()
  (emacs-help-test--with-fresh-world
    (let (shown)
      (emacs-help-gui-register-backend
       :show-help-buffer
       (lambda (title body)
         (setq shown (list title body))
         "*Help*"))
      (emacs-help-gui-set-context :arg "forward-char")
      (should (equal "*Help*" (emacs-help-gui-describe-function)))
      (should (equal "forward-char" (car shown)))
      (should (string-match-p "forward-char is a function" (cadr shown)))
      (should (string-match-p "Move point one character forward" (cadr shown))))))

(ert-deftest emacs-help-gui-description-cores-return-title-body ()
  (emacs-help-test--with-fresh-world
    (emacs-help-gui-set-context
     :arg "forward-char"
     :current-file-name "/tmp/current.txt"
     :buffer-name "notes"
     :buffer-read-only-p t)
    (let ((function-entry (emacs-help-gui-describe-function-core))
          (variable-entry nil))
      (should (equal "forward-char" (car function-entry)))
      (should (string-match-p "forward-char is a function" (cdr function-entry)))
      (should (string-match-p "Move point one character forward"
                              (cdr function-entry)))
      (emacs-help-gui-set-context :arg "buffer-file-name")
      (setq variable-entry (emacs-help-gui-describe-variable-core))
      (should (equal "buffer-file-name" (car variable-entry)))
      (should (string-match-p "Value: /tmp/current.txt"
                              (cdr variable-entry)))
      (should (string-match-p "buffer-file-name is a variable"
                              (cdr variable-entry))))))

(ert-deftest emacs-help-gui-refresh-context-from-backend ()
  (emacs-help-test--with-fresh-world
    (emacs-help-gui-register-backend
     :current-arg (lambda () "buffer-file-name")
     :current-file-name (lambda () "/tmp/current.txt")
     :buffer-name (lambda () "notes")
     :buffer-read-only-p (lambda () t)
     :window-layout (lambda () "single")
     :keymap-source (lambda () "C-x C-f\tfind-file\n")
     :user-keymap-source (lambda () "C-c n\tnote-open\n")
     :minibuffer-keymap-source (lambda () "RET\texit-minibuffer\n")
     :current-status (lambda () "ok"))
    (should (equal '(:arg "buffer-file-name"
                     :current-file-name "/tmp/current.txt"
                     :buffer-name "notes"
                     :buffer-read-only-p t
                     :window-layout "single"
                     :keymap-source "C-x C-f\tfind-file\n"
                     :user-keymap-source "C-c n\tnote-open\n"
                     :minibuffer-keymap-source "RET\texit-minibuffer\n"
                     :status "ok")
                   (emacs-help-gui-refresh-context-from-backend)))
    (should (equal "buffer-file-name" emacs-help-gui-arg))
    (should (equal "/tmp/current.txt"
                   emacs-help-gui-current-file-name))
    (should (equal "notes" emacs-help-gui-buffer-name))
    (should emacs-help-gui-buffer-read-only-p)
    (should (equal "single" emacs-help-gui-window-layout))))

(ert-deftest emacs-help-gui-key-lookup-uses-backend ()
  (emacs-help-test--with-fresh-world
    (let (lookup)
      (emacs-help-gui-register-backend
       :lookup-key-command
       (lambda (source key)
         (setq lookup (list source key))
         "find-file"))
      (should (equal "find-file"
                     (emacs-help-gui--lookup-key-command
                      "C-x C-f\ttoo-slow\n" "C-x C-f")))
      (should (equal '("C-x C-f\ttoo-slow\n" "C-x C-f") lookup)))))

(ert-deftest emacs-help-gui-current-context-command-variants ()
  (emacs-help-test--with-fresh-world
    (let (shown
          (arg "forward-char"))
      (emacs-help-gui-register-backend
       :current-arg (lambda () arg)
       :current-file-name (lambda () "/tmp/current.txt")
       :buffer-name (lambda () "notes")
       :buffer-read-only-p (lambda () nil)
       :window-layout (lambda () "single")
       :keymap-source
       (lambda () "C-x C-f\tfind-file\nC-x C-s\tsave-buffer\n")
       :user-keymap-source (lambda () "C-c n\tnote-open\n")
       :minibuffer-keymap-source (lambda () "RET\texit-minibuffer\n")
       :current-status (lambda () "ok")
       :show-help-buffer
       (lambda (title body)
         (push (list title body) shown)
         "*Help*"))
      (should (equal "*Help*"
                     (emacs-help-gui-current-context-command
                      'describe-function)))
      (setq arg "buffer-file-name")
      (should (equal "*Help*"
                     (emacs-help-gui-current-context-command
                      'describe-variable)))
      (setq arg "C-x C-f")
      (should (equal "*Help*"
                     (emacs-help-gui-current-context-command
                      'describe-key)))
      (should (equal "*Help*"
                     (emacs-help-gui-current-context-command
                      'describe-key-briefly)))
      (should (equal "*Help*"
                     (emacs-help-gui-current-context-command
                      'describe-bindings)))
      (setq arg "note-open")
      (should (equal "*Help*"
                     (emacs-help-gui-current-context-command
                      'where-is)))
      (should (equal "*Help*"
                     (emacs-help-gui-current-context-command
                      'help-for-help)))
      (setq arg "forward-char")
      (should (equal "*Help*"
                     (emacs-help-gui-current-context-command
                      'describe-command)))
      (setq arg "files")
      (should (equal "*Help*"
                     (emacs-help-gui-current-context-command
                      'describe-package)))
      (setq arg "find")
      (should (equal "*Help*"
                     (emacs-help-gui-current-context-command
                      'apropos-command)))
      (should (equal "*Help*"
                     (emacs-help-gui-current-context-command
                      'apropos-documentation)))
      (should (equal "*Help*"
                     (emacs-help-gui-current-context-command
                      'finder-by-keyword 'finder-by-keyword)))
      (should (cl-some (lambda (entry)
                         (string-match-p "forward-char is a function"
                                         (cadr entry)))
                       shown))
      (should (cl-some (lambda (entry)
                         (string-match-p "Value: /tmp/current.txt"
                                         (cadr entry)))
                       shown))
      (should (cl-some (lambda (entry)
                         (string-match-p "C-x C-f runs the command find-file"
                                         (cadr entry)))
                       shown))
      (should (cl-some (lambda (entry)
                         (string-match-p "note-open is on C-c n"
                                         (cadr entry)))
                       shown))
      (should (cl-some (lambda (entry)
                         (equal "Package: files" (car entry)))
                       shown)))))

(ert-deftest emacs-help-gui-show-help-buffer-wrapper ()
  (emacs-help-test--with-fresh-world
    (let (shown)
      (emacs-help-gui-register-backend
       :show-help-buffer
       (lambda (title body)
         (setq shown (list title body))
         "*Help*"))
      (should (equal "*Help*"
                     (emacs-help-gui-show-help-buffer
                      "Title" "Body\n")))
      (should (equal '("Title" "Body\n") shown)))))

(ert-deftest emacs-help-gui-keymap-help-uses-registered-sources ()
  (emacs-help-test--with-fresh-world
    (let (shown)
      (emacs-help-gui-register-backend
       :show-help-buffer
       (lambda (title body)
         (setq shown (list title body))
         "*Help*"))
      (emacs-help-gui-set-context
       :arg "C-x C-f"
       :keymap-source "C-x C-f\tfind-file\tFind file: \nC-x C-s\tsave-buffer\n"
       :user-keymap-source ""
       :minibuffer-keymap-source "RET\texit-minibuffer\n")
      (should (equal "*Help*" (emacs-help-gui-describe-key)))
      (should (string-match-p "C-x C-f runs the command find-file" (cadr shown)))
      (should (equal "*Help*" (emacs-help-gui-describe-bindings)))
      (should (string-match-p "C-x C-s[ \t]+save-buffer" (cadr shown))))))

(ert-deftest emacs-help-gui-keymap-cores-return-title-body ()
  (emacs-help-test--with-fresh-world
    (emacs-help-gui-set-context
     :arg "C-x C-f"
     :keymap-source "C-x C-f\tfind-file\tFind file: \nC-x C-s\tsave-buffer\n"
     :user-keymap-source "C-c n\tnote-open\n"
     :minibuffer-keymap-source "RET\texit-minibuffer\n")
    (let ((key-entry (emacs-help-gui-describe-key-core))
          (brief-entry (emacs-help-gui-describe-key-briefly-core))
          (bindings-entry (emacs-help-gui-describe-bindings-core))
          (where-entry nil))
      (should (equal "C-x C-f" (car key-entry)))
      (should (string-match-p "C-x C-f runs the command find-file"
                              (cdr key-entry)))
      (should (equal "C-x C-f" (car brief-entry)))
      (should (string-match-p "C-x C-f runs the command find-file"
                              (cdr brief-entry)))
      (should (equal "Key Bindings" (car bindings-entry)))
      (should (string-match-p "C-c n[ \t]+note-open"
                              (cdr bindings-entry)))
      (emacs-help-gui-set-context :arg "note-open")
      (setq where-entry (emacs-help-gui-where-is-core))
      (should (equal "note-open" (car where-entry)))
      (should (string-match-p "note-open is on C-c n"
                              (cdr where-entry))))))

(ert-deftest emacs-help-gui-static-and-package-help-render-through-backend ()
  (emacs-help-test--with-fresh-world
    (let (shown)
      (emacs-help-gui-register-backend
       :show-help-buffer
       (lambda (title body)
         (setq shown (list title body))
         "*Help*"))
      (should (equal "*Help*" (emacs-help-gui-static-command 'about-emacs)))
      (should (equal "About GNU Emacs" (car shown)))
      (emacs-help-gui-set-context :arg "files")
      (should (equal "*Help*" (emacs-help-gui-describe-package)))
      (should (equal "Package: files" (car shown)))
      (should (string-match-p "Full package metadata lookup" (cadr shown))))))

(ert-deftest emacs-help-gui-writeback-spec ()
  (should (equal '(:buffer t :file t :buffer-name t :read-only t
                   :point t :mark t :window-start t)
                 (emacs-help-gui-writeback-spec 'describe-function)))
  (should (equal '(:buffer t :file t :buffer-name t :read-only t
                   :window t :point t :mark t :window-start t)
                 (emacs-help-gui-writeback-spec 'about-emacs)))
  (should-not (emacs-help-gui-writeback-spec 'find-file)))

(ert-deftest emacs-help-gui-writeback-spec-flag ()
  (let ((spec (emacs-help-gui-writeback-spec 'about-emacs)))
    (should (emacs-help-gui-writeback-spec-flag spec :buffer))
    (should (emacs-help-gui-writeback-spec-flag spec :window))
    (should-not (emacs-help-gui-writeback-spec-flag spec :modeline))
    (should-not (emacs-help-gui-writeback-spec-flag nil :buffer))))

(ert-deftest emacs-help-gui-writeback-state ()
  (emacs-help-test--with-fresh-world
    (let (calls)
      (emacs-help-gui-register-backend
       :write-buffer-state (lambda () (push :buffer calls))
       :write-file-state (lambda () (push :file calls))
       :write-buffer-name-state (lambda () (push :buffer-name calls))
       :write-read-only-state (lambda () (push :read-only calls))
       :write-window-state (lambda () (push :window calls))
       :write-point-state (lambda () (push :point calls))
       :write-mark-state (lambda () (push :mark calls))
       :write-window-start-state (lambda () (push :window-start calls))
       :mark-written-state (lambda () (push :written calls)))
      (should (emacs-help-gui-writeback-state 'describe-function))
      (should (equal '(:buffer :file :buffer-name :read-only
                       :point :mark :window-start :written)
                     (nreverse calls)))
      (setq calls nil)
      (should (emacs-help-gui-writeback-state 'about-emacs))
      (should (equal '(:buffer :file :buffer-name :read-only :window
                       :point :mark :window-start :written)
                     (nreverse calls)))
      (setq calls nil)
      (should-not (emacs-help-gui-writeback-state 'find-file))
      (should-not calls))))

(ert-deftest help-mode-q-buries-buffer ()
  (emacs-help-test--with-fresh-world
    (describe-function 'emacs-help-test--sample-function)
    (let ((quit-called nil))
      (cl-letf (((symbol-function 'quit-window)
                 (lambda (&rest _)
                   (setq quit-called t)
                   :quit)))
        (should (eq :quit (funcall (lookup-key help-mode-map (kbd "q")))))
        (should quit-called)))))

(ert-deftest help-mode-g-rerenders-last-description ()
  (emacs-help-test--with-fresh-world
    (let ((calls 0))
      (cl-letf (((symbol-function 'documentation)
                 (lambda (_symbol &optional _raw)
                   (setq calls (1+ calls))
                   (format "render %d" calls))))
        (describe-function 'emacs-help-test--sample-function)
        (should (string-match-p "render 1" (emacs-help-test--help-string)))
        (call-interactively (lookup-key help-mode-map (kbd "g")))
        (should (string-match-p "render 2" (emacs-help-test--help-string)))))))

(ert-deftest emacs-help-global-key-bindings-installed ()
  (emacs-help-test--with-fresh-world
    (let ((map (current-global-map)))
      (should (eq #'describe-function (lookup-key map (kbd "C-h f"))))
      (should (eq #'describe-variable (lookup-key map (kbd "C-h v"))))
      (should (eq #'describe-key (lookup-key map (kbd "C-h k")))))))

(provide 'emacs-help-test)

;;; emacs-help-test.el ends here
