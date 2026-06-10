;;; nemacs-gui-file-bridge-runtime-test.el --- GUI bridge runtime checks  -*- lexical-binding: t; -*-

;;; Commentary:

;; The GUI bridge is a small Layer-2 adapter consumed by nelisp-gui.  Host ERT
;; pins the source shape; the standalone reader subprocess gate is opt-in
;; because it depends on a built NeLisp binary.  The runtime defaults to
;; /tmp/nemacs-* transport but can be pointed at an isolated directory by
;; setting `files--transport-dir' before `nemacs-gui-file-bridge-run'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nemacs-gui-file-bridge-runtime-test--repo-root
  (expand-file-name
   ".."
   (file-name-directory (or load-file-name buffer-file-name))))

(defconst nemacs-gui-file-bridge-runtime-test--source
  (expand-file-name
   "src/nemacs-gui-file-bridge-runtime.el"
   nemacs-gui-file-bridge-runtime-test--repo-root))

(defun nemacs-gui-file-bridge-runtime-test--slurp (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun nemacs-gui-file-bridge-runtime-test--point-value ()
  "Return the numeric bridge point transport value."
  (string-to-number
   (nemacs-gui-file-bridge-runtime-test--slurp "/tmp/nemacs-point")))

(defun nemacs-gui-file-bridge-runtime-test--mark-value ()
  "Return the numeric bridge mark transport value."
  (string-to-number
   (nemacs-gui-file-bridge-runtime-test--slurp "/tmp/nemacs-mark")))

(defun nemacs-gui-file-bridge-runtime-test--should-point (label expected)
  "Assert that point transport is EXPECTED and report LABEL on failure."
  (ert-info ((format "%s point transport" label))
    (should (= expected (nemacs-gui-file-bridge-runtime-test--point-value)))))

(defun nemacs-gui-file-bridge-runtime-test--reader ()
  "Return an executable standalone reader candidate, or nil."
  (let ((candidates
         (list
          (getenv "NEMACS_GUI_BRIDGE_NELISP")
          (getenv "NELISP")
          "/tmp/nelisp-snap/nelisp"
          (expand-file-name "../nelisp/target/nelisp"
                            nemacs-gui-file-bridge-runtime-test--repo-root)
          (expand-file-name "vendor/nelisp/target/nelisp"
                            nemacs-gui-file-bridge-runtime-test--repo-root))))
    (catch 'found
      (dolist (candidate candidates)
        (when (and candidate (file-executable-p candidate))
          (throw 'found candidate)))
      nil)))

(defun nemacs-gui-file-bridge-runtime-test--write-image ()
  "Write a temporary source-v1 runtime image for the GUI bridge."
  (let ((image (make-temp-file "nemacs-gui-file-bridge-" nil ".nlri")))
    (with-temp-file image
      (insert ";;; nelisp-runtime-image source-v1\n(progn\n")
      (insert-file-contents nemacs-gui-file-bridge-runtime-test--source)
      (insert "\n)\n"))
    image))

(defconst nemacs-gui-file-bridge-runtime-test--transport-lock
  "/tmp/nemacs-transport.lock")

(defvar nemacs-gui-file-bridge-runtime-test--transport-lock-held nil)

(defun nemacs-gui-file-bridge-runtime-test--transport-lock-stale-p ()
  "Return non-nil when the fixed transport lock has no living owner."
  (let* ((pid-file
          (expand-file-name
           "pid"
           nemacs-gui-file-bridge-runtime-test--transport-lock))
         (pid-text
          (and (file-exists-p pid-file)
               (nemacs-gui-file-bridge-runtime-test--slurp pid-file))))
    (if (or (not pid-text) (equal pid-text ""))
        t
      (not (equal 0 (call-process "kill" nil nil nil "-0" pid-text))))))

(defun nemacs-gui-file-bridge-runtime-test--acquire-transport-lock ()
  "Acquire the fixed /tmp/nemacs-* transport lock for standalone checks."
  (if nemacs-gui-file-bridge-runtime-test--transport-lock-held
      t
    (let ((tries 0)
          (acquired nil))
      (while (not acquired)
        (condition-case nil
            (progn
              (make-directory nemacs-gui-file-bridge-runtime-test--transport-lock)
              (write-region
               (number-to-string (emacs-pid)) nil
               (expand-file-name
                "pid"
                nemacs-gui-file-bridge-runtime-test--transport-lock)
               nil 'silent)
              (setq acquired t)
              (setq nemacs-gui-file-bridge-runtime-test--transport-lock-held t))
          (file-already-exists
           (when (nemacs-gui-file-bridge-runtime-test--transport-lock-stale-p)
             (delete-directory
              nemacs-gui-file-bridge-runtime-test--transport-lock t)
             (setq tries 0))
           (setq tries (1+ tries))
           (when (> tries 300)
             (ert-fail
              (format "timed out waiting for %s"
                      nemacs-gui-file-bridge-runtime-test--transport-lock)))
           (sleep-for 1))))
      acquired)))

(defun nemacs-gui-file-bridge-runtime-test--release-transport-lock ()
  "Release the fixed /tmp/nemacs-* transport lock."
  (when nemacs-gui-file-bridge-runtime-test--transport-lock-held
    (when (file-directory-p nemacs-gui-file-bridge-runtime-test--transport-lock)
      (delete-directory nemacs-gui-file-bridge-runtime-test--transport-lock t))
    (setq nemacs-gui-file-bridge-runtime-test--transport-lock-held nil)))

(add-hook 'kill-emacs-hook
          #'nemacs-gui-file-bridge-runtime-test--release-transport-lock)

(defmacro nemacs-gui-file-bridge-runtime-test--with-transport (&rest body)
  "Run BODY after backing up fixed /tmp/nemacs-* bridge transport files."
  (declare (indent 0) (debug t))
  `(let* ((paths '("/tmp/nemacs-cmd"
                   "/tmp/nemacs-keys"
                   "/tmp/nemacs-file"
	                   "/tmp/nemacs-arg"
	                   "/tmp/nemacs-minibuffer-text"
	                   "/tmp/nemacs-minibuffer-arg"
		                   "/tmp/nemacs-buf"
		                   "/tmp/nemacs-point"
		                   "/tmp/nemacs-mark"
					                   "/tmp/nemacs-exit"
					                   "/tmp/nemacs-kill"
					                   "/tmp/nemacs-kill-ring"
					                   "/tmp/nemacs-kill-ring-index"
					                   "/tmp/nemacs-read-only"
					                   "/tmp/nemacs-buffer-name"
				                   "/tmp/nemacs-buffer-list"
				                   "/tmp/nemacs-window-layout"
				                   "/tmp/nemacs-window-selected"
				                   "/tmp/nemacs-window-start"
	                   "/tmp/nemacs-window-hscroll"
	                   "/tmp/nemacs-window-split-delta"
                       "/tmp/nemacs-window-dedicated"
                       "/tmp/nemacs-side-windows-visible"
	                                   "/tmp/nemacs-tab-state"
	                                   "/tmp/nemacs-frame-state"
                                       "/tmp/nemacs-frame-undo-state"
						                   "/tmp/nemacs-cursor"
					                   "/tmp/nemacs-modeline"
							                   "/tmp/nemacs-prefix-arg"
                                               "/tmp/nemacs-kmacro-recording"
                                               "/tmp/nemacs-kmacro-keys"
							                   "/tmp/nemacs-goal-column"
						                   "/tmp/nemacs-global-mark"
						                   "/tmp/nemacs-truncate-lines"
						                   "/tmp/nemacs-rectangle-mark-mode"
						                   "/tmp/nemacs-last-command"
					                   "/tmp/nemacs-cycle-spacing-action"
					                   "/tmp/nemacs-cycle-spacing-point"
					                   "/tmp/nemacs-cycle-spacing-whitespace"
					                   "/tmp/nemacs-undo-buf"
	                   "/tmp/nemacs-undo-point"
	                   "/tmp/nemacs-undo-mark"
	                   "/tmp/nemacs-undo-ready"
	                   "/tmp/nemacs-session-ready"
	                   "/tmp/nemacs-session-request"
	                   "/tmp/nemacs-session-response"
	                   "/tmp/nemacs-session-shutdown"
	                   "/tmp/nemacs-minibuffer-active"
			                   "/tmp/nemacs-minibuffer-prompt"
			                   "/tmp/nemacs-minibuffer-state"
                           "/tmp/nemacs-minibuffer-purpose"
			                   "/tmp/nemacs-minibuffer-cursor"
			                   "/tmp/nemacs-minibuffer-candidates"
			                   "/tmp/nemacs-minibuffer-history"
	                           "/tmp/nemacs-minibuffer-require-match"
                           "/tmp/nemacs-replace-string-from"
                           "/tmp/nemacs-query-replace-from"
                           "/tmp/nemacs-query-replace-to"
                           "/tmp/nemacs-query-replace-active"
                           "/tmp/nemacs-query-replace-regexp"
	                           "/tmp/nemacs-rectangle-kill"
                           "/tmp/nemacs-bookmark-list"
                           "/tmp/nemacs-abbrev-table"
			                   "/tmp/nemacs-status"
			                   "/tmp/nemacs-dired-marks"
			                   "/tmp/nemacs-magit-root"
			                   "/tmp/nemacs-magit-output"
			                   "/tmp/nemacs-tramp-output"
			                   "/tmp/nemacs-tramp-stage"
			                   "/tmp/nemacs-org-time"
			                   "/tmp/nemacs-org-capture-file"))
	          (dirs '("/tmp/nemacs-buffer-store"
	                  "/tmp/nemacs-buffer-file-store"
		                  "/tmp/nemacs-buffer-point-store"
		                  "/tmp/nemacs-buffer-mark-store"
		                  "/tmp/nemacs-buffer-window-start-store"
		                  "/tmp/nemacs-buffer-read-only-store"
                          "/tmp/nemacs-buffer-modified-store"
                          "/tmp/nemacs-buffer-narrow-active-store"
                          "/tmp/nemacs-buffer-narrow-start-store"
                          "/tmp/nemacs-buffer-narrow-end-store"
                          "/tmp/nemacs-buffer-narrow-full-store"
	                          "/tmp/nemacs-register-store"
	                          "/tmp/nemacs-bookmark-store"))
	          (backup-dir (make-temp-file "nemacs-gui-file-bridge-transport-" t)))
     (unwind-protect
         (progn
                   (nemacs-gui-file-bridge-runtime-test--acquire-transport-lock)
		           (dolist (path paths)
			             (when (file-regular-p path)
			               (copy-file path (expand-file-name (file-name-nondirectory path)
			                                                 backup-dir)
			                          t)))
		           (dolist (dir dirs)
		             (when (file-directory-p dir)
		               (copy-directory dir (expand-file-name (file-name-nondirectory dir)
		                                                     backup-dir)
		                               t t t)))
                   (dolist (path paths)
                     (when (file-exists-p path)
                       (delete-file path)))
                   (dolist (dir dirs)
                     (when (file-directory-p dir)
                       (delete-directory dir t)))
		           (dolist (dir dirs)
		             (unless (file-directory-p dir)
		               (make-directory dir t)))
		           (unless (file-exists-p "/tmp/nemacs-buffer-name")
		             (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent))
			           (unless (file-exists-p "/tmp/nemacs-window-layout")
			             (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent))
		           (unless (file-exists-p "/tmp/nemacs-window-selected")
			             (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent))
		           (unless (file-exists-p "/tmp/nemacs-window-start")
		             (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent))
		           (unless (file-exists-p "/tmp/nemacs-window-hscroll")
		             (write-region "0" nil "/tmp/nemacs-window-hscroll" nil 'silent))
		           (unless (file-exists-p "/tmp/nemacs-window-split-delta")
		             (write-region "0" nil "/tmp/nemacs-window-split-delta" nil 'silent))
                   (unless (file-exists-p "/tmp/nemacs-window-dedicated")
                     (write-region "0" nil "/tmp/nemacs-window-dedicated" nil 'silent))
                   (unless (file-exists-p "/tmp/nemacs-side-windows-visible")
                     (write-region "0" nil "/tmp/nemacs-side-windows-visible" nil 'silent))
	                   (unless (file-exists-p "/tmp/nemacs-tab-state")
	                     (write-region "0\t1\t1" nil "/tmp/nemacs-tab-state" nil 'silent))
	                   (unless (file-exists-p "/tmp/nemacs-frame-state")
	                     (write-region "0\t1\t1" nil "/tmp/nemacs-frame-state" nil 'silent))
			           (unless (file-exists-p "/tmp/nemacs-read-only")
		             (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent))
		           (unless (file-exists-p "/tmp/nemacs-point")
		             (write-region "0" nil "/tmp/nemacs-point" nil 'silent))
		           (unless (file-exists-p "/tmp/nemacs-mark")
		             (write-region "0" nil "/tmp/nemacs-mark" nil 'silent))
		           (unless (file-exists-p "/tmp/nemacs-rectangle-mark-mode")
		             (write-region "0" nil "/tmp/nemacs-rectangle-mark-mode" nil 'silent))
		           (unless (file-exists-p "/tmp/nemacs-kill-ring")
		             (write-region "" nil "/tmp/nemacs-kill-ring" nil 'silent))
		           (unless (file-exists-p "/tmp/nemacs-kill-ring-index")
		             (write-region "0" nil "/tmp/nemacs-kill-ring-index" nil 'silent))
		           (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
		           (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
		           (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
		           (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
			           (write-region "" nil "/tmp/nemacs-minibuffer-prompt" nil 'silent)
			           (write-region "" nil "/tmp/nemacs-minibuffer-state" nil 'silent)
                       (write-region "" nil "/tmp/nemacs-minibuffer-purpose" nil 'silent)
			           (write-region "0" nil "/tmp/nemacs-minibuffer-cursor" nil 'silent)
				           (write-region "" nil "/tmp/nemacs-minibuffer-candidates" nil 'silent)
				           (write-region "" nil "/tmp/nemacs-minibuffer-history" nil 'silent)
				           (write-region "0" nil "/tmp/nemacs-minibuffer-require-match" nil 'silent)
                       (write-region "" nil "/tmp/nemacs-replace-string-from" nil 'silent)
                       (write-region "" nil "/tmp/nemacs-query-replace-from" nil 'silent)
                       (write-region "" nil "/tmp/nemacs-query-replace-to" nil 'silent)
                       (write-region "0" nil "/tmp/nemacs-query-replace-active" nil 'silent)
					           (write-region "0" nil "/tmp/nemacs-query-replace-regexp" nil 'silent)
					           (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
					           (write-region "0" nil "/tmp/nemacs-kmacro-recording" nil 'silent)
					           (write-region "" nil "/tmp/nemacs-kmacro-keys" nil 'silent)
				           ,@body)
	       (dolist (path paths)
	         (when (file-exists-p path)
	           (delete-file path)))
	       (dolist (dir dirs)
	         (when (file-directory-p dir)
	           (delete-directory dir t)))
	       (dolist (path paths)
	         (let ((backup (expand-file-name (file-name-nondirectory path)
	                                         backup-dir)))
	           (when (file-exists-p backup)
	             (copy-file backup path t))))
	       (dolist (dir dirs)
	         (let ((backup (expand-file-name (file-name-nondirectory dir)
	                                         backup-dir)))
		           (when (file-directory-p backup)
		             (copy-directory backup dir t t t))))
		       (delete-directory backup-dir t))))

(defun nemacs-gui-file-bridge-runtime-test--run-image (reader image form)
  "Run READER against IMAGE with FORM and return captured stdout/stderr/status."
  (let ((stdout-file (make-temp-file "nemacs-gui-file-bridge-stdout-"))
        (stderr-file (make-temp-file "nemacs-gui-file-bridge-stderr-"))
        status stdout stderr)
    (unwind-protect
        (progn
          (setq status
                (call-process reader nil (list stdout-file stderr-file) nil
                              "exec-runtime-image" image form))
          (setq stdout
                (and (file-exists-p stdout-file)
                     (nemacs-gui-file-bridge-runtime-test--slurp stdout-file)))
          (setq stderr
                (and (file-exists-p stderr-file)
                     (nemacs-gui-file-bridge-runtime-test--slurp stderr-file)))
          (list :status status :stdout stdout :stderr stderr))
      (when (file-exists-p stdout-file)
        (delete-file stdout-file))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun nemacs-gui-file-bridge-runtime-test--run-ok (reader image form)
  "Run FORM and fail the current test unless it exits successfully."
  (let ((result
         (nemacs-gui-file-bridge-runtime-test--run-image reader image form)))
    (unless (equal 0 (plist-get result :status))
      (ert-fail
       (format "exec-runtime-image failed: status=%S\nstdout:\n%s\nstderr:\n%s"
               (plist-get result :status)
               (plist-get result :stdout)
               (plist-get result :stderr))))
    result))

(defun nemacs-gui-file-bridge-runtime-test--raw-key-form (keys)
  "Return a standalone form that dispatches each raw key in KEYS."
  (concat
   "(progn\n"
   (mapconcat
    (lambda (key)
      (format "  (nl-write-file \"/tmp/nemacs-keys\" %S)\n  (nemacs-gui-file-bridge-run)"
              key))
    keys
    "\n")
   "\n)"))

(defmacro nemacs-gui-file-bridge-runtime-test--skip-unless-reader (&rest body)
  "Run BODY only when the opt-in standalone GUI bridge gate is enabled."
  (declare (indent 0) (debug t))
  `(cond
    ((not (getenv "NEMACS_RUN_GUI_BRIDGE"))
     (ert-skip "set NEMACS_RUN_GUI_BRIDGE=1 to run standalone GUI bridge checks"))
    ((not (nemacs-gui-file-bridge-runtime-test--reader))
     (ert-skip "no standalone reader found; set NEMACS_GUI_BRIDGE_NELISP or NELISP"))
    (t ,@body)))

(ert-deftest nemacs-gui-file-bridge-runtime-test/source-shape ()
  "The bridge source should expose the GUI adapter through command execution."
  (should (file-readable-p nemacs-gui-file-bridge-runtime-test--source))
  (with-temp-buffer
    (insert-file-contents nemacs-gui-file-bridge-runtime-test--source)
    (goto-char (point-min))
    (check-parens))
  (let ((source
         (nemacs-gui-file-bridge-runtime-test--slurp
          nemacs-gui-file-bridge-runtime-test--source)))
    (dolist (needle '("(fset 'commandp"
                      "(fset 'call-interactively"
	                      "(fset 'command-execute"
	                              "(fset 'execute-extended-command"
			                      "(fset 'execute-extended-command-for-buffer"
                                  "(fset 'call-process"
                                  "nelisp-process-call-process"
		                              "(fset 'shell-command"
	                              "(fset 'shell-command-on-region"
	                              "(fset 'async-shell-command"
                                  "(fset 'project-shell-command"
                                  "(fset 'project-async-shell-command"
                                  "(fset 'project-shell"
                                  "(fset 'project-eshell"
                                  "(fset 'project-compile"
                                  "(fset 'project-find-regexp"
                                  "(fset 'project-or-external-find-regexp"
                                  "(fset 'project-vc-dir"
	                              "(fset 'files--async-shell-native-available-p"
                              "(fset 'files--async-shell-poll"
		                      "(setq files--keymap-source"
                      "(setq files--minibuffer-keymap-source"
                      "(fset 'files--lookup-key-sequence"
			                      "(fset 'files--maybe-start-minibuffer-from-keymap"
	                                  "M-!\\tshell-command\\tShell command: "
	                                  "M-|\\tshell-command-on-region\\tShell command on region: "
                                      "M-&\\tasync-shell-command\\tAsync shell command: "
                                      "C-x p !\\tproject-shell-command\\tProject shell command: "
                                      "C-x p &\\tproject-async-shell-command\\tProject async shell command: "
                                      "C-x p e\\tproject-eshell"
                                      "C-x p s\\tproject-shell"
                                      "C-x p c\\tproject-compile\\tProject compile command: "
                                      "C-x p G\\tproject-or-external-find-regexp\\tFind regexp in project or external roots: "
                                      "C-x p g\\tproject-find-regexp\\tFind regexp in project: "
                                      "C-x p v\\tproject-vc-dir"
				                      "(fset 'files--dispatch-key-sequence"
		                      "(setq files--quoted-insert-p"
		                      "(fset 'files--quoted-insert-key-text"
		                      "(fset 'nemacs-gui-file-bridge-session-run"
	                      "(fset 'describe-function"
		                      "(fset 'describe-variable"
		                      "(fset 'describe-key"
			                      "(fset 'describe-key-briefly"
			                      "(fset 'describe-bindings"
				                      "(fset 'help-for-help"
				                      "(fset 'files--show-static-help"
				                      "(fset 'describe-coding-system"
				                      "(fset 'describe-input-method"
				                      "(fset 'describe-language-environment"
				                      "(fset 'apropos-command"
				                      "(fset 'apropos-documentation"
				                      "(fset 'view-echo-area-messages"
				                      "(fset 'about-emacs"
			                      "(fset 'describe-copying"
			                      "(fset 'view-emacs-debugging"
			                      "(fset 'view-external-packages"
			                      "(fset 'view-emacs-FAQ"
			                      "(fset 'view-emacs-news"
			                      "(fset 'describe-distribution"
			                      "(fset 'view-emacs-problems"
			                      "(fset 'view-emacs-todo"
			                      "(fset 'describe-no-warranty"
				                      "(fset 'describe-gnu-project"
				                      "(fset 'view-hello-file"
				                      "(fset 'view-lossage"
				                      "(fset 'describe-mode"
				                      "(fset 'describe-symbol"
				                      "(fset 'help-quit"
				                      "(fset 'describe-syntax"
				                      "(fset 'help-with-tutorial"
                                      "(fset 'display-local-help"
                                      "(fset 'help-find-source"
                                      "(fset 'help-quick-toggle"
                                      "(fset 'search-forward-help-for-help"
                                      "(fset 'eval-last-sexp"
                                      "(fset 'eval-expression"
                                      "(fset 'repeat-complex-command"
                                      "(fset 'font-lock-update"
                                      "(fset 'insert-char"
                                      "(fset 'xref-go-back"
                                      "(fset 'xref-go-forward"
                                      "(fset 'xref-find-definitions"
                                      "(fset 'xref-find-references"
                                      "(fset 'xref-find-apropos"
                                      "(fset 'xref-find-definitions-other-window"
                                      "(fset 'xref-find-definitions-other-frame"
                                      "(fset 'next-error"
                                      "(fset 'previous-error"
                                      "(fset 'info"
                                      "(fset 'info-other-window"
                                      "(fset 'info-emacs-manual"
                                      "(fset 'info-display-manual"
                                      "(fset 'view-order-manuals"
                                      "(fset 'Info-goto-emacs-command-node"
                                      "(fset 'Info-goto-emacs-key-command-node"
                                      "(fset 'info-lookup-symbol"
                                      "(fset 'describe-package"
                                      "(fset 'finder-by-keyword"
				                      "(fset 'where-is"
		                      "(fset 'describe-command"
		                      "(fset 'what-cursor-position"
                              "(fset 'repeat"
		                      "(fset 'universal-argument"
		                      "(fset 'digit-argument"
		                      "(fset 'negative-argument"
		                      "(fset 'files--execute-with-prefix-arg"
		                      "(fset 'files--write-prefix-arg-state"
		                      "(fset 'files--lookup-key-command-in-source"
		                      "(setq files--key-list-source"
		                      "(fset 'files--key-list-from-source"
		                      "(fset 'files--binding-list-from-source"
					                      "(fset 'files--write-transport-point"
                                      "(fset 'files--change-log-date-string"
						                      "(fset 'find-alternate-file"
                                      "(fset 'same-window-prefix"
                                      "(fset 'other-window-prefix"
                                      "(fset 'other-tab-prefix"
                                      "(fset 'other-frame-prefix"
						                      "(fset 'find-file-read-only"
				                      "(fset 'find-file-other-window"
				                      "(fset 'find-file-read-only-other-window"
                                      "(fset 'find-file-other-frame"
                                      "(fset 'find-file-read-only-other-frame"
		                                  "(fset 'find-file-other-tab"
	                                          "(fset 'find-file-read-only-other-tab"
                                      "(fset 'add-change-log-entry-other-window"
                                      "(fset 'project-or-external-find-file"
					                      "(fset 'toggle-read-only"
				                      "(fset 'read-only-mode"
				                      "(fset 'insert-file"
                                      "(fset 'insert-buffer"
                                      "(fset 'point-to-register"
                                      "(fset 'jump-to-register"
                                      "(fset 'frameset-to-register"
                                      "(fset 'window-configuration-to-register"
                                      "(fset 'copy-to-register"
                                      "(fset 'insert-register"
                                      "(fset 'number-to-register"
                                      "(fset 'increment-register"
                                      "(fset 'bookmark-set"
                                      "(fset 'bookmark-set-no-overwrite"
                                      "(fset 'bookmark-jump"
                                      "(fset 'bookmark-bmenu-list"
                                      "(fset 'copy-rectangle-to-register"
                                      "(fset 'copy-rectangle-as-kill"
                                      "(fset 'rectangle-number-lines"
                                      "(fset 'kill-rectangle"
                                      "(fset 'delete-rectangle"
                                      "(fset 'clear-rectangle"
                                      "(fset 'open-rectangle"
                                      "(fset 'string-rectangle"
                                      "(fset 'yank-rectangle"
				                      "(fset 'basic-save-buffer"
					                      "(fset 'save-some-buffers"
                                      "(fset 'list-directory"
                                      "(fset 'dired"
                                      "(fset 'dired-jump"
                                      "(fset 'dired-jump-other-window"
                                      "(fset 'dired-other-window"
                                      "(fset 'dired-other-frame"
                                              "(fset 'dired-other-tab"
                                      "(fset 'dired-mark"
                                      "(fset 'dired-unmark"
                                      "(fset 'dired-flag-file-deletion"
                                      "(fset 'dired-do-flagged-delete"
                                      "(fset 'dired-do-rename"
                                      "(fset 'dired-do-copy"
                                      "(fset 'org-todo"
                                      "(fset 'org-narrow-to-subtree"
                                      "(fset 'org-table-next-field"
                                      "(fset 'org-capture"
                                      "(fset 'org-agenda"
                                      "(fset 'magit-status"
                                      "(fset 'magit-stage-file"
                                      "(fset 'magit-unstage-file"
                                      "(fset 'magit-commit"
                                      "(fset 'magit-diff"
                                      "(fset 'magit-log"
                                      "(fset 'files--tramp-read-file"
                                      "(fset 'files--tramp-write-file"
                                      "(fset 'org-cycle"
                                      "(fset 'org-shifttab"
                                      "(fset 'org-table-align"
                                      "(fset 'files--mode-keymap-source"
                                      "(fset 'compose-mail"
                                      "(fset 'compose-mail-other-window"
                                      "(fset 'compose-mail-other-frame"
                                      "(fset 'calc-dispatch"
                                      "(fset '2C-command"
                                      "(fset '2C-two-columns"
                                      "(fset '2C-associate-buffer"
                                      "(fset '2C-split"
                                              "(fset 'project-find-dir"
                                              "(fset 'project-dired"
                                              "(fset 'project-any-command"
                                              "(fset 'project-execute-extended-command"
                                              "(fset 'project-other-window-command"
                                              "(fset 'project-other-tab-command"
                                              "(fset 'project-other-frame-command"
                                              "(fset 'project-switch-project"
								                      "(fset 'switch-to-buffer"
							                      "(fset 'switch-to-buffer-other-window"
                                              "(fset 'switch-to-buffer-other-tab"
                                              "(fset 'project-switch-to-buffer"
		                                          "(fset 'rename-buffer"
                                          "(fset 'rename-uniquely"
                                          "(fset 'clone-buffer"
                                          "(fset 'clone-indirect-buffer-other-window"
						                      "(fset 'kill-buffer"
				                      "(fset 'kill-buffer-and-window"
                                              "(fset 'project-kill-buffers"
					                      "(fset 'list-buffers"
                                              "(fset 'project-list-buffers"
					                      "(fset 'occur"
                                          "(fset 'imenu"
					                      "(fset 'save-buffers-kill-terminal"
			                      "(fset 'save-buffers-kill-emacs"
			                      "(fset 'kill-emacs"
			                      "(fset 'forward-char"
                      "(fset 'backward-char"
                      "(fset 'beginning-of-buffer"
                      "(fset 'end-of-buffer"
	                      "(fset 'beginning-of-line"
	                      "(fset 'back-to-indentation"
	                      "(fset 'end-of-line"
	                      "(fset 'move-beginning-of-line"
		                      "(fset 'move-end-of-line"
		                      "(fset 'goto-line"
		                      "(fset 'goto-line-relative"
                              "(fset 'narrow-to-defun"
                              "(fset 'narrow-to-region"
                              "(fset 'narrow-to-page"
                              "(fset 'widen"
                              "(fset 'kmacro-start-macro"
                              "(fset 'kmacro-end-macro"
                              "(fset 'kmacro-end-and-call-macro"
                              "(fset 'kbd-macro-query"
                              "(fset 'files--read-kmacro-state"
                              "(fset 'files--write-kmacro-state"
			                      "(fset 'move-to-column"
		                      "(fset 'next-line"
		                      "(fset 'previous-line"
		                      "(fset 'set-goal-column"
		                      "(fset 'scroll-up-command"
	                      "(fset 'scroll-down-command"
	                      "(fset 'scroll-left"
	                      "(fset 'scroll-right"
                          "(fset 'files--read-transport-tab-state"
                          "(fset 'files--write-transport-tab-state"
                          "(fset 'files--read-transport-frame-state"
                          "(fset 'files--write-transport-frame-state"
                          "(fset 'files--read-transport-window-dedicated-state"
                          "(fset 'files--write-transport-window-dedicated-state"
                          "(fset 'files--read-transport-side-windows-state"
                          "(fset 'files--write-transport-side-windows-state"
                          "(fset 'files--read-transport-frame-undo-state"
                          "(fset 'files--write-transport-frame-undo-state"
                          "(fset 'files--read-transport-tab-undo-state"
                          "(fset 'files--write-transport-tab-undo-state"
                          "(fset 'tab-new"
                          "(fset 'tab-new-to"
                          "(fset 'tab-group"
                          "(fset 'delete-frame"
                          "(fset 'delete-other-frames"
                          "(fset 'make-frame-command"
                          "(fset 'other-frame"
                          "(fset 'clone-frame"
                          "(fset 'undelete-frame"
                          "(fset 'tab-undo"
                          "(fset 'tab-move"
                          "(fset 'tab-move-to"
                          "(fset 'tab-detach"
                          "(fset 'tab-window-detach"
                          "(fset 'tab-close"
                          "(fset 'tab-close-other"
                          "(fset 'tab-next"
                          "(fset 'tab-previous"
                          "(fset 'tab-duplicate"
                          "(fset 'tab-switch"
                          "(fset 'tab-rename"
	                      "(fset 'scroll-other-window"
	                      "(fset 'scroll-other-window-down"
	                      "(fset 'recenter-top-bottom"
	                      "(fset 'move-to-window-line-top-bottom"
	                      "(fset 'reposition-window"
	                      "(fset 'recenter-other-window"
					                      "(fset 'isearch-forward"
				                      "(fset 'isearch-backward"
				                      "(fset 'isearch-forward-regexp"
				                      "(fset 'isearch-backward-regexp"
                              "(fset 'isearch-forward-symbol-at-point"
                              "(fset 'isearch-forward-thing-at-point"
                              "(fset 'isearch-forward-symbol"
                              "(fset 'isearch-forward-word"
				                      "(fset 'replace-string"
					                      "(fset 'replace-regexp"
					                      "(fset 'query-replace"
					                      "(fset 'query-replace-regexp"
                                          "(fset 'project-query-replace-regexp"
					                      "(fset 'files--query-replace-handle-key"
			                      "(fset 'keyboard-quit"
			                      "(fset 'keyboard-escape-quit"
			                      "(fset 'exit-recursive-edit"
			                      "(fset 'abort-recursive-edit"
	                      "(fset 'delete-other-windows"
		                      "(fset 'delete-window"
		                      "(fset 'split-window-right"
		                      "(fset 'split-window-below"
		                      "(fset 'balance-windows"
		                      "(fset 'shrink-window-if-larger-than-buffer"
		                      "(fset 'fit-window-to-buffer"
                          "(fset 'delete-windows-on"
                          "(fset 'split-root-window-below"
                          "(fset 'split-root-window-right"
                          "(fset 'tear-off-window"
                          "(fset 'toggle-window-dedicated"
                          "(fset 'quit-window"
                          "(fset 'window-toggle-side-windows"
		                      "(fset 'enlarge-window"
		                      "(fset 'shrink-window-horizontally"
		                      "(fset 'enlarge-window-horizontally"
		                      "(fset 'other-window"
                      "(fset 'forward-word"
                      "(fset 'backward-word"
                      "(fset 'beginning-of-defun"
                      "(fset 'forward-sexp"
                      "(fset 'backward-sexp"
                      "(fset 'end-of-defun"
                      "(fset 'mark-defun"
                      "(fset 'mark-sexp"
                      "(fset 'kill-sexp"
                      "(fset 'down-list"
                      "(fset 'forward-list"
                      "(fset 'backward-list"
                      "(fset 'transpose-sexps"
                      "(fset 'backward-up-list"
		                      "(fset 'kill-word"
	                      "(fset 'backward-kill-word"
	                      "(fset 'zap-to-char"
                          "(fset 'expand-abbrev"
                          "(fset 'add-global-abbrev"
                          "(fset 'add-mode-abbrev"
                          "(fset 'inverse-add-global-abbrev"
                          "(fset 'inverse-add-mode-abbrev"
                          "(fset 'abbrev-prefix-mark"
                          "(fset 'expand-jump-to-next-slot"
                          "(fset 'expand-jump-to-previous-slot"
	                      "(fset 'dabbrev-expand"
	                      "(fset 'dabbrev-completion"
	                      "(fset 'complete-symbol"
	                      "(fset 'transpose-words"
	                      "(fset 'insert-parentheses"
                      "(fset 'move-past-close-and-reindent"
                      "(fset 'transpose-lines"
                      "(fset 'mark-word"
                      "(fset 'count-words-region"
                      "(fset 'count-lines-page"
                      "(fset 'forward-paragraph"
                      "(fset 'backward-paragraph"
		                      "(fset 'mark-paragraph"
		                      "(fset 'fill-paragraph"
		                      "(fset 'set-fill-column"
		                      "(fset 'set-fill-prefix"
		                      "(fset 'forward-sentence"
                      "(fset 'backward-sentence"
                      "(fset 'kill-sentence"
	                      "(fset 'backward-kill-sentence"
	                      "(fset 'transpose-chars"
	                      "(fset 'delete-horizontal-space"
		                      "(fset 'cycle-spacing"
		                      "(fset 'not-modified"
		                      "(fset 'just-one-space"
		                      "(fset 'delete-indentation"
		                      "(fset 'comment-line"
		                      "(fset 'comment-set-column"
		                      "(fset 'comment-dwim"
	                      "(fset 'upcase-word"
                      "(fset 'downcase-word"
                      "(fset 'capitalize-word"
                      "(fset 'upcase-region"
                      "(fset 'downcase-region"
                      "(fset 'capitalize-region"
                      "(fset 'sort-lines"
                      "(fset 'delete-char"
                      "(fset 'backward-delete-char"
                      "(fset 'delete-backward-char"
                      "(fset 'self-insert-command"
	                      "(fset 'quoted-insert"
	                      "(fset 'indent-for-tab-command"
	                      "(fset 'tab-to-tab-stop"
	                      "(fset 'indent-region"
	                      "(fset 'indent-rigidly"
	                      "(fset 'newline"
		                      "(fset 'electric-newline-and-maybe-indent"
		                      "(fset 'default-indent-new-line"
	                      "(fset 'open-line"
	                      "(fset 'split-line"
	                      "(fset 'delete-blank-lines"
                      "(fset 'kill-line"
                      "(fset 'kill-whole-line"
                      "(fset 'yank"
                      "(fset 'yank-pop"
	                      "(fset 'set-mark-command"
	                      "(fset 'exchange-point-and-mark"
	                      "(fset 'pop-global-mark"
	                      "(fset 'rectangle-mark-mode"
	                      "(fset 'toggle-truncate-lines"
	                      "(fset 'mark-whole-buffer"
	                      "(fset 'mark-page"
	                      "(fset 'backward-page"
	                      "(fset 'forward-page"
	                      "(fset 'delete-region"
                      "(fset 'kill-region"
	                      "(fset 'copy-region-as-kill"
	                      "(fset 'kill-ring-save"
	                      "(fset 'append-next-kill"
	                      "(fset 'undo"
	                      "(fset 'undo-redo"
		                      "(fset 'files--save-undo-state"
	                      "(fset 'files--read-only-command-p"
	                      "(fset 'revert-buffer"
                          "(fset 'revert-buffer-quick"
                      "(command-execute)"
                      "(fset 'nemacs-gui-file-bridge-run"))
      (should (string-match-p (regexp-quote needle) source)))
    (dolist (needle '("(setq files--transport-dir \"/tmp\")"
                      "(fset 'files--transport-path"
                      "files--transport-name"))
      (should (string-match-p (regexp-quote needle) source)))
    (should (string-match-p "Runtime lambdas intentionally avoid" source))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/source-shape-tier-1-ui-smoke-contract ()
  "The checked-in bridge source should expose the Tier 1 UI smoke surface."
  (let ((source
         (nemacs-gui-file-bridge-runtime-test--slurp
          nemacs-gui-file-bridge-runtime-test--source)))
	    (dolist (needle '("(fset 'find-file"
		                      "(fset 'find-file-noselect"
				                      "(fset 'find-file-other-window"
				                      "(fset 'find-file-read-only-other-window"
                                      "(fset 'find-file-other-frame"
                                      "(fset 'find-file-read-only-other-frame"
		                                  "(fset 'find-file-other-tab"
	                                  "(fset 'find-file-read-only-other-tab"
                                      "(fset 'project-find-file"
                                      "(fset 'project-find-dir"
                                      "(fset 'project-dired"
                                      "(fset 'project-any-command"
                                      "(fset 'project-execute-extended-command"
                                      "(fset 'project-other-window-command"
                                      "(fset 'project-other-tab-command"
                                      "(fset 'project-other-frame-command"
                                      "(fset 'project-or-external-find-file"
                                      "(fset 'project-switch-project"
				                      "(fset 'save-buffer"
	                      "(fset 'write-file"
		                      "(fset 'insert-file"
			                      "(fset 'replace-string"
				                      "(fset 'replace-regexp"
				                      "(fset 'query-replace"
				                      "(fset 'query-replace-regexp"
                                      "(fset 'project-query-replace-regexp"
				                      "(fset 'sort-lines"
				                      "(fset 'switch-to-buffer"
				                      "(fset 'switch-to-buffer-other-window"
                                      "(fset 'switch-to-buffer-other-frame"
                                      "(fset 'display-buffer"
                                      "(fset 'display-buffer-other-frame"
                                  "(fset 'rename-buffer"
		                      "(fset 'kill-buffer"
	                      "(fset 'kill-buffer-and-window"
                              "(fset 'project-kill-buffers"
	                      "(fset 'self-insert-command"
                      "(fset 'command-execute"
                      "(fset 'call-interactively"))
      (ert-info ((format "Tier 1 callable %s" needle))
        (should (string-match-p (regexp-quote needle) source))))
    (dolist (needle '("nemacs-buf"
                      "nemacs-file"
                      "nemacs-buffer-name"
                      "nemacs-point"
	                      "nemacs-mark"
	                      "nemacs-window-hscroll"
                          "nemacs-window-dedicated"
                          "nemacs-side-windows-visible"
	                          "nemacs-tab-state"
	                          "nemacs-frame-state"
                              "nemacs-frame-undo-state"
	                          "nemacs-tab-undo-state"
	                      "nemacs-minibuffer-text"
	                      "nemacs-minibuffer-candidates"
	                      "nemacs-minibuffer-history"
	                      "nemacs-minibuffer-require-match"
	                      "nemacs-cursor"
	                      "nemacs-modeline"
                      "nemacs-status"
                      "nemacs-kill-ring"
                      "nemacs-kill-ring-index"
                      "files--transport-path"
	                      "files--bridge-status"))
      (ert-info ((format "Tier 1 bridge transport %s" needle))
        (should (string-match-p (regexp-quote needle) source))))
	                    (dolist (needle '("C-x C-f\\tfind-file\\tFind file: "
				                      "C-x 4 C-f\\tfind-file-other-window\\tFind file in other window: "
				                      "C-x 4 f\\tfind-file-other-window\\tFind file in other window: "
                                      "C-x 5 C-f\\tfind-file-other-frame\\tFind file in other frame: "
                                      "C-x 5 f\\tfind-file-other-frame\\tFind file in other frame: "
                                      "C-x t C-f\\tfind-file-other-tab\\tFind file in other tab: "
                                      "C-x t f\\tfind-file-other-tab\\tFind file in other tab: "
                                      "C-x p F\\tproject-or-external-find-file\\tFind project or external file: "
                                      "C-x p f\\tproject-find-file\\tFind file in project: "
                                      "C-x p d\\tproject-find-dir\\tFind directory in project: "
                                      "C-x p D\\tproject-dired"
                                      "C-x p o\\tproject-any-command\\tProject command: "
                                      "C-x p p\\tproject-switch-project\\tSwitch to project: "
                                      "C-x p x\\tproject-execute-extended-command\\tProject M-x "
				                      "C-x 4 r\\tfind-file-read-only-other-window\\tFind file read-only in other window: "
                                      "C-x 5 r\\tfind-file-read-only-other-frame\\tFind file read-only in other frame: "
                                      "C-x t C-r\\tfind-file-read-only-other-tab\\tFind file read-only in other tab: "
	                                      "C-x 4 1\\tsame-window-prefix"
                                      "C-x 4 4\\tother-window-prefix"
                                      "C-x 5 5\\tother-frame-prefix"
	                                      "C-x t t\\tother-tab-prefix"
                                      "C-x 4 p\\tproject-other-window-command\\tProject other window command: "
                                      "C-x 5 p\\tproject-other-frame-command\\tProject other frame command: "
                                      "C-x t p\\tproject-other-tab-command\\tProject other tab command: "
                                      "C-x 4 a\\tadd-change-log-entry-other-window"
		                              "C-x C-d\\tlist-directory\\tList directory: "
	                              "C-x d\\tdired\\tDired directory: "
	                                  "C-x 4 d\\tdired-other-window\\tDired directory in other window: "
                                      "C-x 5 d\\tdired-other-frame\\tDired directory in other frame: "
	                                  "C-x t d\\tdired-other-tab\\tDired directory in other tab: "
                                      "C-x m\\tcompose-mail"
                                      "C-x 4 m\\tcompose-mail-other-window"
                                      "C-x 5 m\\tcompose-mail-other-frame"
				                      "C-x C-w\\twrite-file\\tWrite file: "
				                      "C-x C-s\\tsave-buffer"
                                      "C-x C-j\\tdired-jump"
	                                      "C-x 4 C-j\\tdired-jump-other-window"
			                                      "C-x 4 b\\tswitch-to-buffer-other-window\\tSwitch to buffer in other window: "
                                                  "C-x 5 b\\tswitch-to-buffer-other-frame\\tSwitch to buffer in other frame: "
		                                          "C-x t b\\tswitch-to-buffer-other-tab\\tSwitch to buffer in other tab: "
                                              "C-x p b\\tproject-switch-to-buffer\\tSwitch to project buffer: "
                                              "C-x p C-b\\tproject-list-buffers"
			                                      "C-x 4 C-o\\tdisplay-buffer\\tDisplay buffer: "
                                                  "C-x 5 C-o\\tdisplay-buffer-other-frame\\tDisplay buffer in other frame: "
                                      "C-x 4 c\\tclone-indirect-buffer-other-window"
                                      "C-x x r\\trename-buffer\\tRename buffer: "
                                      "C-x x i\\tinsert-buffer\\tInsert buffer: "
                                      "C-x x g\\trevert-buffer-quick"
                                      "C-x x n\\tclone-buffer"
                                      "C-x x u\\trename-uniquely"
					                      "C-x f\\tset-fill-column\\tSet fill column: "
			                      "M-z\\tzap-to-char\\tZap to char: "
				                      "C-j\\telectric-newline-and-maybe-indent"
				                      "C-M-j\\tdefault-indent-new-line"
				                      "M-j\\tdefault-indent-new-line"
				                      "M-~\\tnot-modified"
				                      "C-M-o\\tsplit-line"
				                      "C-x 4 0\\tkill-buffer-and-window"
                                      "C-x p k\\tproject-kill-buffers"
				                      "C-w\\tkill-region"
		                      "M-w\\tkill-ring-save"
		                      "C-M-w\\tappend-next-kill"
		                      "C-M-f\\tforward-sexp"
		                      "C-M-b\\tbackward-sexp"
		                      "C-M-a\\tbeginning-of-defun"
		                      "C-M-e\\tend-of-defun"
		                      "C-M-h\\tmark-defun"
		                      "C-M-@\\tmark-sexp"
		                      "C-M-SPC\\tmark-sexp"
		                      "C-M-k\\tkill-sexp"
		                      "C-M-d\\tdown-list"
		                      "C-M-n\\tforward-list"
		                      "C-M-p\\tbackward-list"
		                      "C-M-t\\ttranspose-sexps"
		                      "C-M-u\\tbackward-up-list"
		                      "M-(\\tinsert-parentheses"
		                      "M-)\\tmove-past-close-and-reindent"
                              "C-x '\\texpand-abbrev"
                              "C-x a '\\texpand-abbrev"
                              "C-x a e\\texpand-abbrev"
                              "C-x a +\\tadd-mode-abbrev\\tAdd mode abbrev: "
                              "C-x a C-a\\tadd-mode-abbrev\\tAdd mode abbrev: "
                              "C-x a l\\tadd-mode-abbrev\\tAdd mode abbrev: "
                              "C-x a g\\tadd-global-abbrev\\tAdd global abbrev: "
                              "C-x a -\\tinverse-add-global-abbrev\\tExpansion for global abbrev: "
                              "C-x a i g\\tinverse-add-global-abbrev\\tExpansion for global abbrev: "
                              "C-x a i l\\tinverse-add-mode-abbrev\\tExpansion for mode abbrev: "
                              "C-x a n\\texpand-jump-to-next-slot"
                              "C-x a p\\texpand-jump-to-previous-slot"
                              "C-x *\\tcalc-dispatch"
                              "C-x 6\\t2C-command"
                              "C-x 6 2\\t2C-two-columns"
                              "C-x 6 b\\t2C-associate-buffer\\tAssociate buffer: "
                              "C-x 6 s\\t2C-split"
                              "M-'\\tabbrev-prefix-mark"
		                      "M-/\\tdabbrev-expand"
		                      "C-M-/\\tdabbrev-completion"
		                      "C-M-i\\tcomplete-symbol"
	                      "M-y\\tyank-pop"
			                      "C-q\\tquoted-insert"
			                      "M-m\\tback-to-indentation"
			                      "C-a\\tmove-beginning-of-line"
			                      "C-e\\tmove-end-of-line"
			                      "C-x C-q\\tread-only-mode"
			                      "C-M-v\\tscroll-other-window"
			                      "C-M-S-v\\tscroll-other-window-down"
			                      "M-r\\tmove-to-window-line-top-bottom"
				                      "C-M-l\\treposition-window"
				                      "C-M-S-l\\trecenter-other-window"
				                      "C-x +\\tbalance-windows"
				                      "C-x -\\tshrink-window-if-larger-than-buffer"
                                      "C-x w -\\tfit-window-to-buffer"
                                      "C-x w 0\\tdelete-windows-on\\tDelete windows on buffer: "
                                      "C-x w 2\\tsplit-root-window-below"
                                      "C-x w 3\\tsplit-root-window-right"
                                      "C-x w ^ f\\ttear-off-window"
                                      "C-x w d\\ttoggle-window-dedicated"
                                      "C-x w q\\tquit-window"
                                      "C-x w s\\twindow-toggle-side-windows"
				                      "C-x ^\\tenlarge-window"
				                      "C-x {\\tshrink-window-horizontally"
				                      "C-x }\\tenlarge-window-horizontally"
				                      "C-M-s\\tisearch-forward-regexp"
		                      "C-M-r\\tisearch-backward-regexp"
                          "M-s .\\tisearch-forward-symbol-at-point"
                          "M-s M-.\\tisearch-forward-thing-at-point"
                          "M-s _\\tisearch-forward-symbol"
                          "M-s w\\tisearch-forward-word"
                          "M-s o\\toccur"
                          "M-g i\\timenu"
		                      "M-t\\ttranspose-words"
	                      "C-x C-t\\ttranspose-lines"
	                      "M-@\\tmark-word"
	                      "M-=\\tcount-words-region"
                          "C-x l\\tcount-lines-page"
		                      "M-{\\tbackward-paragraph"
		                      "M-}\\tforward-paragraph"
		                      "M-h\\tmark-paragraph"
			                      "M-q\\tfill-paragraph"
			                      "C-x .\\tset-fill-prefix"
			                      "M-i\\ttab-to-tab-stop"
		                      "C-M-\\\\\\tindent-region"
		                      "C-x TAB\\tindent-rigidly"
			                      "M-SPC\\tcycle-spacing"
				                      "M-~\\tnot-modified"
				                      "M-;\\tcomment-dwim"
				                      "C-x ;\\tcomment-set-column"
				                      "C-x C-;\\tcomment-line"
			                      "C-x C-o\\tdelete-blank-lines"
                          "C-x C-u\\tupcase-region"
                          "C-x C-l\\tdowncase-region"
                          "C-x r M-w\\tcopy-rectangle-as-kill"
                          "C-x r N\\trectangle-number-lines"
                          "C-x r k\\tkill-rectangle"
                          "C-x r d\\tdelete-rectangle"
                          "C-x r c\\tclear-rectangle"
	                          "C-x r o\\topen-rectangle"
	                          "C-x r t\\tstring-rectangle\\tString rectangle: "
	                          "C-x r y\\tyank-rectangle"
                          "C-x r l\\tbookmark-bmenu-list"
                          "C-x z\\trepeat"
                          "C-x ESC ESC\\trepeat-complex-command"
                          "C-x M-:\\trepeat-complex-command"
                          "C-x x f\\tfont-lock-update"
	                      "C-x u\\tundo"
	                      "C-?\\tundo-redo"
		                      "C-_\\tundo"
		                      "C-M-_\\tundo-redo"
			                      "M-ESC ESC\\tkeyboard-escape-quit"
			                      "C-M-c\\texit-recursive-edit"
			                      "C-]\\tabort-recursive-edit"
		                      "M-X\\texecute-extended-command-for-buffer"
					                      "M-x\\texecute-extended-command\\tM-x "
					                      "M-X\\texecute-extended-command-for-buffer\\tM-X "
                                  "C-x r SPC\\tpoint-to-register\\tPoint to register: "
                                  "C-x r C-@\\tpoint-to-register\\tPoint to register: "
                                  "C-x r C-SPC\\tpoint-to-register\\tPoint to register: "
                                  "C-x r j\\tjump-to-register\\tJump to register: "
                                  "C-x r f\\tframeset-to-register\\tFrameset to register: "
                                  "C-x r w\\twindow-configuration-to-register\\tWindow configuration to register: "
                                  "C-x r s\\tcopy-to-register\\tCopy to register: "
                                  "C-x r x\\tcopy-to-register\\tCopy to register: "
                                  "C-x r i\\tinsert-register\\tInsert register: "
                                  "C-x r g\\tinsert-register\\tInsert register: "
                                  "C-x r n\\tnumber-to-register\\tNumber to register: "
                                  "C-x r +\\tincrement-register\\tIncrement register: "
                                  "C-x r m\\tbookmark-set\\tSet bookmark: "
                                  "C-x r M\\tbookmark-set-no-overwrite\\tSet bookmark: "
                                  "C-x r b\\tbookmark-jump\\tJump to bookmark: "
                                  "C-x r r\\tcopy-rectangle-to-register\\tCopy rectangle to register: "
                                  "C-x r t\\tstring-rectangle\\tString rectangle: "
				                      "C-h b\\tdescribe-bindings"
				                      "C-h ?\\thelp-for-help"
				                      "C-h C-h\\thelp-for-help"
				                      "C-h C\\tdescribe-coding-system"
				                      "C-h C-\\\\\\tdescribe-input-method"
				                      "C-h I\\tdescribe-input-method"
				                      "C-h L\\tdescribe-language-environment"
				                      "C-h a\\tapropos-command"
				                      "C-h d\\tapropos-documentation"
				                      "C-h e\\tview-echo-area-messages"
				                      "C-h C-a\\tabout-emacs"
			                      "C-h C-c\\tdescribe-copying"
			                      "C-h C-d\\tview-emacs-debugging"
			                      "C-h C-e\\tview-external-packages"
			                      "C-h C-f\\tview-emacs-FAQ"
			                      "C-h C-n\\tview-emacs-news"
			                      "C-h n\\tview-emacs-news"
			                      "C-h C-o\\tdescribe-distribution"
			                      "C-h C-p\\tview-emacs-problems"
			                      "C-h C-t\\tview-emacs-todo"
				                      "C-h C-w\\tdescribe-no-warranty"
				                      "C-h g\\tdescribe-gnu-project"
				                      "C-h h\\tview-hello-file"
				                      "C-h l\\tview-lossage"
				                      "C-h m\\tdescribe-mode"
				                      "C-h o\\tdescribe-symbol"
				                      "C-h q\\thelp-quit"
				                      "C-h s\\tdescribe-syntax"
				                      "C-h t\\thelp-with-tutorial"
                                      "C-h .\\tdisplay-local-help"
                                      "C-h 4 s\\thelp-find-source"
                                      "C-h C-q\\thelp-quick-toggle"
                                      "C-h C-s\\tsearch-forward-help-for-help"
                                      "C-x C-e\\teval-last-sexp"
                                      "M-:\\teval-expression\\tEval: "
                                      "M-ESC :\\teval-expression\\tEval: "
                                      "C-x 8 RET\\tinsert-char\\tUnicode (name or hex): "
                                      "C-x `\\tnext-error"
                                      "M-g n\\tnext-error"
                                      "M-g M-n\\tnext-error"
                                      "M-g p\\tprevious-error"
                                      "M-g M-p\\tprevious-error"
                                      "M-,\\txref-go-back"
                                      "C-M-,\\txref-go-forward"
                                      "M-.\\txref-find-definitions\\tFind definitions of: "
                                      "M-?\\txref-find-references\\tFind references of: "
                                      "C-h i\\tinfo"
                                      "C-h 4 i\\tinfo-other-window"
                                      "C-h r\\tinfo-emacs-manual"
                                      "C-h RET\\tview-order-manuals"
                                      "C-h p\\tfinder-by-keyword"
				                      "C-h c\\tdescribe-key-briefly\\tDescribe key briefly: "
                                      "C-h F\\tInfo-goto-emacs-command-node\\tInfo command node: "
                                      "C-h K\\tInfo-goto-emacs-key-command-node\\tInfo key node: "
                                      "C-h P\\tdescribe-package\\tDescribe package: "
                                      "C-h R\\tinfo-display-manual\\tDisplay manual: "
                                      "C-h S\\tinfo-lookup-symbol\\tLookup symbol: "
                                      "C-M-.\\txref-find-apropos\\tSearch for pattern (word list or regexp): "
                                      "C-x 4 .\\txref-find-definitions-other-window\\tFind definitions of: "
                                      "C-x 5 .\\txref-find-definitions-other-frame\\tFind definitions of: "
			                      "C-h w\\twhere-is\\tWhere is command: "
				                      "C-h x\\tdescribe-command\\tDescribe command: "
				                      "C-M-%\\tquery-replace-regexp\\tQuery replace regexp: "
                                      "C-x p r\\tproject-query-replace-regexp\\tProject query replace regexp: "
                                      "C-x p v\\tproject-vc-dir"
				                      "C-x =\\twhat-cursor-position"
			                      "C-x <\\tscroll-left"
			                      "C-x >\\tscroll-right"
                                  "C-x t 2\\ttab-new"
                                  "C-x 5 0\\tdelete-frame"
                                  "C-x 5 1\\tdelete-other-frames"
                                  "C-x 5 2\\tmake-frame-command"
                                  "C-x 5 c\\tclone-frame"
                                  "C-x 5 o\\tother-frame"
                                  "C-x 5 u\\tundelete-frame"
                                  "C-x t 0\\ttab-close"
                                  "C-x t 1\\ttab-close-other"
                                  "C-x t o\\ttab-next"
                                  "C-x t O\\ttab-previous"
                                  "C-x t N\\ttab-new-to"
                                  "C-x t G\\ttab-group\\tTab group: "
                                  "C-x t u\\ttab-undo"
                                  "C-x t M\\ttab-move-to"
                                  "C-x t m\\ttab-move"
                                  "C-x t ^ f\\ttab-detach"
                                  "C-x w ^ t\\ttab-window-detach"
                                  "C-x t n\\ttab-duplicate"
                                  "C-x t RET\\ttab-switch"
                                  "C-x t RET\\ttab-switch\\tSwitch to tab: "
                                  "C-x t r\\ttab-rename\\tRename tab to: "
			                      "C-u\\tuniversal-argument"
			                      "C-3\\tdigit-argument"
			                      "M-3\\tdigit-argument"
			                      "C-M-3\\tdigit-argument"
			                      "C--\\tnegative-argument"
			                      "M--\\tnegative-argument"
			                      "C-M--\\tnegative-argument"
					                      "C-@\\tset-mark-command"
					                      "C-x C-SPC\\tpop-global-mark"
					                      "C-x SPC\\trectangle-mark-mode"
					                      "C-x x t\\ttoggle-truncate-lines"
					                      "C-x C-p\\tmark-page"
					                      "C-x [\\tbackward-page"
					                      "C-x ]\\tforward-page"
					                      "M-g c\\tgoto-char\\tGoto char: "
					                      "M-g M-g\\tgoto-line\\tGoto line: "
                                          "C-x n g\\tgoto-line-relative\\tGoto line: "
                                          "C-x n d\\tnarrow-to-defun"
                                          "C-x n n\\tnarrow-to-region"
                                          "C-x n p\\tnarrow-to-page"
                                          "C-x n w\\twiden"
                                          "C-x (\\tkmacro-start-macro"
                                          "C-x )\\tkmacro-end-macro"
                                          "C-x e\\tkmacro-end-and-call-macro"
                                          "C-x q\\tkbd-macro-query"
						                      "M-g TAB\\tmove-to-column\\tMove to column: "
					                      "C-x C-n\\tset-goal-column"
				                      "(fset 'read-from-minibuffer"
		                      "(fset 'completing-read"
		                      "(fset 'emacs-minibuffer-read-from-minibuffer"
		                      "(fset 'emacs-minibuffer-completing-read"
		                      "(setq cmd \"\")"
		                      "emacs-minibuffer-gui-initial-input"
	                      "(fset 'emacs-minibuffer-gui--collection-lines"
	                      "(fset 'emacs-minibuffer-gui-begin-read"
	                      "(fset 'emacs-minibuffer-gui-complete"
	                      "(files--lookup-key-sequence)"
                      "(if (equal files--bridge-keys \"TAB\")"))
      (ert-info ((format "Tier 1 key/minibuffer dispatch %s" needle))
        (should (string-match-p (regexp-quote needle) source))))
    (dolist (needle '("(if (equal files--bridge-keys \"C-x C-s\")"
                      "(if (equal files--bridge-keys \"C-x C-w\")"
                      "(if (equal files--bridge-keys \"M-x\")"
                      "(if (equal files--bridge-keys \"C-h f\")"
                      "(if (equal files--bridge-keys \"M-g g\")"))
      (ert-info ((format "Tier 1 dispatch should not hard-code %s" needle))
        (should-not (string-match-p (regexp-quote needle) source))))
		    (dolist (needle '("(if (equal cmd \"find-file\")"
				                      "(if (equal cmd \"find-file-other-window\")"
				                      "(if (equal cmd \"find-file-read-only-other-window\")"
                                      "(if (equal cmd \"find-file-other-frame\")"
                                      "(if (equal cmd \"find-file-read-only-other-frame\")"
			                                  "(if (equal cmd \"find-file-other-tab\")"
		                                  "(if (equal cmd \"find-file-read-only-other-tab\")"
                                      "(if (equal cmd \"project-find-file\")"
                                      "(if (equal cmd \"project-find-dir\")"
                                      "(if (equal cmd \"project-dired\")"
                                      "(if (equal cmd \"project-switch-project\")"
	                                      "(if (equal cmd \"add-change-log-entry-other-window\")"
			                      "(if (equal cmd \"save-buffer\")"
		                      "(if (equal cmd \"write-file\")"
                                      "(if (equal cmd \"frameset-to-register\")"
                                      "(if (equal cmd \"window-configuration-to-register\")"
                                      "(if (equal cmd \"expand-abbrev\")"
                                      "(if (equal cmd \"add-global-abbrev\")"
                                      "(if (equal cmd \"add-mode-abbrev\")"
                                      "(if (equal cmd \"inverse-add-global-abbrev\")"
                                      "(if (equal cmd \"inverse-add-mode-abbrev\")"
                                      "(if (equal cmd \"abbrev-prefix-mark\")"
                                      "(if (equal cmd \"expand-jump-to-next-slot\")"
                                      "(if (equal cmd \"expand-jump-to-previous-slot\")"
					                      "(if (equal cmd \"switch-to-buffer\")"
					                      "(if (equal cmd \"switch-to-buffer-other-window\")"
                                          "(if (equal cmd \"switch-to-buffer-other-frame\")"
			                                  "(if (equal cmd \"switch-to-buffer-other-tab\")"
                                          "(if (equal cmd \"project-switch-to-buffer\")"
                                          "(if (equal cmd \"project-list-buffers\")"
	                                          "(if (equal cmd \"imenu\")"
	                                      "(if (equal cmd \"dired-other-frame\")"
		                                      "(if (equal cmd \"dired-other-tab\")"
                                      "(if (equal cmd \"compose-mail\")"
                                      "(if (equal cmd \"compose-mail-other-window\")"
                                      "(if (equal cmd \"compose-mail-other-frame\")"
                                      "(if (equal cmd \"calc-dispatch\")"
                                      "(if (equal cmd \"2C-command\")"
                                      "(if (equal cmd \"2C-two-columns\")"
                                      "(if (equal cmd \"2C-associate-buffer\")"
                                      "(if (equal cmd \"2C-split\")"
			                              "(if (equal cmd \"display-buffer\")"
	                                      "(if (equal cmd \"display-buffer-other-frame\")"
                                      "(if (equal cmd \"delete-frame\")"
                                      "(if (equal cmd \"delete-other-frames\")"
                                      "(if (equal cmd \"make-frame-command\")"
                                      "(if (equal cmd \"other-frame\")"
                                      "(if (equal cmd \"clone-frame\")"
                                      "(if (equal cmd \"undelete-frame\")"
	                              "(if (equal cmd \"narrow-to-defun\")"
                              "(if (equal cmd \"narrow-to-region\")"
                              "(if (equal cmd \"narrow-to-page\")"
                              "(if (equal cmd \"widen\")"
                              "(if (equal cmd \"kmacro-start-macro\")"
                              "(if (equal cmd \"kmacro-end-and-call-macro\")"
		                              "(if (equal cmd \"rename-buffer\")"
			                      "(if (equal cmd \"kill-buffer\")"
                                      "(if (equal cmd \"project-kill-buffers\")"
			                      "(if (equal cmd \"balance-windows\")"
			                      "(if (equal cmd \"shrink-window-if-larger-than-buffer\")"
                                  "(if (equal cmd \"fit-window-to-buffer\")"
                                  "(if (equal cmd \"delete-windows-on\")"
                                  "(if (equal cmd \"split-root-window-below\")"
                                  "(if (equal cmd \"split-root-window-right\")"
                                  "(if (equal cmd \"tear-off-window\")"
                                  "(if (equal cmd \"toggle-window-dedicated\")"
                                  "(if (equal cmd \"quit-window\")"
                                  "(if (equal cmd \"window-toggle-side-windows\")"
			                      "(if (equal cmd \"enlarge-window\")"
			                      "(if (equal cmd \"shrink-window-horizontally\")"
			                      "(if (equal cmd \"enlarge-window-horizontally\")"
                                  "nemacs-window-split-delta"
	                      "(if (equal cmd \"self-insert-command\")"))
      (ert-info ((format "Tier 1 UI result writer %s" needle))
        (should (string-match-p (regexp-quote needle) source))))
    (should (string-match-p
             (regexp-quote "(setq files--bridge-status \"unsupported\")")
             source))
    (should (string-match-p
             (regexp-quote "(nl-write-file (progn (setq files--transport-name \"nemacs-status\") (files--transport-path)) files--bridge-status)")
             source))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-transport-dir-override ()
  "Standalone bridge can run against an isolated transport directory."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (transport-dir (make-temp-file "nemacs-gui-transport-" t)))
      (unwind-protect
          (progn
            (nemacs-gui-file-bridge-runtime-test--with-transport
              (write-region "42" nil "/tmp/nemacs-point" nil 'silent)
              (dolist (entry '(("nemacs-cmd" . "forward-char")
                               ("nemacs-keys" . "")
                               ("nemacs-arg" . "")
                               ("nemacs-buf" . "abc\n")
                               ("nemacs-point" . "0")
                               ("nemacs-mark" . "0")
                               ("nemacs-read-only" . "0")
                               ("nemacs-buffer-name" . "main")
	                               ("nemacs-window-layout" . "single")
	                               ("nemacs-window-selected" . "0")
	                               ("nemacs-window-start" . "0")
	                               ("nemacs-goal-column" . "")))
                (write-region (cdr entry) nil
                              (expand-file-name (car entry) transport-dir)
                              nil 'silent))
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image
               (format "(progn (setq files--transport-dir %S) (nemacs-gui-file-bridge-run))"
                       transport-dir))
              (should (equal "00001"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              (expand-file-name "nemacs-point" transport-dir))))
              (should (equal "42"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))))
        (delete-file image)
        (when (file-directory-p transport-dir)
          (delete-directory transport-dir t))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-tab-transport ()
  "Standalone bridge should persist tab state through /tmp/nemacs-tab-state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x t 2" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1\t2\t2"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "1\t2\t2" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "tab-next" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0\t2\t1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "0\t2\t1" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "C-x t N" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1\t3\t2"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "1\t3\t2" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "tab-new-to" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0\t4\t1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "1\t4\t2" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "C-x t m" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "2\t4\t3"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "2\t4\twork" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "tab-move" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "-2" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0\t4\twork"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "0\t4\t1" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "C-- C-x t m" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0\t4\t1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-prefix-arg")))
            (write-region "0\t4\t1" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "tab-move-to" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "2\t4\t3"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "2\t4\twork" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "tab-move-to" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "-1" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "3\t4\twork"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "0\t2\t1" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "tab-rename" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "work" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0\t2\twork"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "0\t2\twork" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "tab-group" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "build" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0\t2\twork\tbuild"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "0\t2\twork\tbuild" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "tab-group" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0\t2\twork"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "0\t2\twork" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "C-x t O" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1\t2\t2"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "1\t2\t2" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "tab-duplicate" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "2\t3\t2"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "2\t3\t2" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "tab-switch" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0\t3\t1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "0\t3\t1" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "tab-switch" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "work" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0\t3\twork"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "0\t3\twork" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "C-x t RET" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-active")))
            (should (equal "tab-switch"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-purpose")))
            (should (equal "Switch to tab: "
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-prompt")))
            (write-region "2\t3\t2" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-purpose" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-prompt" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "2\t3\t2" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x t 1" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0\t1\t2"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (write-region "1\t2\t2" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "C-x t 0" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "0\t1\t1"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-tab-state")))
            (should (equal "1\t2"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-undo-state")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "C-x t u" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1\t2\t2"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-undo-state")))
            (write-region "1\t3\twork\tbuild" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-tab-undo-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "C-x t ^ f" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1\t2\t2"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (should (equal "1\twork\tbuild"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-undo-state")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "C-x t u" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1\t3\twork\tbuild"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-undo-state")))
            (write-region "0\t2\twork" nil "/tmp/nemacs-tab-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-tab-undo-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "C-x w ^ t" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "2\t3\t3"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-state")))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-tab-undo-state")))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-frame-undo-state" nil 'silent)
            (write-region "0\t1\t1" nil "/tmp/nemacs-frame-state" nil 'silent)
            (write-region "make-frame-command" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1\t2\t2"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-frame-state")))
            (write-region "0\t2\t1" nil "/tmp/nemacs-frame-state" nil 'silent)
            (write-region "clone-frame" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "2\t3\t3"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-frame-state")))
            (write-region "0\t3\t1" nil "/tmp/nemacs-frame-state" nil 'silent)
            (write-region "other-frame" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1\t3\t2"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-frame-state")))
            (write-region "2\t3\t3" nil "/tmp/nemacs-frame-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-frame-undo-state" nil 'silent)
            (write-region "delete-frame" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1\t2\t2"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-frame-state")))
            (should (equal "2\t3"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-frame-undo-state")))
            (write-region "undelete-frame" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "2\t3\t3"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-frame-state")))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-frame-undo-state")))
            (write-region "2\t4\t3" nil "/tmp/nemacs-frame-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-frame-undo-state" nil 'silent)
            (write-region "delete-other-frames" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0\t1\t1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-frame-state")))
            (should (equal "1\t2"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-frame-undo-state")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "C-x t C-f" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-active")))
            (should (equal "find-file-other-tab"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-purpose")))
            (should (equal "Find file in other tab: "
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-prompt")))
            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-purpose" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-prompt" nil 'silent)
	            (write-region "C-x t C-r" nil "/tmp/nemacs-keys" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "find-file-read-only-other-tab"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-minibuffer-purpose")))
	            (should (equal "Find file read-only in other tab: "
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-minibuffer-prompt")))
                (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
                (write-region "" nil "/tmp/nemacs-minibuffer-purpose" nil 'silent)
                (write-region "" nil "/tmp/nemacs-minibuffer-prompt" nil 'silent)
                (write-region "C-x t d" nil "/tmp/nemacs-keys" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (should (equal "dired-other-tab"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-minibuffer-purpose")))
                (should (equal "Dired directory in other tab: "
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-minibuffer-prompt"))))
	        (delete-file image)))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-shell-command-key-starts-minibuffer ()
  "M-! should start a shell-command minibuffer in the standalone bridge."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (state (make-temp-file "nemacs-gui-shell-command-key-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq files--bridge-keys \"M-!\")
                 (files--maybe-start-minibuffer-from-keymap)
                 (files--write-minibuffer-state)
                 (nl-write-file %S
                                (concat files--minibuffer-purpose
                                        \"\\t\"
                                        files--minibuffer-prompt
                                        \"\\t\"
                                        (if files--minibuffer-active \"1\" \"0\"))))"
              state))
            (should (equal "shell-command\tShell command: \t1"
                           (nemacs-gui-file-bridge-runtime-test--slurp state))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p state)
          (delete-file state))))))

	(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-shell-process-keys-start-minibuffer ()
	  "M-|, M-&, and project shell keys should start minibuffers."
	  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
	    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
	          (image (nemacs-gui-file-bridge-runtime-test--write-image))
	          (region-state
	           (make-temp-file "nemacs-gui-shell-region-key-"))
	          (async-state
	           (make-temp-file "nemacs-gui-async-shell-key-"))
              (project-shell-state
               (make-temp-file "nemacs-gui-project-shell-key-"))
              (project-async-state
               (make-temp-file "nemacs-gui-project-async-shell-key-"))
              (project-compile-state
               (make-temp-file "nemacs-gui-project-compile-key-"))
              (project-grep-state
               (make-temp-file "nemacs-gui-project-grep-key-"))
              (project-or-external-grep-state
               (make-temp-file "nemacs-gui-project-or-external-grep-key-")))
	      (unwind-protect
	          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq files--bridge-keys \"M-|\")
                 (files--maybe-start-minibuffer-from-keymap)
                 (files--write-minibuffer-state)
                 (nl-write-file %S
                                (concat files--minibuffer-purpose
                                        \"\\t\"
                                        files--minibuffer-prompt
                                        \"\\t\"
                                        (if files--minibuffer-active \"1\" \"0\"))))"
              region-state))
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq files--bridge-keys \"M-&\")
                 (files--maybe-start-minibuffer-from-keymap)
                 (files--write-minibuffer-state)
                 (nl-write-file %S
	                                (concat files--minibuffer-purpose
	                                        \"\\t\"
	                                        files--minibuffer-prompt
	                                        \"\\t\"
	                                        (if files--minibuffer-active \"1\" \"0\"))))"
	              async-state))
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq files--bridge-keys \"C-x p !\")
                 (files--maybe-start-minibuffer-from-keymap)
                 (files--write-minibuffer-state)
                 (nl-write-file %S
                                (concat files--minibuffer-purpose
                                        \"\\t\"
                                        files--minibuffer-prompt
                                        \"\\t\"
                                        (if files--minibuffer-active \"1\" \"0\"))))"
              project-shell-state))
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq files--bridge-keys \"C-x p &\")
                 (files--maybe-start-minibuffer-from-keymap)
                 (files--write-minibuffer-state)
                 (nl-write-file %S
                                (concat files--minibuffer-purpose
                                        \"\\t\"
                                        files--minibuffer-prompt
                                        \"\\t\"
                                        (if files--minibuffer-active \"1\" \"0\"))))"
              project-async-state))
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq files--bridge-keys \"C-x p c\")
                 (files--maybe-start-minibuffer-from-keymap)
                 (files--write-minibuffer-state)
                 (nl-write-file %S
	                                (concat files--minibuffer-purpose
	                                        \"\\t\"
	                                        files--minibuffer-prompt
	                                        \"\\t\"
	                                        (if files--minibuffer-active \"1\" \"0\"))))"
              project-compile-state))
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq files--bridge-keys \"C-x p g\")
                 (files--maybe-start-minibuffer-from-keymap)
                 (files--write-minibuffer-state)
                 (nl-write-file %S
                                (concat files--minibuffer-purpose
                                        \"\\t\"
                                        files--minibuffer-prompt
                                        \"\\t\"
                                        (if files--minibuffer-active \"1\" \"0\"))))"
              project-grep-state))
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq files--bridge-keys \"C-x p G\")
                 (files--maybe-start-minibuffer-from-keymap)
                 (files--write-minibuffer-state)
                 (nl-write-file %S
                                (concat files--minibuffer-purpose
                                        \"\\t\"
                                        files--minibuffer-prompt
                                        \"\\t\"
                                        (if files--minibuffer-active \"1\" \"0\"))))"
              project-or-external-grep-state))
		            (should (equal "shell-command-on-region\tShell command on region: \t1"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            region-state)))
	            (should (equal "async-shell-command\tAsync shell command: \t1"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            async-state)))
                (should (equal "project-shell-command\tProject shell command: \t1"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                project-shell-state)))
                (should (equal "project-async-shell-command\tProject async shell command: \t1"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                project-async-state)))
	                (should (equal "project-compile\tProject compile command: \t1"
	                               (nemacs-gui-file-bridge-runtime-test--slurp
	                                project-compile-state)))
                (should (equal "project-find-regexp\tFind regexp in project: \t1"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                project-grep-state)))
                (should (equal "project-or-external-find-regexp\tFind regexp in project or external roots: \t1"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                project-or-external-grep-state))))
	        (when (file-exists-p image)
	          (delete-file image))
		        (dolist (file (list region-state async-state
	                               project-shell-state project-async-state
	                               project-compile-state project-grep-state
                                   project-or-external-grep-state))
	          (when (file-exists-p file)
	            (delete-file file)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-project-shell-buffer-facades ()
  "Project shell commands should open durable project shell buffers."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (project-dir "/tmp/nemacs-project-interactive-shell-test")
          (project-file "/tmp/nemacs-project-interactive-shell-test/sub/file.txt"))
      (unwind-protect
          (progn
            (when (file-directory-p project-dir)
              (delete-directory project-dir t))
            (make-directory (file-name-directory project-file) t)
            (write-region "project\n" nil project-file nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--with-transport
              (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
              (write-region project-file nil "/tmp/nemacs-file" nil 'silent)
              (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
              (write-region "old\n" nil "/tmp/nemacs-buf" nil 'silent)
              (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
              (write-region "project-shell" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "*shell*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (string-match-p
                       (regexp-quote "Project directory: /tmp/nemacs-project-interactive-shell-test/sub")
                       (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-suffix-p
                       "$ "
                       (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (equal "0"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-read-only")))
              (write-region project-file nil "/tmp/nemacs-file" nil 'silent)
              (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
              (write-region "old\n" nil "/tmp/nemacs-buf" nil 'silent)
              (write-region "project-eshell" nil "/tmp/nemacs-cmd" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "*eshell*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (string-match-p
                       (regexp-quote "Project directory: /tmp/nemacs-project-interactive-shell-test/sub")
                       (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-suffix-p
                       "eshell> "
                       (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-directory-p project-dir)
          (delete-directory project-dir t))))))

	(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-shell-process-direct-smoke ()
	  "Direct shell/process commands should use the NeLisp call-process substrate."
	  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
	    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
	          (image (nemacs-gui-file-bridge-runtime-test--write-image))
	          (probe-file "/tmp/nemacs-call-process-probe")
              (project-dir "/tmp/nemacs-project-shell-test")
              (project-file "/tmp/nemacs-project-shell-test/sub/file.txt"))
	      (unwind-protect
	          (progn
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(nl-write-file %S (if (fboundp (quote nelisp-process-call-process)) \"1\" (if (fboundp (quote nelisp-call-process)) \"1\" \"0\")))"
              probe-file))
            (unless (equal "1" (nemacs-gui-file-bridge-runtime-test--slurp
                                probe-file))
              (ert-skip "standalone reader lacks a call-process substrate"))
            (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "seed" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-purpose" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-minibuffer-cursor" nil 'silent)
            (write-region "shell-command" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "printf shell-ok" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Shell Command Output*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "shell-ok"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "abc" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "shell-command-on-region" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "tr a-z A-Z" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "ABC"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
	            (write-region "async-shell-command" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "printf async-ok" nil "/tmp/nemacs-arg" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "*Async Shell Command*"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-name")))
	            (should (equal "async-ok"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
                (when (file-directory-p project-dir)
                  (delete-directory project-dir t))
                (make-directory (file-name-directory project-file) t)
                (write-region "project" nil project-file nil 'silent)
                (write-region "project-shell-command" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region project-file nil "/tmp/nemacs-file" nil 'silent)
                (write-region "pwd" nil "/tmp/nemacs-arg" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (should (equal "*Shell Command Output*"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-buffer-name")))
                (should (string-match-p
                         (regexp-quote "Project directory: /tmp/nemacs-project-shell-test/sub")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (string-match-p
                         (regexp-quote "/tmp/nemacs-project-shell-test/sub")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (write-region "project-async-shell-command" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region project-file nil "/tmp/nemacs-file" nil 'silent)
                (write-region "printf project-async-ok" nil "/tmp/nemacs-arg" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (should (equal "*Async Shell Command*"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-buffer-name")))
                (should (string-match-p
                         "project-async-ok"
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (write-region "project-compile" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region project-file nil "/tmp/nemacs-file" nil 'silent)
                (write-region "printf project-compile-ok" nil "/tmp/nemacs-arg" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (should (equal "*compilation*"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-buffer-name")))
                (should (string-match-p
                         (regexp-quote "Project directory: /tmp/nemacs-project-shell-test/sub")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (string-match-p
                         (regexp-quote "Compile command: printf project-compile-ok")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (string-match-p
                         (regexp-quote "Exit status: 0")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (string-match-p
                         "project-compile-ok"
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (equal "1"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-read-only")))
                (make-directory (concat project-dir "/sub/nested") t)
                (write-region "beta hit\n" nil (concat project-dir "/sub/nested/other.txt") nil 'silent)
                (write-region "alpha hit\nskip\n" nil project-file nil 'silent)
                (write-region "project-find-regexp" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region project-file nil "/tmp/nemacs-file" nil 'silent)
                (write-region "hit" nil "/tmp/nemacs-arg" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (should (equal "*compilation*"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-buffer-name")))
                (should (string-match-p
                         (regexp-quote "Project directory: /tmp/nemacs-project-shell-test/sub")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (string-match-p
                         (regexp-quote "Find regexp: hit")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (string-match-p
                         (regexp-quote "Exit status: 0")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (string-match-p
                         (regexp-quote "./file.txt:1:alpha hit")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (string-match-p
                         (regexp-quote "./nested/other.txt:1:beta hit")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (equal "1"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-read-only")))
                (write-region "project-or-external-find-regexp" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region project-file nil "/tmp/nemacs-file" nil 'silent)
                (write-region "hit" nil "/tmp/nemacs-arg" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (should (equal "*compilation*"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-buffer-name")))
                (should (string-match-p
                         (regexp-quote "Project/external roots: /tmp/nemacs-project-shell-test/sub")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (string-match-p
                         (regexp-quote "Find regexp: hit")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (string-match-p
                         (regexp-quote "Exit status: 0")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (string-match-p
                         (regexp-quote "./file.txt:1:alpha hit")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (string-match-p
                         (regexp-quote "./nested/other.txt:1:beta hit")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buf")))
                (should (equal "1"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-read-only")))
                (when (executable-find "git")
                  (let ((untracked-file (concat project-dir "/sub/untracked.txt")))
                    (call-process "git" nil nil nil "-C" project-dir "init")
                    (call-process "git" nil nil nil "-C" project-dir "add" "sub/file.txt")
                    (write-region "changed\n" nil project-file nil 'silent)
                    (write-region "new\n" nil untracked-file nil 'silent)
                    (write-region "project-vc-dir" nil "/tmp/nemacs-cmd" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                    (write-region project-file nil "/tmp/nemacs-file" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (equal "*vc-dir*"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-buffer-name")))
                    (should (string-match-p
                             (regexp-quote "Project directory: /tmp/nemacs-project-shell-test/sub")
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
                    (should (string-match-p
                             (regexp-quote "VC root: /tmp/nemacs-project-shell-test")
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
                    (should (string-match-p
                             (regexp-quote "Exit status: 0")
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
                    (should (string-match-p
                             (regexp-quote "sub/file.txt")
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
                    (should (string-match-p
                             (regexp-quote "sub/untracked.txt")
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
                    (should (equal "1"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-read-only"))))))
	        (when (file-exists-p image)
	          (delete-file image))
	        (when (file-exists-p probe-file)
	          (delete-file probe-file))
            (when (file-directory-p project-dir)
              (delete-directory project-dir t)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-goto-line ()
  "In standalone NeLisp, goto-line should use the point transport."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
	          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-state" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-minibuffer-cursor" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
	            (write-region "one\ntwo\nthree\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "goto-line" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "goto-line" 4)
            (write-region "goto-line-relative" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "goto-line-relative" 8)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x n g" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "C-x n g" 4)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "goto-char" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "goto-char" 5)
            (write-region "move-to-column" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "a\tb\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "move-to-column" 2)
            (write-region "register source\n" nil "/tmp/nemacs-buffer-store/main" nil 'silent)
            (write-region "" nil "/tmp/nemacs-buffer-file-store/main" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-buffer-point-store/main" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-buffer-mark-store/main" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-buffer-window-start-store/main" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-buffer-read-only-store/main" nil 'silent)
            (write-region "point-to-register" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "a" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "register source\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "8" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "8\nmain\n1\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-register-store/97")))
            (write-region "other text\n" nil "/tmp/nemacs-buffer-store/other" nil 'silent)
            (write-region "" nil "/tmp/nemacs-buffer-file-store/other" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-buffer-point-store/other" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-buffer-mark-store/other" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-buffer-window-start-store/other" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-buffer-read-only-store/other" nil 'silent)
            (write-region "jump-to-register" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "a" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "other" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "other text\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "main"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "register source\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "jump-to-register" 8)
            (should (equal "00001"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "window-configuration-to-register" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "w" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "vertical" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-window-split-delta" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "window\nvertical\n1\n3\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-register-store/119")))
            (write-region "jump-to-register" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "w" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-split-delta" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "vertical"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-layout")))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-selected")))
            (should (equal "3"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-split-delta")))
            (write-region "frameset-to-register" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "f" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "1\t3\t3" nil "/tmp/nemacs-frame-state" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "frame\n1\t3\t3"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-register-store/102")))
            (write-region "jump-to-register" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "f" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "0\t1\t1" nil "/tmp/nemacs-frame-state" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1\t3\t3"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-frame-state")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x r SPC" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "b" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "3\nmain\n0\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-register-store/98")))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "copy-to-register" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "c" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "text\nbcd"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-register-store/99")))
            (write-region "insert-register" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "c" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "12\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1bcd2\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "insert-register" 4)
            (write-region "number-to-register" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "n" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abc 42 def\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "number\n42"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-register-store/110")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "number-to-register" 6)
            (write-region "insert-register" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "n" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "x\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "x42\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "increment-register" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "n" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "number\n43"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-register-store/110")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x r +" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "n" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "number\n44"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-register-store/110")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x r n" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "m" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "-7 zz\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "number\n-7"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-register-store/109")))
            (write-region "alpha\nbeta\ngamma\n" nil "/tmp/nemacs-bookmark-target.txt" nil 'silent)
            (write-region "bookmark-set" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "spot" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "/tmp/nemacs-bookmark-target.txt" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "alpha\nbeta\ngamma\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "/tmp/nemacs-bookmark-target.txt\nmain\n6\n0\nalpha\nbeta\ngamma\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-bookmark-store/115-112-111-116")))
            (write-region "bookmark-set-no-overwrite" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "spot" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "changed\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "/tmp/nemacs-bookmark-target.txt\nmain\n6\n0\nalpha\nbeta\ngamma\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-bookmark-store/115-112-111-116")))
            (write-region "bookmark-jump" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "spot" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "wrong\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "alpha\nbeta\ngamma\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "/tmp/nemacs-bookmark-target.txt"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-file")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "bookmark-jump" 6)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x r m" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "raw" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "/tmp/nemacs-bookmark-target.txt" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "alpha\nbeta\ngamma\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "11" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "/tmp/nemacs-bookmark-target.txt\nmain\n11\n0\nalpha\nbeta\ngamma\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-bookmark-store/114-97-119")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x r b" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "raw" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "C-x r b" 11)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x r l" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (string-match-p "Bookmark List"
                                    (nemacs-gui-file-bridge-runtime-test--slurp
                                     "/tmp/nemacs-buf")))
            (should (string-match-p "spot"
                                    (nemacs-gui-file-bridge-runtime-test--slurp
                                     "/tmp/nemacs-buf")))
            (should (string-match-p "raw"
                                    (nemacs-gui-file-bridge-runtime-test--slurp
                                     "/tmp/nemacs-buf")))
            (should (equal "*Bookmark List*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x r s" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "d" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "xyz\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "text\nxyz"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-register-store/100")))
            (write-region "text\nZZ" nil "/tmp/nemacs-register-store/101" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x r i" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "e" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "aa\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "aZZa\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "C-x r i" 3)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "copy-rectangle-as-kill" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abcd\nefgh\nijkl\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "13" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-rectangle-kill" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "bc\nfg\njk"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-rectangle-kill")))
            (should (equal "abcd\nefgh\nijkl\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "delete-rectangle" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "abcd\nefgh\nijkl\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "13" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "ad\neh\nil\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "delete-rectangle" 1)
            (write-region "clear-rectangle" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "abcd\nefgh\nijkl\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "13" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a  d\ne  h\ni  l\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "open-rectangle" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "abcd\nefgh\nijkl\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "13" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a  bcd\ne  fgh\ni  jkl\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x r k" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "abcd\nefgh\nijkl\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "13" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-rectangle-kill" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "ad\neh\nil\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "bc\nfg\njk"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-rectangle-kill")))
            (write-region "yank-rectangle" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "ad\neh\nil\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "abcd\nefgh\nijkl\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "yank-rectangle" 1)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x r y" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "ad\neh\nil\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "abcd\nefgh\nijkl\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "copy-rectangle-to-register" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "f" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abcd\nefgh\nijkl\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "13" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-rectangle-kill" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "rect\nbc\nfg\njk"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-register-store/102")))
            (write-region "insert-register" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "f" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "ad\neh\nil\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "abcd\nefgh\nijkl\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x r r" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "g" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abcd\nefgh\nijkl\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "13" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-rectangle-kill" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "rect\nbc\nfg\njk"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-register-store/103")))
            (write-region "rectangle-number-lines" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abcd\nefgh\nijkl\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "13" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a1 bcd\ne2 fgh\ni3 jkl\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x r N" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abcd\nefgh\nijkl\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "13" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a1 bcd\ne2 fgh\ni3 jkl\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "string-rectangle" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "XX" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abcd\nefgh\nijkl\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "13" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "aXXd\neXXh\niXXl\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x r t" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "Q" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abcd\nefgh\nijkl\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "13" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "aQd\neQh\niQl\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "execute-extended-command" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "forward-char" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "one\ntwo\nthree\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 5 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "describe-function" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "forward-char" nil "/tmp/nemacs-arg" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
		            (let ((help-buffer-name
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buffer-name"))
                    (help-status
                     (if (file-exists-p "/tmp/nemacs-status")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-status")
                       "<missing>")))
                (ert-info ((format "describe-function buffer=%S status=%S"
                                   help-buffer-name help-status))
                  (should (equal "*Help*" help-buffer-name))))
            (should (string-match-p
                     "forward-char is a function"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (string-match-p
                     "Move point one character forward"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-read-only")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (let ((raw-file (make-temp-file "nemacs-gui-file-bridge-raw-find-")))
              (unwind-protect
                  (progn
                    (write-region "raw find file\n" nil raw-file nil 'silent)
                    (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                    (write-region "C-x C-f" nil "/tmp/nemacs-keys" nil 'silent)
                    (write-region raw-file nil "/tmp/nemacs-minibuffer-text" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
                    (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (equal "raw find file\n"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-buf")))
	                    (should (equal raw-file
	                                   (nemacs-gui-file-bridge-runtime-test--slurp
	                                    "/tmp/nemacs-file")))
	                    (let ((raw-write-file
	                           (make-temp-file
	                            "nemacs-gui-file-bridge-raw-write-")))
	                      (unwind-protect
	                          (progn
	                            (write-region "raw write file\n"
	                                          nil "/tmp/nemacs-buf" nil 'silent)
	                            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
	                            (write-region "C-x C-w" nil "/tmp/nemacs-keys" nil 'silent)
	                            (write-region raw-write-file
	                                          nil "/tmp/nemacs-minibuffer-text" nil 'silent)
	                            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
	                            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
	                            (nemacs-gui-file-bridge-runtime-test--run-ok
	                             reader image "(nemacs-gui-file-bridge-run)")
	                            (should (equal "raw write file\n"
	                                           (nemacs-gui-file-bridge-runtime-test--slurp
	                                            raw-write-file)))
	                            (should (equal raw-write-file
	                                           (nemacs-gui-file-bridge-runtime-test--slurp
	                                            "/tmp/nemacs-file")))
	                            (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value))))
	                        (when (file-exists-p raw-write-file)
	                          (delete-file raw-write-file))))
	                    (let ((raw-alternate-file
	                           (make-temp-file
	                            "nemacs-gui-file-bridge-raw-alternate-")))
	                      (unwind-protect
	                          (progn
	                            (write-region "raw alternate file\n"
	                                          nil raw-alternate-file nil 'silent)
	                            (write-region "old raw alternate buffer\n"
	                                          nil "/tmp/nemacs-buf" nil 'silent)
	                            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
	                            (write-region "C-x C-v" nil "/tmp/nemacs-keys" nil 'silent)
	                            (write-region raw-alternate-file
	                                          nil "/tmp/nemacs-minibuffer-text" nil 'silent)
	                            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
	                            (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
	                            (nemacs-gui-file-bridge-runtime-test--run-ok
	                             reader image "(nemacs-gui-file-bridge-run)")
	                            (should (equal "raw alternate file\n"
	                                           (nemacs-gui-file-bridge-runtime-test--slurp
	                                            "/tmp/nemacs-buf")))
	                            (should (equal raw-alternate-file
	                                           (nemacs-gui-file-bridge-runtime-test--slurp
	                                            "/tmp/nemacs-file")))
	                            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value))))
	                        (when (file-exists-p raw-alternate-file)
	                          (delete-file raw-alternate-file))))
	                    (write-region "M-x" nil "/tmp/nemacs-keys" nil 'silent)
	                    (write-region "forward-char"
	                                  nil "/tmp/nemacs-minibuffer-text" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
                    (write-region "line one\nline two\nline three\n"
                                  nil "/tmp/nemacs-buf" nil 'silent)
                    (write-region "M-x" nil "/tmp/nemacs-keys" nil 'silent)
                    (write-region "goto-line"
                                  nil "/tmp/nemacs-minibuffer-text" nil 'silent)
                    (write-region "3"
                                  nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (= 18 (nemacs-gui-file-bridge-runtime-test--point-value)))
                    (write-region "M-g c" nil "/tmp/nemacs-keys" nil 'silent)
                    (write-region "6"
                                  nil "/tmp/nemacs-minibuffer-text" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
	                    (nemacs-gui-file-bridge-runtime-test--run-ok
	                     reader image "(nemacs-gui-file-bridge-run)")
		                    (should (= 5 (nemacs-gui-file-bridge-runtime-test--point-value)))
		                    (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
		                    (write-region "M-g TAB" nil "/tmp/nemacs-keys" nil 'silent)
		                    (write-region "2" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
		                    (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
		                    (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
		                    (write-region "" nil "/tmp/nemacs-minibuffer-state" nil 'silent)
		                    (write-region "0" nil "/tmp/nemacs-minibuffer-cursor" nil 'silent)
		                    (write-region "a\tb\n" nil "/tmp/nemacs-buf" nil 'silent)
		                    (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
		                    (nemacs-gui-file-bridge-runtime-test--run-ok
		                     reader image "(nemacs-gui-file-bridge-run)")
		                    (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
		                    (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
	                    (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-minibuffer-state" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-minibuffer-cursor" nil 'silent)
	                    (write-region "C-h f" nil "/tmp/nemacs-keys" nil 'silent)
                    (write-region "forward-char"
                                  nil "/tmp/nemacs-minibuffer-text" nil 'silent)
                    (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
	                    (nemacs-gui-file-bridge-runtime-test--run-ok
	                     reader image "(nemacs-gui-file-bridge-run)")
                    (let ((raw-help-buffer-name
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name"))
                          (raw-help-status
                           (if (file-exists-p "/tmp/nemacs-status")
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-status")
                             "<missing>")))
                      (ert-info ((format "raw C-h f buffer=%S status=%S"
                                         raw-help-buffer-name raw-help-status))
                        (should (equal "*Help*" raw-help-buffer-name))))
		                    (should (string-match-p
		                             "forward-char is a function"
		                             (nemacs-gui-file-bridge-runtime-test--slurp
		                              "/tmp/nemacs-buf"))))
                    (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-minibuffer-state" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-minibuffer-cursor" nil 'silent)
	                    (write-region "C-x =" nil "/tmp/nemacs-keys" nil 'silent)
                    ;; Preceding raw-find-file block changed window/file state;
                    ;; reset volatile transport to a clean baseline (as a
                    ;; front-end would re-send) so what-cursor-position opens
                    ;; *Help* instead of acting on stale window/file state.
                    (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-file" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
                    (write-region "one\ntwo\nthree\n"
                                  nil "/tmp/nemacs-buf" nil 'silent)
                    (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                    (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
	                    (nemacs-gui-file-bridge-runtime-test--run-ok
	                     reader image "(nemacs-gui-file-bridge-run)")
                    (let ((cursor-help-buffer-name
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
                      (ert-info ((format "raw C-x = buffer=%S"
                                         cursor-help-buffer-name))
                        (should (equal "*Help*" cursor-help-buffer-name))))
                    (let ((cursor-help
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
                      (should (string-match-p "Cursor position" cursor-help))
                      (should (string-match-p "Point: 00005" cursor-help))
                      (should (string-match-p "Line: 00002" cursor-help))
                      (should (string-match-p "Column: 00001" cursor-help))
                      (should (string-match-p "Buffer: main" cursor-help)))
                    (should (equal "1"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-read-only")))
                    (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                    (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
                    (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
                    (write-region "C-3" nil "/tmp/nemacs-keys" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (equal "3"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-prefix-arg")))
                    (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                    (write-region "C-f" nil "/tmp/nemacs-keys" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
                    (should (equal ""
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-prefix-arg")))
                    (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
                    (write-region "C--" nil "/tmp/nemacs-keys" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (equal "-"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-prefix-arg")))
                    (write-region "C-f" nil "/tmp/nemacs-keys" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value)))
                    (should (equal ""
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-prefix-arg")))
                    (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
                    (write-region "C-u" nil "/tmp/nemacs-keys" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (equal "4"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-prefix-arg")))
                    (write-region "C-f" nil "/tmp/nemacs-keys" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value)))
                    (write-region "" nil "/tmp/nemacs-buf" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
                    (write-region "M-2" nil "/tmp/nemacs-keys" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (equal "2"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-prefix-arg")))
                    (write-region "x" nil "/tmp/nemacs-keys" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (equal "xx"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-buf")))
                    (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
	                (when (file-exists-p raw-file)
	                  (delete-file raw-file))))
	            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-minibuffer-cursor" nil 'silent)
            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
	            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "one\ntwo\nthree\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "describe-key" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x C-f" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Help*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
	            (should (string-match-p
	                     "C-x C-f runs the command find-file"
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-buf")))
	            (write-region "describe-key" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "M-g M-g" nil "/tmp/nemacs-arg" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
		            (should (string-match-p
	                     "M-g M-g runs the command goto-line"
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-buf")))
	            (write-region "describe-key-briefly" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "C-x C-s" nil "/tmp/nemacs-arg" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (string-match-p
	                     "C-x C-s runs the command save-buffer"
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-buf")))
	            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "C-h c" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "C-x C-f" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
	            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (string-match-p
	                     "C-x C-f runs the command find-file"
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-buf")))
		            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region "C-h b" nil "/tmp/nemacs-keys" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
	            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (let ((bindings-help
	                   (nemacs-gui-file-bridge-runtime-test--slurp
	                    "/tmp/nemacs-buf")))
	              (should (string-match-p
	                       "Key bindings in the current GUI runtime"
	                       bindings-help))
	              (should (string-match-p
	                       "C-x C-s[	]save-buffer"
	                       bindings-help))
		              (should (string-match-p
		                       "C-h c[	]describe-key-briefly"
		                       bindings-help)))
		            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region "C-h ?" nil "/tmp/nemacs-keys" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
		            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
		            (let ((help-text
		                   (nemacs-gui-file-bridge-runtime-test--slurp
		                    "/tmp/nemacs-buf")))
		              (should (string-match-p
		                       "Help commands in the current GUI runtime"
		                       help-text))
		              (should (string-match-p
		                       "C-h b[	]describe-bindings"
		                       help-text)))
		            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region "C-h C-h" nil "/tmp/nemacs-keys" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
		            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
			            (should (string-match-p
			                     "C-h C-h[	]help-for-help"
			                     (nemacs-gui-file-bridge-runtime-test--slurp
			                      "/tmp/nemacs-buf")))
			            (write-region "about-emacs" nil "/tmp/nemacs-cmd" nil 'silent)
			            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
			            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
			            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
			            (nemacs-gui-file-bridge-runtime-test--run-ok
			             reader image "(nemacs-gui-file-bridge-run)")
			            (should (equal "*Help*"
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            "/tmp/nemacs-buffer-name")))
			            (should (string-match-p
			                     "About GNU Emacs"
			                     (nemacs-gui-file-bridge-runtime-test--slurp
			                      "/tmp/nemacs-buf")))
			            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
			            (write-region "C-h C-a" nil "/tmp/nemacs-keys" nil 'silent)
			            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
			            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
			            (nemacs-gui-file-bridge-runtime-test--run-ok
			             reader image "(nemacs-gui-file-bridge-run)")
			            (should (string-match-p
			                     "About GNU Emacs"
			                     (nemacs-gui-file-bridge-runtime-test--slurp
			                      "/tmp/nemacs-buf")))
			            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
			            (write-region "C-h C-n" nil "/tmp/nemacs-keys" nil 'silent)
			            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
			            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
			            (nemacs-gui-file-bridge-runtime-test--run-ok
			             reader image "(nemacs-gui-file-bridge-run)")
				            (should (string-match-p
				                     "GNU Emacs News"
				                     (nemacs-gui-file-bridge-runtime-test--slurp
				                      "/tmp/nemacs-buf")))
                            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                            (write-region "C-h i" nil "/tmp/nemacs-keys" nil 'silent)
                            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                            (nemacs-gui-file-bridge-runtime-test--run-ok
                             reader image "(nemacs-gui-file-bridge-run)")
                            (should (equal "*info*"
                                           (nemacs-gui-file-bridge-runtime-test--slurp
                                            "/tmp/nemacs-buffer-name")))
                            (should (string-match-p
                                     "Info Directory"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
                            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                            (write-region "C-h r" nil "/tmp/nemacs-keys" nil 'silent)
                            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                            (nemacs-gui-file-bridge-runtime-test--run-ok
                             reader image "(nemacs-gui-file-bridge-run)")
                            (should (string-match-p
                                     "Emacs Manual"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
                            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                            (write-region "C-h F" nil "/tmp/nemacs-keys" nil 'silent)
                            (write-region "save-buffer" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
                            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                            (nemacs-gui-file-bridge-runtime-test--run-ok
                             reader image "(nemacs-gui-file-bridge-run)")
                            (should (string-match-p
                                     "Emacs Command: save-buffer"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
                            (write-region "describe-package" nil "/tmp/nemacs-cmd" nil 'silent)
                            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                            (write-region "files" nil "/tmp/nemacs-arg" nil 'silent)
                            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                            (nemacs-gui-file-bridge-runtime-test--run-ok
                             reader image "(nemacs-gui-file-bridge-run)")
                            (should (string-match-p
                                     "Package: files"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
                            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                            (write-region "C-h ." nil "/tmp/nemacs-keys" nil 'silent)
                            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                            (nemacs-gui-file-bridge-runtime-test--run-ok
                             reader image "(nemacs-gui-file-bridge-run)")
                            (should (equal "*Help*"
                                           (nemacs-gui-file-bridge-runtime-test--slurp
                                            "/tmp/nemacs-buffer-name")))
                            (should (string-match-p
                                     "Local Help"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
                            (write-region "help-find-source" nil "/tmp/nemacs-cmd" nil 'silent)
                            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                            (nemacs-gui-file-bridge-runtime-test--run-ok
                             reader image "(nemacs-gui-file-bridge-run)")
                            (should (string-match-p
                                     "Find Source"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
                            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                            (write-region "C-M-." nil "/tmp/nemacs-keys" nil 'silent)
                            (write-region "alpha beta\nbeta gamma\n" nil "/tmp/nemacs-buf" nil 'silent)
                            (write-region "beta" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
                            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                            (nemacs-gui-file-bridge-runtime-test--run-ok
                             reader image "(nemacs-gui-file-bridge-run)")
                            (should (equal "*xref*"
                                           (nemacs-gui-file-bridge-runtime-test--slurp
                                            "/tmp/nemacs-buffer-name")))
                            (should (string-match-p
                                     "Xref Apropos: beta"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
                            (should (string-match-p
                                     "2 matches"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
                            (should (equal "1"
                                           (nemacs-gui-file-bridge-runtime-test--slurp
                                            "/tmp/nemacs-read-only")))
                            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                            (write-region "M-." nil "/tmp/nemacs-keys" nil 'silent)
                            (write-region "alpha beta\nbeta gamma\n" nil "/tmp/nemacs-buf" nil 'silent)
                            (write-region "alpha" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
                            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                            (nemacs-gui-file-bridge-runtime-test--run-ok
                             reader image "(nemacs-gui-file-bridge-run)")
                            (should (equal "*xref*"
                                           (nemacs-gui-file-bridge-runtime-test--slurp
                                            "/tmp/nemacs-buffer-name")))
                            (should (string-match-p
                                     "Xref Definitions: alpha"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
                            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                            (write-region "M-?" nil "/tmp/nemacs-keys" nil 'silent)
                            (write-region "alpha beta\nbeta alpha\n" nil "/tmp/nemacs-buf" nil 'silent)
                            (write-region "alpha" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
                            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                            (nemacs-gui-file-bridge-runtime-test--run-ok
                             reader image "(nemacs-gui-file-bridge-run)")
                            (should (equal "*xref*"
                                           (nemacs-gui-file-bridge-runtime-test--slurp
                                            "/tmp/nemacs-buffer-name")))
                            (should (string-match-p
                                     "Xref References: alpha"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
                            (should (string-match-p
                                     "2 matches"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
                            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                            (write-region "M-," nil "/tmp/nemacs-keys" nil 'silent)
                            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                            (nemacs-gui-file-bridge-runtime-test--run-ok
                             reader image "(nemacs-gui-file-bridge-run)")
                            (should (equal "*xref*"
                                           (nemacs-gui-file-bridge-runtime-test--slurp
                                            "/tmp/nemacs-buffer-name")))
	                            (should (string-match-p
	                                     "Xref Back"
	                                     (nemacs-gui-file-bridge-runtime-test--slurp
	                                      "/tmp/nemacs-buf")))
                            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                            (write-region "C-x `" nil "/tmp/nemacs-keys" nil 'silent)
                            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                            ;; The preceding xref scenarios used the minibuffer;
                            ;; clear the minibuffer-active transport (as the
                            ;; front-end would) so `C-x `' dispatches next-error
                            ;; rather than being read as minibuffer input.
                            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
                            (nemacs-gui-file-bridge-runtime-test--run-ok
                             reader image "(nemacs-gui-file-bridge-run)")
                            (should (equal "*compilation*"
                                           (nemacs-gui-file-bridge-runtime-test--slurp
                                            "/tmp/nemacs-buffer-name")))
                            (should (string-match-p
                                     "Next Error"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
                            (should (equal "1"
                                           (nemacs-gui-file-bridge-runtime-test--slurp
                                            "/tmp/nemacs-read-only")))
                            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                            (write-region "M-g p" nil "/tmp/nemacs-keys" nil 'silent)
                            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                            (nemacs-gui-file-bridge-runtime-test--run-ok
                             reader image "(nemacs-gui-file-bridge-run)")
                            (should (equal "*compilation*"
                                           (nemacs-gui-file-bridge-runtime-test--slurp
                                            "/tmp/nemacs-buffer-name")))
                            (should (string-match-p
                                     "Previous Error"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
	                            (write-region "xref-find-definitions-other-window" nil "/tmp/nemacs-cmd" nil 'silent)
                            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                            (write-region "target value\nother\n" nil "/tmp/nemacs-buf" nil 'silent)
                            (write-region "target" nil "/tmp/nemacs-arg" nil 'silent)
                            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                            (nemacs-gui-file-bridge-runtime-test--run-ok
                             reader image "(nemacs-gui-file-bridge-run)")
                            (should (equal "*xref*"
                                           (nemacs-gui-file-bridge-runtime-test--slurp
                                            "/tmp/nemacs-buffer-name")))
                            (should (string-match-p
                                     "Xref Definitions: target"
                                     (nemacs-gui-file-bridge-runtime-test--slurp
                                      "/tmp/nemacs-buf")))
				            (write-region "describe-mode" nil "/tmp/nemacs-cmd" nil 'silent)
				            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
				            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
				            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
				            (nemacs-gui-file-bridge-runtime-test--run-ok
				             reader image "(nemacs-gui-file-bridge-run)")
				            (should (string-match-p
				                     "Mode Help"
				                     (nemacs-gui-file-bridge-runtime-test--slurp
				                      "/tmp/nemacs-buf")))
				            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
				            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
				            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
				            (write-region "" nil "/tmp/nemacs-minibuffer-state" nil 'silent)
				            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
				            (write-region "C-h e" nil "/tmp/nemacs-keys" nil 'silent)
				            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
				            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
				            (nemacs-gui-file-bridge-runtime-test--run-ok
				             reader image "(nemacs-gui-file-bridge-run)")
				            (should (string-match-p
				                     "Echo Area Messages"
				                     (nemacs-gui-file-bridge-runtime-test--slurp
				                      "/tmp/nemacs-buf")))
				            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
				            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
				            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
				            (write-region "" nil "/tmp/nemacs-minibuffer-state" nil 'silent)
				            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
				            (write-region "C-h m" nil "/tmp/nemacs-keys" nil 'silent)
				            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
				            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
				            (nemacs-gui-file-bridge-runtime-test--run-ok
				             reader image "(nemacs-gui-file-bridge-run)")
				            (should (string-match-p
				                     "Mode Help"
				                     (nemacs-gui-file-bridge-runtime-test--slurp
				                      "/tmp/nemacs-buf")))
				            (write-region "where-is" nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region "save-buffer" nil "/tmp/nemacs-arg" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
		            (should (string-match-p
		                     "save-buffer is on .*C-x C-s"
		                     (nemacs-gui-file-bridge-runtime-test--slurp
		                      "/tmp/nemacs-buf")))
		            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region "C-h w" nil "/tmp/nemacs-keys" nil 'silent)
		            (write-region "find-file" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
		            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
		            (should (string-match-p
		                     "find-file is on .*C-x C-f"
		                     (nemacs-gui-file-bridge-runtime-test--slurp
		                      "/tmp/nemacs-buf")))
		            (write-region "describe-command" nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region "save-buffer" nil "/tmp/nemacs-arg" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
		            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
		            (should (string-match-p
		                     "save-buffer is a function"
		                     (nemacs-gui-file-bridge-runtime-test--slurp
		                      "/tmp/nemacs-buf")))
		            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region "C-h x" nil "/tmp/nemacs-keys" nil 'silent)
		            (write-region "forward-char" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
		            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
		            (should (string-match-p
		                     "forward-char is a function"
		                     (nemacs-gui-file-bridge-runtime-test--slurp
		                      "/tmp/nemacs-buf")))
		            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
	            (should (equal "1"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-read-only")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "one\ntwo\nthree\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "what-cursor-position" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Help*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (let ((cursor-help
                   (nemacs-gui-file-bridge-runtime-test--slurp
                    "/tmp/nemacs-buf")))
              (should (string-match-p "Cursor position" cursor-help))
              (should (string-match-p "Point: 00005" cursor-help))
              (should (string-match-p "Line: 00002" cursor-help))
              (should (string-match-p "Column: 00001" cursor-help))
              (should (string-match-p "Buffer: main" cursor-help)))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-read-only")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "one\ntwo\nthree\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "describe-variable" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "buffer-file-name" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "/tmp/nemacs-gui-help-target" nil "/tmp/nemacs-file" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Help*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (string-match-p
                     "buffer-file-name is a variable"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (string-match-p
                     "Value: /tmp/nemacs-gui-help-target"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-read-only")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "one\ntwo\nthree\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "goto-line" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "99" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 14 (nemacs-gui-file-bridge-runtime-test--point-value))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-dired-file-ops ()
  "Dired mark/unmark/flag/delete/rename/copy operate through the standalone bridge."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (dir "/tmp/nemacs-dired-ops-test"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (when (file-directory-p dir)
              (delete-directory dir t))
            (make-directory dir t)
            (write-region "alpha\n" nil (concat dir "/a.txt") nil 'silent)
            (write-region "beta\n" nil (concat dir "/b.txt") nil 'silent)
            (when (file-exists-p "/tmp/nemacs-dired-marks")
              (delete-file "/tmp/nemacs-dired-marks"))
            ;; Step 1: dired renders the listing with an empty mark column.
            (write-region "dired" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region dir nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "old\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Directory*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "  a.txt\n") buf))
              (should (string-match-p (regexp-quote "  b.txt\n") buf)))
            ;; Step 2: flag a.txt for deletion -> "D a.txt".
            (let* ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                         "/tmp/nemacs-buf"))
                   (pos (string-match (regexp-quote "  a.txt\n") buf)))
              (should pos)
              (write-region (number-to-string pos)
                            nil "/tmp/nemacs-point" nil 'silent))
            (write-region "dired-flag-file-deletion"
                          nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "D a.txt\n") buf))
              (should (string-match-p (regexp-quote "  b.txt\n") buf)))
            ;; Step 3: flagged delete removes a.txt from disk and listing.
            (write-region "dired-do-flagged-delete"
                          nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should-not (file-exists-p (concat dir "/a.txt")))
            (should (file-exists-p (concat dir "/b.txt")))
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should-not (string-match-p (regexp-quote "a.txt") buf))
              (should (string-match-p (regexp-quote "  b.txt\n") buf)))
            (should (equal "Deleted 1 files"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-modeline")))
            ;; Step 4: mark b.txt -> "* b.txt", then unmark -> "  b.txt".
            (let* ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                         "/tmp/nemacs-buf"))
                   (pos (string-match (regexp-quote "  b.txt\n") buf)))
              (should pos)
              (write-region (number-to-string pos)
                            nil "/tmp/nemacs-point" nil 'silent))
            (write-region "dired-mark" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "* b.txt\n") buf)))
            (let* ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                         "/tmp/nemacs-buf"))
                   (pos (string-match (regexp-quote "* b.txt\n") buf)))
              (should pos)
              (write-region (number-to-string pos)
                            nil "/tmp/nemacs-point" nil 'silent))
            (write-region "dired-unmark" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "  b.txt\n") buf)))
            ;; Step 5: rename b.txt -> c.txt via the minibuffer arg transport.
            (let* ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                         "/tmp/nemacs-buf"))
                   (pos (string-match (regexp-quote "  b.txt\n") buf)))
              (should pos)
              (write-region (number-to-string pos)
                            nil "/tmp/nemacs-point" nil 'silent))
            (write-region "dired-do-rename" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "c.txt" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should-not (file-exists-p (concat dir "/b.txt")))
            (should (file-exists-p (concat dir "/c.txt")))
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "  c.txt\n") buf))
              (should-not (string-match-p (regexp-quote "b.txt") buf)))
            ;; Step 6: copy c.txt -> d.txt; both files remain with same text.
            (let* ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                         "/tmp/nemacs-buf"))
                   (pos (string-match (regexp-quote "  c.txt\n") buf)))
              (should pos)
              (write-region (number-to-string pos)
                            nil "/tmp/nemacs-point" nil 'silent))
            (write-region "dired-do-copy" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "d.txt" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (file-exists-p (concat dir "/c.txt")))
            (should (file-exists-p (concat dir "/d.txt")))
            (should (equal "beta\n"
                           (with-temp-buffer
                             (insert-file-contents (concat dir "/d.txt"))
                             (buffer-string))))
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "  c.txt\n") buf))
              (should (string-match-p (regexp-quote "  d.txt\n") buf))))
        (when (file-directory-p dir)
          (delete-directory dir t))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-org-daily-lane ()
  "Org TODO cycle / agenda / narrow-to-subtree / table motion / capture (M9)."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (org-text (concat "* TODO buy milk\n"
                            "some body\n"
                            "** sub task\n"
                            "* DONE done item\n"
                            "| a | b |\n"
                            "| 1 | 2 |\n"
                            "* plain heading\n")))
      (nemacs-gui-file-bridge-runtime-test--with-transport
        (cl-flet ((reset-main (buf point)
                    (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-file" nil 'silent)
                    (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                    (write-region buf nil "/tmp/nemacs-buf" nil 'silent)
                    (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                    (write-region (number-to-string point)
                                  nil "/tmp/nemacs-point" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)))
          ;; Step 1: org-todo cycles TODO -> DONE on the first heading.
          (reset-main org-text 0)
          (write-region "org-todo" nil "/tmp/nemacs-cmd" nil 'silent)
          (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
          (nemacs-gui-file-bridge-runtime-test--run-ok
           reader image "(nemacs-gui-file-bridge-run)")
          (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (string-prefix-p "* DONE buy milk\n" buf)))
          ;; Step 2: org-todo again removes the keyword.
          (write-region "org-todo" nil "/tmp/nemacs-cmd" nil 'silent)
          (nemacs-gui-file-bridge-runtime-test--run-ok
           reader image "(nemacs-gui-file-bridge-run)")
          (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (string-prefix-p "* buy milk\n" buf)))
          ;; Step 3: org-todo on a plain heading adds TODO.
          (let ((pos (string-match (regexp-quote "* plain heading")
                                   org-text)))
            (should pos)
            (reset-main org-text pos))
          (write-region "org-todo" nil "/tmp/nemacs-cmd" nil 'silent)
          (nemacs-gui-file-bridge-runtime-test--run-ok
           reader image "(nemacs-gui-file-bridge-run)")
          (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (string-match-p (regexp-quote "* TODO plain heading\n")
                                    buf)))
          ;; Step 4: org-agenda lists only TODO headings, read-only.
          (reset-main org-text 0)
          (write-region "org-agenda" nil "/tmp/nemacs-cmd" nil 'silent)
          (nemacs-gui-file-bridge-runtime-test--run-ok
           reader image "(nemacs-gui-file-bridge-run)")
          (should (equal "*Org Agenda*"
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buffer-name")))
          (should (equal "1"
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-read-only")))
          (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (string-match-p (regexp-quote "* TODO buy milk") buf))
            (should-not (string-match-p (regexp-quote "done item") buf))
            (should-not (string-match-p (regexp-quote "plain heading") buf)))
          ;; Step 5: org-narrow-to-subtree narrows to the first subtree.
          (let ((pos (string-match (regexp-quote "some body") org-text)))
            (should pos)
            (reset-main org-text pos))
          (write-region "org-narrow-to-subtree" nil "/tmp/nemacs-cmd" nil 'silent)
          (nemacs-gui-file-bridge-runtime-test--run-ok
           reader image "(nemacs-gui-file-bridge-run)")
          (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (equal "* TODO buy milk\nsome body\n** sub task\n" buf)))
          ;; Step 6: org-table-next-field jumps to the next cell.
          (let ((pos (string-match (regexp-quote "| a | b |") org-text)))
            (should pos)
            (reset-main org-text pos)
            (write-region "org-table-next-field" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= (+ pos 2)
                       (nemacs-gui-file-bridge-runtime-test--point-value))))
          ;; Step 7: org-capture appends a TODO heading at end of buffer.
          (reset-main org-text 0)
          (write-region "org-capture" nil "/tmp/nemacs-cmd" nil 'silent)
          (write-region "write report" nil "/tmp/nemacs-arg" nil 'silent)
          (nemacs-gui-file-bridge-runtime-test--run-ok
           reader image "(nemacs-gui-file-bridge-run)")
          (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (string-suffix-p "* TODO write report\n" buf))
            (should (string-prefix-p org-text buf))))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-magit-min ()
  "Magit-min status/stage/commit/diff/log workflow (M10)."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (repo "/tmp/nemacs-magit-test"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (when (file-directory-p repo)
              (delete-directory repo t))
            (make-directory repo t)
            (let ((default-directory repo))
              (shell-command-to-string "git init -q .")
              (shell-command-to-string "git config user.email nemacs@test")
              (shell-command-to-string "git config user.name nemacs")
              (write-region "one\n" nil (concat repo "/file.txt") nil 'silent)
              (shell-command-to-string "git add file.txt")
              (shell-command-to-string "git commit -q -m init"))
            (write-region "one\ntwo\n" nil (concat repo "/file.txt") nil 'silent)
            ;; Step 1: magit-status renders head + porcelain.
            (write-region "magit-status" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region repo nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "old\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*magit*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "Head: ") buf))
              (should (string-match-p (regexp-quote "init") buf))
              (should (string-match-p (regexp-quote " M file.txt") buf)))
            ;; Step 2: stage the file at point -> index column set.
            (let* ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                         "/tmp/nemacs-buf"))
                   (pos (string-match (regexp-quote " M file.txt") buf)))
              (should pos)
              (write-region (number-to-string pos)
                            nil "/tmp/nemacs-point" nil 'silent))
            (write-region "magit-stage-file" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "M  file.txt") buf)))
            ;; Step 3: commit -> clean status + modeline.
            (write-region "magit-commit" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "second change" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "Committed"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-modeline")))
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "second change") buf))
              (should (string-match-p (regexp-quote "(clean)") buf)))
            ;; Step 4: diff shows a new unstaged change.
            (write-region "one\ntwo\nthree\n"
                          nil (concat repo "/file.txt") nil 'silent)
            (write-region "magit-diff" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*magit-diff*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "+three") buf)))
            ;; Step 5: log lists both commits.
            (write-region "magit-log" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*magit-log*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "second change") buf))
              (should (string-match-p (regexp-quote "init") buf))))
        (when (file-directory-p repo)
          (delete-directory repo t))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-info-node-navigation ()
  "M13: open a real .info file, render the Top node, navigate n/p/u."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (fixture "/tmp/nemacs-info-fixture.info"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region
             (concat
              "Test fixture preamble.\n"
              "\037\nFile: fixture.info,  Node: Top,  Next: First,  Up: (dir)\n"
              "\nTop node body line.\n\n* Menu:\n\n* First::\n* Second::\n"
              "\037\nFile: fixture.info,  Node: First,  Next: Second,  Prev: Top,  Up: Top\n"
              "\nFirst node body line.\n"
              "\037\nFile: fixture.info,  Node: Second,  Prev: First,  Up: Top\n"
              "\nSecond node body line.\n")
             nil fixture nil 'silent)
            (when (file-exists-p "/tmp/nemacs-info-state")
              (delete-file "/tmp/nemacs-info-state"))
            ;; Step 1: open the file -> Top node in *info*.
            (write-region "info" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region fixture nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "x" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*info*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "1" (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-read-only")))
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "Node: Top") buf))
              (should (string-match-p (regexp-quote "Top node body line.") buf)))
            ;; Step 2: n -> First (raw key through the *info* mode keymap).
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "n" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "Node: First") buf))
              (should (string-match-p (regexp-quote "First node body line.") buf)))
            ;; Step 3: n -> Second.
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "n" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (string-match-p
                     (regexp-quote "Second node body line.")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            ;; Step 4: p -> back to First.
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "p" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (string-match-p
                     (regexp-quote "Node: First")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            ;; Step 5: u -> Top.
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "u" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (string-match-p
                     (regexp-quote "Node: Top")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf"))))
        (when (file-exists-p fixture)
          (delete-file fixture))
        (when (file-exists-p "/tmp/nemacs-info-state")
          (delete-file "/tmp/nemacs-info-state"))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-customize-set-save ()
  "M13: round-trip one defcustom-style variable set+save."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (when (file-exists-p "/tmp/nemacs-custom-store")
              (delete-file "/tmp/nemacs-custom-store"))
            (when (file-exists-p "/tmp/nemacs-custom-file")
              (delete-file "/tmp/nemacs-custom-file"))
            ;; Step 1: open the customize surface for fill-column.
            (write-region "customize-variable" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "fill-column" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "x" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Customize*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "Customize: fill-column") buf))
              (should (string-match-p (regexp-quote "Value: 70") buf)))
            ;; Step 2: set and save a new value.
            (write-region "customize-save-variable" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "84" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "Saved fill-column"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-modeline")))
            (should (string-match-p
                     (regexp-quote "Value: 84")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (string-match-p
                     "fill-column\t84"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-custom-store")))
            (let ((custom (nemacs-gui-file-bridge-runtime-test--slurp
                           "/tmp/nemacs-custom-file")))
              (should (string-match-p (regexp-quote "(custom-set-variables") custom))
              (should (string-match-p (regexp-quote "'(fill-column 84)") custom)))
            ;; Step 3: a fresh bridge process re-applies the persisted
            ;; value (the store survives the one-shot process).
            (write-region "customize-variable" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "fill-column" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (string-match-p
                     (regexp-quote "Value: 84")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf"))))
        (when (file-exists-p "/tmp/nemacs-custom-store")
          (delete-file "/tmp/nemacs-custom-store"))
        (when (file-exists-p "/tmp/nemacs-custom-file")
          (delete-file "/tmp/nemacs-custom-file"))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-tramp-ssh-roundtrip ()
  "M11: /ssh:HOST:/path find-file -> edit -> save round-trip (stub ssh)."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let* ((reader (nemacs-gui-file-bridge-runtime-test--reader))
           (image (nemacs-gui-file-bridge-runtime-test--write-image))
           (stub-dir "/tmp/nemacs-tramp-bin")
           (remote-dir "/tmp/nemacs-tramp-remote")
           (remote-file (concat remote-dir "/hello.txt"))
           (tramp-path (concat "/ssh:fakehost:" remote-file)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (dolist (dir (list stub-dir remote-dir))
              (when (file-directory-p dir)
                (delete-directory dir t))
              (make-directory dir t))
            (write-region "#!/bin/sh\nshift\nexec /bin/sh -c \"$*\"\n"
                          nil (concat stub-dir "/ssh") nil 'silent)
            (set-file-modes (concat stub-dir "/ssh") #o755)
            (write-region "remote hello\n" nil remote-file nil 'silent)
            (let ((process-environment
                   (cons (concat "PATH=" stub-dir ":" (getenv "PATH"))
                         process-environment)))
              ;; Step 1: find-file loads the remote content.
              (write-region "find-file" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region tramp-path nil "/tmp/nemacs-arg" nil 'silent)
              (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
              (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
              (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
              (write-region "" nil "/tmp/nemacs-file" nil 'silent)
              (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
              (write-region "old\n" nil "/tmp/nemacs-buf" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
              (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal tramp-path
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "remote hello\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              ;; Step 2: save-buffer writes the edited text back remotely.
              (write-region "remote hello\nedited locally\n"
                            nil "/tmp/nemacs-buf" nil 'silent)
              (write-region tramp-path nil "/tmp/nemacs-file" nil 'silent)
              (write-region "save-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "remote hello\nedited locally\n"
                             (with-temp-buffer
                               (insert-file-contents remote-file)
                               (buffer-string))))
              ;; Step 3: a non-ssh Tramp method is not silently mangled —
              ;; find-file on /scp:... falls through to the local path check
              ;; and reports file-not-found.
              (write-region "find-file" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "/scp:fakehost:/tmp/x" nil "/tmp/nemacs-arg" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "file-not-found"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-status")))))
        (dolist (dir (list stub-dir remote-dir))
          (when (file-directory-p dir)
            (delete-directory dir t)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-org-v2 ()
  "M9 v2: CLOSED timestamps, org-cycle fold toggle, table align, capture file."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (capture-file "/tmp/nemacs-org-capture-target.org")
          (org-text (concat "* TODO buy milk\n"
                            "some body\n"
                            "** sub task\n"
                            "* second\n")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (when (file-exists-p capture-file)
              (delete-file capture-file))
            (when (file-exists-p "/tmp/nemacs-org-capture-file")
              (delete-file "/tmp/nemacs-org-capture-file"))
            ;; Step 1: TODO -> DONE adds a CLOSED stamp line.
            (write-region "org-todo" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region org-text nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-prefix-p "* DONE buy milk\n  CLOSED: [" buf))
              (should (string-match-p (regexp-quote "]\nsome body\n") buf)))
            ;; Step 2: DONE -> none removes the keyword and the CLOSED line.
            (write-region "org-todo" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-prefix-p "* buy milk\nsome body\n" buf))
              (should-not (string-match-p (regexp-quote "CLOSED") buf)))
            ;; Step 3: org-cycle on a heading narrows; org-cycle again widens.
            (write-region org-text nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "org-cycle" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "* TODO buy milk\nsome body\n** sub task\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "org-cycle" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal org-text
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            ;; Step 4: org-table-align pads ragged columns and separators.
            (let ((table "| a | bbb |\n|---+--|\n| cc | d |\n"))
              (write-region table nil "/tmp/nemacs-buf" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
              (write-region "org-table-align" nil "/tmp/nemacs-cmd" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "| a  | bbb |\n|----+-----|\n| cc | d   |\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf"))))
            ;; Step 5: org-shifttab renders the headings-only overview.
            (write-region org-text nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "org-shifttab" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Org Overview*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "* TODO buy milk\n") buf))
              (should (string-match-p (regexp-quote "** sub task\n") buf))
              (should-not (string-match-p (regexp-quote "some body") buf)))
            ;; Step 6: org-capture appends to the configured capture file.
            (write-region capture-file nil "/tmp/nemacs-org-capture-file" nil 'silent)
            (write-region org-text nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "org-capture" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "captured item" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "* TODO captured item\n"
                           (with-temp-buffer
                             (insert-file-contents capture-file)
                             (buffer-string))))
            (should (equal org-text
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf"))))
        (when (file-exists-p capture-file)
          (delete-file capture-file))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-mode-local-keys ()
  "Mode-local raw keys dispatch in *Directory* / *magit* / .org buffers."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (dir "/tmp/nemacs-modekey-dired-test")
          (repo "/tmp/nemacs-modekey-magit-test")
          (org-file "/tmp/nemacs-modekey-note.org"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            ;; --- dired keys: d flags, x deletes, C copies (prompted) ---
            (when (file-directory-p dir)
              (delete-directory dir t))
            (make-directory dir t)
            (write-region "alpha\n" nil (concat dir "/a.txt") nil 'silent)
            (when (file-exists-p "/tmp/nemacs-dired-marks")
              (delete-file "/tmp/nemacs-dired-marks"))
            (write-region "dired" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region dir nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "old\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let* ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                         "/tmp/nemacs-buf"))
                   (pos (string-match (regexp-quote "  a.txt") buf)))
              (should pos)
              (write-region (number-to-string pos)
                            nil "/tmp/nemacs-point" nil 'silent))
            ;; prompted copy: C with arg pre-filled
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "b.txt" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (file-exists-p (concat dir "/b.txt")))
            ;; flag a.txt with raw key d, then delete with x
            (let* ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                         "/tmp/nemacs-buf"))
                   (pos (string-match (regexp-quote "  a.txt") buf)))
              (should pos)
              (write-region (number-to-string pos)
                            nil "/tmp/nemacs-point" nil 'silent))
            (write-region "d" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "D a.txt") buf)))
            (write-region "x" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should-not (file-exists-p (concat dir "/a.txt")))
            ;; --- magit keys: s stages, c commits (prompted) ---
            (when (file-directory-p repo)
              (delete-directory repo t))
            (make-directory repo t)
            (let ((default-directory repo))
              (shell-command-to-string "git init -q .")
              (shell-command-to-string "git config user.email nemacs@test")
              (shell-command-to-string "git config user.name nemacs")
              (write-region "one\n" nil (concat repo "/file.txt") nil 'silent)
              (shell-command-to-string "git add file.txt")
              (shell-command-to-string "git commit -q -m init"))
            (write-region "one\ntwo\n" nil (concat repo "/file.txt") nil 'silent)
            (write-region "magit-status" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region repo nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "old\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let* ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                         "/tmp/nemacs-buf"))
                   (pos (string-match (regexp-quote " M file.txt") buf)))
              (should pos)
              (write-region (number-to-string pos)
                            nil "/tmp/nemacs-point" nil 'silent))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "s" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "M  file.txt") buf)))
            (write-region "c" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "key commit" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p (regexp-quote "key commit") buf))
              (should (string-match-p (regexp-quote "(clean)") buf)))
            ;; --- org TAB: org-cycle narrows in a .org buffer ---
            (write-region "TAB-org\n" nil org-file nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "TAB" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region org-file nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "* head\nbody\n* tail\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "* head\nbody\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf"))))
        (dolist (d (list dir repo))
          (when (file-directory-p d)
            (delete-directory d t)))
        (when (file-exists-p org-file)
          (delete-file org-file))
        (when (file-exists-p image)
          (delete-file image))))))

(defun nemacs-gui-file-bridge-runtime-test--wait-for (predicate timeout)
  "Poll PREDICATE every 0.1s for up to TIMEOUT seconds; return its last value."
  (let ((deadline (+ (float-time) timeout))
        (value nil))
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (sleep-for 0.1))
    value))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-session-bridge-roundtrip ()
  "The persistent session loop serves requests with in-process buffer state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (proc nil))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "abc\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-session-request" nil 'silent)
            (write-region "" nil "/tmp/nemacs-session-response" nil 'silent)
            (write-region "" nil "/tmp/nemacs-session-shutdown" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-session-ready" nil 'silent)
            (setq proc
                  (start-process "nemacs-session-test" nil reader
                                 "exec-runtime-image" image
                                 "(nemacs-gui-file-bridge-session-run)"))
            (should (nemacs-gui-file-bridge-runtime-test--wait-for
                     (lambda ()
                       (equal "1" (nemacs-gui-file-bridge-runtime-test--slurp
                                   "/tmp/nemacs-session-ready")))
                     60))
            ;; Request 1: C-f moves point 0 -> 1.
            (write-region "C-f" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "req-1" nil "/tmp/nemacs-session-request" nil 'silent)
            (should (nemacs-gui-file-bridge-runtime-test--wait-for
                     (lambda ()
                       (equal "req-1"
                              (nemacs-gui-file-bridge-runtime-test--slurp
                               "/tmp/nemacs-session-response")))
                     30))
            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
            ;; Request 2: poison the point transport; the session must keep
            ;; its IN-PROCESS state, so a second C-f lands on 2 (not 1).
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "C-f" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "req-2" nil "/tmp/nemacs-session-request" nil 'silent)
            (should (nemacs-gui-file-bridge-runtime-test--wait-for
                     (lambda ()
                       (equal "req-2"
                              (nemacs-gui-file-bridge-runtime-test--slurp
                               "/tmp/nemacs-session-response")))
                     30))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
            ;; Shutdown: loop exits, ready flag drops, process dies.
            (write-region "1" nil "/tmp/nemacs-session-shutdown" nil 'silent)
            (should (nemacs-gui-file-bridge-runtime-test--wait-for
                     (lambda ()
                       (equal "0" (nemacs-gui-file-bridge-runtime-test--slurp
                                   "/tmp/nemacs-session-ready")))
                     30))
            (should (nemacs-gui-file-bridge-runtime-test--wait-for
                     (lambda () (not (process-live-p proc)))
                     30)))
        (when (and proc (process-live-p proc))
          (kill-process proc))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-large-org-file ()
  "Daily-driver scale: ~500KB org file find-file / edit / org-todo / save."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (big-file "/tmp/nemacs-large-org-test.org")
          (content nil))
      (with-temp-buffer
        (dotimes (i 6000)
          (insert (format "* TODO task %04d entry heading line\n" i))
          (insert (format "body text for entry %04d with some padding text\n" i)))
        (setq content (buffer-string)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region content nil big-file nil 'silent)
            (should (> (file-attribute-size (file-attributes big-file))
                       400000))
            ;; Step 1: find-file loads the whole file through the bridge.
            (write-region "find-file" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region big-file nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "old\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (let ((start (float-time)))
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (princ (format "[large-org] find-file %.1fs\n"
                             (- (float-time) start))))
            (should (equal content
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            ;; Step 2: org-todo on a deep heading near the end.
            (let ((pos (string-match (regexp-quote "* TODO task 5990") content)))
              (should pos)
              (write-region (number-to-string pos)
                            nil "/tmp/nemacs-point" nil 'silent))
            (write-region "org-todo" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (let ((start (float-time)))
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (princ (format "[large-org] org-todo %.1fs\n"
                             (- (float-time) start))))
            (let ((buf (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p
                       (regexp-quote "* DONE task 5990 entry heading line")
                       buf)))
            ;; Step 3: save-buffer writes the edited 500KB back to disk.
            (write-region "save-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (let ((start (float-time)))
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (princ (format "[large-org] save-buffer %.1fs\n"
                             (- (float-time) start))))
            (let ((on-disk (with-temp-buffer
                             (insert-file-contents big-file)
                             (buffer-string))))
              (should (string-match-p
                       (regexp-quote "* DONE task 5990 entry heading line")
                       on-disk))
              (should (string-match-p (regexp-quote "  CLOSED: [") on-disk))
              ;; everything except the one edited heading + CLOSED line
              ;; survives byte-identically
              (should (string-prefix-p
                       (substring content 0
                                  (string-match (regexp-quote "* TODO task 5990")
                                                content))
                       on-disk))
              (should (string-suffix-p
                       (substring content
                                  (string-match (regexp-quote "body text for entry 5990")
                                                content))
                       on-disk))))
        (when (file-exists-p big-file)
          (delete-file big-file))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-narrow-widen ()
  "In standalone NeLisp, narrowing should persist and widen should merge edits."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x n n" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "alpha\nbeta\ngamma\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "11" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "beta\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 5 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "BETA!\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x n w" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "alpha\nBETA!\ngamma\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 12 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 6 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x n p" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "one\n\fpage2\nend\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "page2\nend\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x n w" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x n d" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "(defun a\n  x)\n(defun b\n  y)\n"
                          nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "25" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "(defun b\n  y)\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf"))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-kmacro-core ()
  "In one standalone NeLisp runtime, keyboard macros record and replay raw keys."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-kmacro-recording" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kmacro-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (concat
              "(progn\n"
              (nemacs-gui-file-bridge-runtime-test--raw-key-form
               '("C-x (" "a" "b" "C-x )"))
              "\n"
              "(nl-write-file \"/tmp/nemacs-buf\" \"\")\n"
              "(nl-write-file \"/tmp/nemacs-point\" \"0\")\n"
              "(nl-write-file \"/tmp/nemacs-mark\" \"0\")\n"
              "(nl-write-file \"/tmp/nemacs-cmd\" \"\")\n"
              "(nl-write-file \"/tmp/nemacs-keys\" \"C-x e\")\n"
              "(nemacs-gui-file-bridge-run)\n"
              ")\n"))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kmacro-recording")))
            (should (equal "a\nb\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kmacro-keys")))
            (should (equal "ab"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-save-and-transform ()
  "In standalone NeLisp, the bridge should execute commands through its adapter."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
	    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
	          (image (nemacs-gui-file-bridge-runtime-test--write-image))
	          (save-file (make-temp-file "nemacs-gui-file-bridge-save-"))
	          (find-other-file
	           (make-temp-file "nemacs-gui-file-bridge-find-other-"))
	          (read-only-other-file
	           (make-temp-file "nemacs-gui-file-bridge-ro-other-")))
	      (unwind-protect
	          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-state" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-minibuffer-cursor" nil 'silent)
            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "save-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region save-file nil "/tmp/nemacs-file" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "saved through command-execute\n"
                          nil "/tmp/nemacs-buf" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
		            (should (equal "saved through command-execute\n"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            save-file)))
		            (should (= 7 (nemacs-gui-file-bridge-runtime-test--point-value)))
		            (write-region "basic-save-buffer"
		                          nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region save-file nil "/tmp/nemacs-file" nil 'silent)
		            (write-region "basic save alias\n"
		                          nil "/tmp/nemacs-buf" nil 'silent)
		            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
			            (should (equal "basic save alias\n"
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            save-file)))
			            (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value)))
			            (write-region "find other window\n" nil find-other-file nil 'silent)
			            (write-region "find-file-other-window"
			                          nil "/tmp/nemacs-cmd" nil 'silent)
			            (write-region find-other-file nil "/tmp/nemacs-arg" nil 'silent)
			            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
			            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
			            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
			            (nemacs-gui-file-bridge-runtime-test--run-ok
			             reader image "(nemacs-gui-file-bridge-run)")
			            (should (equal "find other window\n"
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            "/tmp/nemacs-buf")))
			            (should (equal find-other-file
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            "/tmp/nemacs-file")))
			            (should (equal "0"
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            "/tmp/nemacs-read-only")))
				            (should (equal "vertical"
				                           (nemacs-gui-file-bridge-runtime-test--slurp
				                            "/tmp/nemacs-window-layout")))
					            (ert-info ("find-file-other-window selects other window")
						              (should (equal "1"
						                             (nemacs-gui-file-bridge-runtime-test--slurp
						                              "/tmp/nemacs-window-selected"))))
					            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                        (write-region "find other frame\n" nil find-other-file nil 'silent)
                        (write-region "find-file-other-frame"
                                      nil "/tmp/nemacs-cmd" nil 'silent)
                        (write-region find-other-file nil "/tmp/nemacs-arg" nil 'silent)
                        (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                        (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                        (write-region "0\t1\t1" nil "/tmp/nemacs-frame-state" nil 'silent)
                        (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                        (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
                        (nemacs-gui-file-bridge-runtime-test--run-ok
                         reader image "(nemacs-gui-file-bridge-run)")
                        (should (equal "find other frame\n"
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-buf")))
                        (should (equal find-other-file
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-file")))
                        (should (equal "0"
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-read-only")))
                        (should (equal "single"
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-window-layout")))
                        (should (equal "0"
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-window-selected")))
                        (ert-info ("find-file-other-frame selects a new frame")
                          (should (equal "1\t2\t2"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-frame-state"))))
                        (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
					            (write-region "read only other window\n" nil read-only-other-file nil 'silent)
					            (write-region "find-file-read-only-other-window"
					                          nil "/tmp/nemacs-cmd" nil 'silent)
				            (write-region read-only-other-file nil "/tmp/nemacs-arg" nil 'silent)
				            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
				            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
				            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
				            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
				            (nemacs-gui-file-bridge-runtime-test--run-ok
				             reader image "(nemacs-gui-file-bridge-run)")
				            (should (equal "read only other window\n"
				                           (nemacs-gui-file-bridge-runtime-test--slurp
				                            "/tmp/nemacs-buf")))
				            (should (equal read-only-other-file
				                           (nemacs-gui-file-bridge-runtime-test--slurp
				                            "/tmp/nemacs-file")))
				            (should (equal "1"
				                           (nemacs-gui-file-bridge-runtime-test--slurp
				                            "/tmp/nemacs-read-only")))
					            (should (equal "vertical"
					                           (nemacs-gui-file-bridge-runtime-test--slurp
					                            "/tmp/nemacs-window-layout")))
	                      (ert-info ("find-file-read-only-other-window selects other window")
						              (should (equal "1"
						                             (nemacs-gui-file-bridge-runtime-test--slurp
						                              "/tmp/nemacs-window-selected"))))
						            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                        (write-region "read only other frame\n" nil read-only-other-file nil 'silent)
                        (write-region "find-file-read-only-other-frame"
                                      nil "/tmp/nemacs-cmd" nil 'silent)
                        (write-region read-only-other-file nil "/tmp/nemacs-arg" nil 'silent)
                        (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                        (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                        (write-region "0\t1\t1" nil "/tmp/nemacs-frame-state" nil 'silent)
                        (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                        (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
                        (nemacs-gui-file-bridge-runtime-test--run-ok
                         reader image "(nemacs-gui-file-bridge-run)")
                        (should (equal "read only other frame\n"
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-buf")))
                        (should (equal read-only-other-file
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-file")))
                        (should (equal "1"
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-read-only")))
                        (should (equal "single"
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-window-layout")))
                        (should (equal "0"
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-window-selected")))
                        (ert-info ("find-file-read-only-other-frame selects a new frame")
                          (should (equal "1\t2\t2"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-frame-state"))))
                        (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
	                        (write-region "find other tab\n" nil find-other-file nil 'silent)
                        (write-region "find-file-other-tab"
                                      nil "/tmp/nemacs-cmd" nil 'silent)
                        (write-region find-other-file nil "/tmp/nemacs-arg" nil 'silent)
                        (write-region "0\t1\t1" nil "/tmp/nemacs-tab-state" nil 'silent)
                        (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                        (write-region "9" nil "/tmp/nemacs-point" nil 'silent)
                        (nemacs-gui-file-bridge-runtime-test--run-ok
                         reader image "(nemacs-gui-file-bridge-run)")
                        (should (equal "find other tab\n"
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-buf")))
                        (should (equal find-other-file
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-file")))
                        (should (equal "0"
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-read-only")))
                        (ert-info ("find-file-other-tab selects a new tab")
                          (should (equal "1\t2\t2"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-tab-state"))))
                        (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                        (write-region "read only other tab\n" nil read-only-other-file nil 'silent)
                        (write-region "find-file-read-only-other-tab"
                                      nil "/tmp/nemacs-cmd" nil 'silent)
                        (write-region read-only-other-file nil "/tmp/nemacs-arg" nil 'silent)
                        (write-region "0\t1\t1" nil "/tmp/nemacs-tab-state" nil 'silent)
                        (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                        (write-region "8" nil "/tmp/nemacs-point" nil 'silent)
                        (nemacs-gui-file-bridge-runtime-test--run-ok
                         reader image "(nemacs-gui-file-bridge-run)")
                        (should (equal "read only other tab\n"
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-buf")))
                        (should (equal read-only-other-file
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-file")))
                        (should (equal "1"
                                       (nemacs-gui-file-bridge-runtime-test--slurp
                                        "/tmp/nemacs-read-only")))
                        (ert-info ("find-file-read-only-other-tab selects a new tab")
                          (should (equal "1\t2\t2"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-tab-state"))))
                        (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                        (let ((project-dir "/tmp/nemacs-project-find-file-test"))
                          (when (file-directory-p project-dir)
                            (delete-directory project-dir t))
                          (make-directory (concat project-dir "/sub/nested") t)
                          (write-region "project find file\n"
                                        nil
                                        (concat project-dir "/sub/nested/target.txt")
                                        nil
                                        'silent)
                          (write-region "project-find-file"
                                        nil "/tmp/nemacs-cmd" nil 'silent)
                          (write-region "nested/target.txt"
                                        nil "/tmp/nemacs-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-keys" nil 'silent)
                          (write-region (concat project-dir "/sub/current.txt")
                                        nil "/tmp/nemacs-file" nil 'silent)
                          (write-region "main"
                                        nil "/tmp/nemacs-buffer-name" nil 'silent)
                          (write-region "old\n"
                                        nil "/tmp/nemacs-buf" nil 'silent)
                          (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                          (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                          (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                          (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
                          (nemacs-gui-file-bridge-runtime-test--run-ok
                           reader image "(nemacs-gui-file-bridge-run)")
                          (should (equal "project find file\n"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-buf")))
                          (should (equal (concat project-dir "/sub/nested/target.txt")
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-file")))
                          (should (equal "0"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-read-only")))
                          (should (equal "single"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-layout")))
                          (should (equal "0"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-selected")))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                          (delete-directory project-dir t))
                        (let ((project-dir "/tmp/nemacs-project-or-external-find-file-test")
                              (external-file "/tmp/nemacs-project-or-external-external.txt"))
                          (when (file-directory-p project-dir)
                            (delete-directory project-dir t))
                          (when (file-exists-p external-file)
                            (delete-file external-file))
                          (make-directory (concat project-dir "/sub/nested") t)
                          (write-region "project target\n"
                                        nil
                                        (concat project-dir "/sub/nested/project.txt")
                                        nil
                                        'silent)
                          (write-region "external target\n"
                                        nil external-file nil 'silent)
                          (write-region "project-or-external-find-file"
                                        nil "/tmp/nemacs-cmd" nil 'silent)
                          (write-region "nested/project.txt"
                                        nil "/tmp/nemacs-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-keys" nil 'silent)
                          (write-region (concat project-dir "/sub/current.txt")
                                        nil "/tmp/nemacs-file" nil 'silent)
                          (write-region "main"
                                        nil "/tmp/nemacs-buffer-name" nil 'silent)
                          (write-region "old\n"
                                        nil "/tmp/nemacs-buf" nil 'silent)
                          (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                          (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                          (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                          (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
                          (nemacs-gui-file-bridge-runtime-test--run-ok
                           reader image "(nemacs-gui-file-bridge-run)")
                          (should (equal "project target\n"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-buf")))
                          (should (equal (concat project-dir "/sub/nested/project.txt")
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-file")))
                          (should (equal "0"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-read-only")))
                          (should (equal "single"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-layout")))
                          (should (equal "0"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-selected")))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                          (write-region "project-or-external-find-file"
                                        nil "/tmp/nemacs-cmd" nil 'silent)
                          (write-region external-file
                                        nil "/tmp/nemacs-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-keys" nil 'silent)
                          (write-region (concat project-dir "/sub/current.txt")
                                        nil "/tmp/nemacs-file" nil 'silent)
                          (write-region "main"
                                        nil "/tmp/nemacs-buffer-name" nil 'silent)
                          (write-region "old\n"
                                        nil "/tmp/nemacs-buf" nil 'silent)
                          (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                          (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                          (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                          (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
                          (nemacs-gui-file-bridge-runtime-test--run-ok
                           reader image "(nemacs-gui-file-bridge-run)")
                          (should (equal "external target\n"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-buf")))
                          (should (equal external-file
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-file")))
                          (should (equal "0"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-read-only")))
                          (should (equal "single"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-layout")))
                          (should (equal "0"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-selected")))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                          (delete-file external-file)
                          (delete-directory project-dir t))
                        (let ((project-dir "/tmp/nemacs-project-find-dir-test"))
                          (when (file-directory-p project-dir)
                            (delete-directory project-dir t))
                          (make-directory (concat project-dir "/sub/nested") t)
                          (write-region "project dir file\n"
                                        nil
                                        (concat project-dir "/sub/nested/alpha.txt")
                                        nil
                                        'silent)
                          (write-region "project-find-dir"
                                        nil "/tmp/nemacs-cmd" nil 'silent)
                          (write-region "nested"
                                        nil "/tmp/nemacs-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-keys" nil 'silent)
                          (write-region (concat project-dir "/sub/current.txt")
                                        nil "/tmp/nemacs-file" nil 'silent)
                          (write-region "main"
                                        nil "/tmp/nemacs-buffer-name" nil 'silent)
                          (write-region "old\n"
                                        nil "/tmp/nemacs-buf" nil 'silent)
                          (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                          (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                          (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
                          (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
                          (nemacs-gui-file-bridge-runtime-test--run-ok
                           reader image "(nemacs-gui-file-bridge-run)")
                          (should (equal "*Directory*"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-buffer-name")))
                          (let ((directory-buffer
                                 (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-buf")))
                            (should (string-match-p
                                     (regexp-quote
                                      (concat "Directory " project-dir "/sub/nested\n"))
                                     directory-buffer)))
                          (should (equal ""
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-file")))
                          (should (equal "single"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-layout")))
                          (should (equal "0"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-selected")))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
                          (delete-directory project-dir t))
                        (let ((project-dir "/tmp/nemacs-project-dired-test"))
                          (when (file-directory-p project-dir)
                            (delete-directory project-dir t))
                          (make-directory (concat project-dir "/sub") t)
                          (write-region "project root file\n"
                                        nil
                                        (concat project-dir "/sub/root.txt")
                                        nil
                                        'silent)
                          (write-region "project-dired"
                                        nil "/tmp/nemacs-cmd" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-keys" nil 'silent)
                          (write-region (concat project-dir "/sub/current.txt")
                                        nil "/tmp/nemacs-file" nil 'silent)
                          (write-region "main"
                                        nil "/tmp/nemacs-buffer-name" nil 'silent)
                          (write-region "old\n"
                                        nil "/tmp/nemacs-buf" nil 'silent)
                          (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                          (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                          (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
                          (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
                          (nemacs-gui-file-bridge-runtime-test--run-ok
                           reader image "(nemacs-gui-file-bridge-run)")
                          (should (equal "*Directory*"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-buffer-name")))
                          (let ((directory-buffer
                                 (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-buf")))
                            (should (string-match-p
                                     (regexp-quote
                                      (concat "Directory " project-dir "/sub\n"))
                                     directory-buffer)))
                          (should (equal ""
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-file")))
                          (should (equal "single"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-layout")))
                          (should (equal "0"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-selected")))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
                          (delete-directory project-dir t))
                        (let ((project-dir "/tmp/nemacs-project-switch-project-test"))
                          (when (file-directory-p project-dir)
                            (delete-directory project-dir t))
                          (make-directory project-dir t)
                          (write-region "project switch file\n"
                                        nil
                                        (concat project-dir "/file.txt")
                                        nil
                                        'silent)
                          (write-region "project-switch-project"
                                        nil "/tmp/nemacs-cmd" nil 'silent)
                          (write-region project-dir
                                        nil "/tmp/nemacs-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-keys" nil 'silent)
                          (write-region "/tmp/nemacs-current-project-switch-source.txt"
                                        nil "/tmp/nemacs-file" nil 'silent)
                          (write-region "main"
                                        nil "/tmp/nemacs-buffer-name" nil 'silent)
                          (write-region "old\n"
                                        nil "/tmp/nemacs-buf" nil 'silent)
                          (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                          (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                          (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
                          (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
                          (nemacs-gui-file-bridge-runtime-test--run-ok
                           reader image "(nemacs-gui-file-bridge-run)")
                          (should (equal "*Directory*"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-buffer-name")))
                          (let ((directory-buffer
                                 (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-buf")))
                            (should (string-match-p
                                     (regexp-quote
                                      (concat "Directory " project-dir "\n"))
                                     directory-buffer)))
                          (should (equal ""
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-file")))
                          (should (equal "single"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-layout")))
                          (should (equal "0"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-selected")))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
                          (delete-directory project-dir t))
                        (let ((project-dir "/tmp/nemacs-project-any-command-test"))
                          (when (file-directory-p project-dir)
                            (delete-directory project-dir t))
                          (make-directory (concat project-dir "/sub") t)
                          (write-region "project any file\n"
                                        nil
                                        (concat project-dir "/sub/file.txt")
                                        nil
                                        'silent)
                          (write-region "project-any-command"
                                        nil "/tmp/nemacs-cmd" nil 'silent)
                          (write-region "project-dired"
                                        nil "/tmp/nemacs-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-keys" nil 'silent)
                          (write-region (concat project-dir "/sub/current.txt")
                                        nil "/tmp/nemacs-file" nil 'silent)
                          (write-region "main"
                                        nil "/tmp/nemacs-buffer-name" nil 'silent)
                          (write-region "old\n"
                                        nil "/tmp/nemacs-buf" nil 'silent)
                          (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                          (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                          (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
                          (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
                          (nemacs-gui-file-bridge-runtime-test--run-ok
                           reader image "(nemacs-gui-file-bridge-run)")
                          (should (equal "*Directory*"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-buffer-name")))
                          (let ((directory-buffer
                                 (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-buf")))
                            (should (string-match-p
                                     (regexp-quote
                                      (concat "Directory " project-dir "/sub\n"))
                                     directory-buffer)))
                          (should (equal ""
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-file")))
                          (should (equal "single"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-layout")))
                          (should (equal "0"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-selected")))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
                          (delete-directory project-dir t))
                        (let ((project-dir "/tmp/nemacs-project-execute-extended-command-test"))
                          (when (file-directory-p project-dir)
                            (delete-directory project-dir t))
                          (make-directory (concat project-dir "/sub") t)
                          (write-region "project extended file\n"
                                        nil
                                        (concat project-dir "/sub/file.txt")
                                        nil
                                        'silent)
                          (write-region "project-execute-extended-command"
                                        nil "/tmp/nemacs-cmd" nil 'silent)
                          (write-region "project-dired"
                                        nil "/tmp/nemacs-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-keys" nil 'silent)
                          (write-region (concat project-dir "/sub/current.txt")
                                        nil "/tmp/nemacs-file" nil 'silent)
                          (write-region "main"
                                        nil "/tmp/nemacs-buffer-name" nil 'silent)
                          (write-region "old\n"
                                        nil "/tmp/nemacs-buf" nil 'silent)
                          (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                          (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                          (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
                          (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
                          (nemacs-gui-file-bridge-runtime-test--run-ok
                           reader image "(nemacs-gui-file-bridge-run)")
                          (should (equal "*Directory*"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-buffer-name")))
                          (let ((directory-buffer
                                 (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-buf")))
                            (should (string-match-p
                                     (regexp-quote
                                      (concat "Directory " project-dir "/sub\n"))
                                     directory-buffer)))
                          (should (equal ""
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-file")))
                          (should (equal "single"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-layout")))
                          (should (equal "0"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-selected")))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
                          (delete-directory project-dir t))
                        (let ((project-dir "/tmp/nemacs-project-other-window-command-test"))
                          (when (file-directory-p project-dir)
                            (delete-directory project-dir t))
                          (make-directory (concat project-dir "/sub") t)
                          (write-region "project other window file\n"
                                        nil
                                        (concat project-dir "/sub/file.txt")
                                        nil
                                        'silent)
                          (write-region "project-other-window-command"
                                        nil "/tmp/nemacs-cmd" nil 'silent)
                          (write-region "project-dired"
                                        nil "/tmp/nemacs-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-keys" nil 'silent)
                          (write-region (concat project-dir "/sub/current.txt")
                                        nil "/tmp/nemacs-file" nil 'silent)
                          (write-region "main"
                                        nil "/tmp/nemacs-buffer-name" nil 'silent)
                          (write-region "old\n"
                                        nil "/tmp/nemacs-buf" nil 'silent)
                          (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                          (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                          (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
                          (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
                          (nemacs-gui-file-bridge-runtime-test--run-ok
                           reader image "(nemacs-gui-file-bridge-run)")
                          (should (equal "*Directory*"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-buffer-name")))
                          (let ((directory-buffer
                                 (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-buf")))
                            (should (string-match-p
                                     (regexp-quote
                                      (concat "Directory " project-dir "/sub\n"))
                                     directory-buffer)))
                          (should (equal ""
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-file")))
                          (should (equal "vertical"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-layout")))
                          (should (equal "1"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-selected")))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
                          (delete-directory project-dir t))
                        (let ((project-dir "/tmp/nemacs-project-other-tab-command-test"))
                          (when (file-directory-p project-dir)
                            (delete-directory project-dir t))
                          (make-directory (concat project-dir "/sub") t)
                          (write-region "project other tab file\n"
                                        nil
                                        (concat project-dir "/sub/file.txt")
                                        nil
                                        'silent)
                          (write-region "project-other-tab-command"
                                        nil "/tmp/nemacs-cmd" nil 'silent)
                          (write-region "project-dired"
                                        nil "/tmp/nemacs-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
                          (write-region ""
                                        nil "/tmp/nemacs-keys" nil 'silent)
                          (write-region (concat project-dir "/sub/current.txt")
                                        nil "/tmp/nemacs-file" nil 'silent)
                          (write-region "main"
                                        nil "/tmp/nemacs-buffer-name" nil 'silent)
                          (write-region "old\n"
                                        nil "/tmp/nemacs-buf" nil 'silent)
                          (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
                          (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
                          (write-region "0\t1\t1" nil "/tmp/nemacs-tab-state" nil 'silent)
                          (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
                          (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
                          (nemacs-gui-file-bridge-runtime-test--run-ok
                           reader image "(nemacs-gui-file-bridge-run)")
                          (should (equal "*Directory*"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-buffer-name")))
                          (let ((directory-buffer
                                 (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-buf")))
                            (should (string-match-p
                                     (regexp-quote
                                      (concat "Directory " project-dir "/sub\n"))
                                     directory-buffer)))
                          (should (equal ""
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-file")))
                          (should (equal "single"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-layout")))
                          (should (equal "0"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-window-selected")))
                          (should (equal "1\t2\t2"
                                         (nemacs-gui-file-bridge-runtime-test--slurp
                                          "/tmp/nemacs-tab-state")))
	                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
	                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	                          (delete-directory project-dir t))
	                        (let ((project-dir "/tmp/nemacs-project-other-frame-command-test"))
	                          (when (file-directory-p project-dir)
	                            (delete-directory project-dir t))
	                          (make-directory (concat project-dir "/sub") t)
	                          (write-region "project other frame file\n"
	                                        nil
	                                        (concat project-dir "/sub/file.txt")
	                                        nil
	                                        'silent)
	                          (write-region "project-other-frame-command"
	                                        nil "/tmp/nemacs-cmd" nil 'silent)
	                          (write-region "project-dired"
	                                        nil "/tmp/nemacs-arg" nil 'silent)
	                          (write-region ""
	                                        nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
	                          (write-region ""
	                                        nil "/tmp/nemacs-keys" nil 'silent)
	                          (write-region (concat project-dir "/sub/current.txt")
	                                        nil "/tmp/nemacs-file" nil 'silent)
	                          (write-region "main"
	                                        nil "/tmp/nemacs-buffer-name" nil 'silent)
	                          (write-region "old\n"
	                                        nil "/tmp/nemacs-buf" nil 'silent)
	                          (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
	                          (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
	                          (write-region "0\t1\t1" nil "/tmp/nemacs-frame-state" nil 'silent)
	                          (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
	                          (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
	                          (nemacs-gui-file-bridge-runtime-test--run-ok
	                           reader image "(nemacs-gui-file-bridge-run)")
	                          (should (equal "*Directory*"
	                                         (nemacs-gui-file-bridge-runtime-test--slurp
	                                          "/tmp/nemacs-buffer-name")))
	                          (let ((directory-buffer
	                                 (nemacs-gui-file-bridge-runtime-test--slurp
	                                  "/tmp/nemacs-buf")))
	                            (should (string-match-p
	                                     (regexp-quote
	                                      (concat "Directory " project-dir "/sub\n"))
	                                     directory-buffer)))
	                          (should (equal ""
	                                         (nemacs-gui-file-bridge-runtime-test--slurp
	                                          "/tmp/nemacs-file")))
	                          (should (equal "single"
	                                         (nemacs-gui-file-bridge-runtime-test--slurp
	                                          "/tmp/nemacs-window-layout")))
	                          (should (equal "0"
	                                         (nemacs-gui-file-bridge-runtime-test--slurp
	                                          "/tmp/nemacs-window-selected")))
	                          (should (equal "1\t2\t2"
	                                         (nemacs-gui-file-bridge-runtime-test--slurp
	                                          "/tmp/nemacs-frame-state")))
	                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
	                          (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	                          (delete-directory project-dir t))
					            (let ((some-main-file
			                   (make-temp-file "nemacs-gui-file-bridge-some-main-"))
	                  (some-other-file
	                   (make-temp-file "nemacs-gui-file-bridge-some-other-"))
	                  (some-read-only-file
	                   (make-temp-file "nemacs-gui-file-bridge-some-ro-")))
	              (unwind-protect
	                  (progn
	                    (write-region "old main\n" nil some-main-file nil 'silent)
	                    (write-region "old other\n" nil some-other-file nil 'silent)
	                    (write-region "old read only\n"
	                                  nil some-read-only-file nil 'silent)
	                    (write-region "main changed\n" nil "/tmp/nemacs-buf" nil 'silent)
	                    (write-region some-main-file nil "/tmp/nemacs-file" nil 'silent)
	                    (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
	                    (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
	                    (write-region "save-some-buffers" nil "/tmp/nemacs-cmd" nil 'silent)
	                    (write-region "main\nother\nreadonly\n"
	                                  nil "/tmp/nemacs-buffer-list" nil 'silent)
	                    (write-region "other changed\n"
	                                  nil "/tmp/nemacs-buffer-store/other" nil 'silent)
	                    (write-region some-other-file
	                                  nil "/tmp/nemacs-buffer-file-store/other" nil 'silent)
	                    (write-region "0"
	                                  nil "/tmp/nemacs-buffer-read-only-store/other" nil 'silent)
	                    (write-region "read only changed\n"
	                                  nil "/tmp/nemacs-buffer-store/readonly" nil 'silent)
	                    (write-region some-read-only-file
	                                  nil "/tmp/nemacs-buffer-file-store/readonly" nil 'silent)
	                    (write-region "1"
	                                  nil "/tmp/nemacs-buffer-read-only-store/readonly" nil 'silent)
	                    (nemacs-gui-file-bridge-runtime-test--run-ok
	                     reader image "(nemacs-gui-file-bridge-run)")
	                    (should (equal "main changed\n"
	                                   (nemacs-gui-file-bridge-runtime-test--slurp
	                                    some-main-file)))
	                    (should (equal "other changed\n"
	                                   (nemacs-gui-file-bridge-runtime-test--slurp
	                                    some-other-file)))
	                    (should (equal "old read only\n"
	                                   (nemacs-gui-file-bridge-runtime-test--slurp
	                                    some-read-only-file)))
	                    (should (equal "main changed\n"
	                                   (nemacs-gui-file-bridge-runtime-test--slurp
	                                    "/tmp/nemacs-buffer-store/main"))))
	                (dolist (file (list some-main-file
	                                    some-other-file
	                                    some-read-only-file))
	                  (when (file-exists-p file)
	                    (delete-file file)))))
	            (write-region "main text\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "/tmp/nemacs-main-file.txt" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "other text\n"
                          nil "/tmp/nemacs-buffer-store/other" nil 'silent)
            (write-region "/tmp/nemacs-other-file.txt"
                          nil "/tmp/nemacs-buffer-file-store/other" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-buffer-point-store/other" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-buffer-mark-store/other" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-buffer-window-start-store/other" nil 'silent)
            (write-region "switch-to-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "other" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "other text\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "/tmp/nemacs-other-file.txt"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-file")))
            (should (equal "other"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "main text\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-store/main")))
            (should (equal "00004"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-point-store/main")))
            (should (equal "00001"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-mark-store/main")))
            (should (equal "00001"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-window-start-store/main")))
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 5 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (should (equal "00002"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "other changed\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "main text\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
	            (should (equal "other changed\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-store/other")))
	            (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 1 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (write-region "rename-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "renamed" nil "/tmp/nemacs-arg" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
	            (write-region "1" nil "/tmp/nemacs-mark" nil 'silent)
	            (write-region "1" nil "/tmp/nemacs-window-start" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "renamed"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-name")))
	            (should (equal "main text\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (should (equal "main text\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-store/renamed")))
	            (should (equal "/tmp/nemacs-main-file.txt"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-file-store/renamed")))
	            (should (equal "00004"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-point-store/renamed")))
	            (should (equal "00001"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-mark-store/renamed")))
	            (should (equal "00001"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-window-start-store/renamed")))
	            (should (equal ""
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-store/main")))
	            (should (string-match-p
	                     (regexp-quote "renamed\n")
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-buffer-list")))
	            (should-not (string-match-p
	                         (regexp-quote "main\n")
	                         (nemacs-gui-file-bridge-runtime-test--slurp
	                          "/tmp/nemacs-buffer-list")))
            (write-region "rename-uniquely" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "renamed" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "main text\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "/tmp/nemacs-main-file.txt" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "renamed\nrenamed<2>\nother\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "renamed<3>"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "main text\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-store/renamed<3>")))
            (should (string-match-p
                     (regexp-quote "renamed<3>\n")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buffer-list")))
            (should-not (string-match-p
                         (regexp-quote "renamed\n")
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-buffer-list")))
            (write-region "other insert\n" nil "/tmp/nemacs-buffer-store/other" nil 'silent)
            (write-region "insert-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "other" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "before after\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "before other insert\nafter\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 20 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "other changed\n" nil "/tmp/nemacs-buffer-store/other" nil 'silent)
            (write-region "clone-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "clone me\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main\nmain<2>\nother\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "main<3>"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "clone me\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "clone me\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-store/main<3>")))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-file-store/main<3>")))
	            (should (string-match-p
	                     (regexp-quote "main<3>\n")
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-buffer-list")))
	            (should (= 5 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 1 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "/tmp/nemacs-main-file.txt"
                          nil "/tmp/nemacs-buffer-file-store/main" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "/tmp/nemacs-main-file.txt" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "indirect clone\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "main\nmain<2>\nmain<3>\nother\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "clone-indirect-buffer-other-window" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "main<4>"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "indirect clone\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-store/main<4>")))
            (should (equal "/tmp/nemacs-main-file.txt"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-file-store/main<4>")))
            (should (equal "vertical"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-layout")))
            (ert-info ("clone-indirect-buffer-other-window selects other window")
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected"))))
            (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (write-region "/tmp/nemacs-main-file.txt" nil "/tmp/nemacs-buffer-file-store/main" nil 'silent)
		            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                (write-region "/tmp/nemacs-main-file.txt" nil "/tmp/nemacs-file" nil 'silent)
	            (write-region "main text\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "main\nother\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
	            (write-region "switch-to-buffer-other-window" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "other" nil "/tmp/nemacs-arg" nil 'silent)
	            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "other changed\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
		            (should (equal "vertical"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            "/tmp/nemacs-window-layout")))
				                (ert-info ("switch-to-buffer-other-window selects other window")
					              (should (equal "1"
					                             (nemacs-gui-file-bridge-runtime-test--slurp
					                              "/tmp/nemacs-window-selected"))))
				            (should (= 7 (nemacs-gui-file-bridge-runtime-test--point-value)))
				            (should (= 6 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "other changed\n" nil "/tmp/nemacs-buffer-store/other" nil 'silent)
            (write-region "/tmp/nemacs-other-file.txt"
                          nil "/tmp/nemacs-buffer-file-store/other" nil 'silent)
            (write-region "7" nil "/tmp/nemacs-buffer-point-store/other" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-buffer-mark-store/other" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-buffer-window-start-store/other" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "/tmp/nemacs-main-file.txt" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main text\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "main\nother\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
            (write-region "switch-to-buffer-other-frame" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "other" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0\t1\t1" nil "/tmp/nemacs-frame-state" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "other"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "other changed\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "single"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-layout")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-selected")))
            (ert-info ("switch-to-buffer-other-frame selects a new frame")
              (should (equal "1\t2\t2"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-frame-state"))))
            (should (= 7 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 6 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "main text\n" nil "/tmp/nemacs-buffer-store/main" nil 'silent)
            (write-region "/tmp/nemacs-main-file.txt" nil "/tmp/nemacs-buffer-file-store/main" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-buffer-point-store/main" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-buffer-mark-store/main" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-buffer-window-start-store/main" nil 'silent)
            (write-region "display-buffer-other-frame" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0\t1\t1" nil "/tmp/nemacs-frame-state" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "main"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "main text\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "single"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-layout")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-selected")))
            (ert-info ("display-buffer-other-frame selects a new frame")
              (should (equal "1\t2\t2"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-frame-state"))))
            (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "compose-mail" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "main text\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "/tmp/nemacs-main-file.txt" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*mail*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "To: \nSubject: \n\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-file")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-read-only")))
            (should (string-match-p
                     (regexp-quote "*mail*\n")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buffer-list")))
            (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 4 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "compose-mail-other-window" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "main text\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "/tmp/nemacs-main-file.txt" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*mail*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "vertical"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-layout")))
            (ert-info ("compose-mail-other-window selects other window")
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected"))))
            (write-region "compose-mail-other-frame" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "main text\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "/tmp/nemacs-main-file.txt" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0\t1\t1" nil "/tmp/nemacs-frame-state" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*mail*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "single"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-layout")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-selected")))
            (ert-info ("compose-mail-other-frame selects a new frame")
              (should (equal "1\t2\t2"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-frame-state"))))
            (write-region "project buffer\n" nil "/tmp/nemacs-buffer-store/proj" nil 'silent)
            (write-region "/tmp/nemacs-project-switch-test/proj.txt"
                          nil "/tmp/nemacs-buffer-file-store/proj" nil 'silent)
            (write-region "11" nil "/tmp/nemacs-buffer-point-store/proj" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-buffer-mark-store/proj" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-buffer-window-start-store/proj" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-buffer-read-only-store/proj" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-buffer-modified-store/proj" nil 'silent)
            (write-region "outside buffer\n" nil "/tmp/nemacs-buffer-store/outside" nil 'silent)
            (write-region "/tmp/nemacs-outside-switch-test.txt"
                          nil "/tmp/nemacs-buffer-file-store/outside" nil 'silent)
            (write-region "main\nproj\noutside\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "/tmp/nemacs-project-switch-test/main.txt" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main text\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "project-switch-to-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "proj" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "proj"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "project buffer\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "/tmp/nemacs-project-switch-test/proj.txt"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-file")))
            (should (= 11 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (should (equal "00001"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (should (equal "single"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-layout")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-selected")))
			            (write-region "/tmp/nemacs-main-file.txt" nil "/tmp/nemacs-buffer-file-store/main" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "/tmp/nemacs-main-file.txt" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main text\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "main\nother\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
            (write-region "switch-to-buffer-other-tab" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "other" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0\t1\t1" nil "/tmp/nemacs-tab-state" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "other changed\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "other"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (ert-info ("switch-to-buffer-other-tab selects a new tab")
              (should (equal "1\t2\t2"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-tab-state"))))
            (should (= 7 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 6 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (write-region "list-buffers" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Buffer List*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (string-match-p
                     (regexp-quote "Buffer\tFile\n")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (string-match-p
                     (regexp-quote "  main\t/tmp/nemacs-main-file.txt\n")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (string-match-p
                     (regexp-quote "* other\t/tmp/nemacs-other-file.txt\n")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (string-match-p
	                     (regexp-quote "  *Buffer List*\t\n")
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-buf")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (let ((dir "/tmp/nemacs-project-list-buffers-test"))
              (when (file-directory-p dir)
                (delete-directory dir t))
              (make-directory (concat dir "/sub") t)
              (write-region "main text\n" nil "/tmp/nemacs-buffer-store/main" nil 'silent)
              (write-region (concat dir "/sub/main.txt") nil "/tmp/nemacs-buffer-file-store/main" nil 'silent)
              (write-region "project buffer\n" nil "/tmp/nemacs-buffer-store/proj" nil 'silent)
              (write-region (concat dir "/sub/proj.txt") nil "/tmp/nemacs-buffer-file-store/proj" nil 'silent)
              (write-region "outside buffer\n" nil "/tmp/nemacs-buffer-store/outside" nil 'silent)
              (write-region "/tmp/nemacs-outside-list-buffers-test.txt" nil "/tmp/nemacs-buffer-file-store/outside" nil 'silent)
              (write-region "main\nproj\noutside\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
              (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
              (write-region (concat dir "/sub/main.txt") nil "/tmp/nemacs-file" nil 'silent)
              (write-region "main text\n" nil "/tmp/nemacs-buf" nil 'silent)
              (write-region "project-list-buffers" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
              (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "*Buffer List*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (let ((project-buffer-list
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
                (should (string-match-p
                         (regexp-quote "Buffer\tFile\n")
                         project-buffer-list))
                (should (string-match-p
                         (regexp-quote
                          "* main\t/tmp/nemacs-project-list-buffers-test/sub/main.txt\n")
                         project-buffer-list))
                (should (string-match-p
                         (regexp-quote
                          "  proj\t/tmp/nemacs-project-list-buffers-test/sub/proj.txt\n")
                         project-buffer-list))
                (should-not (string-match-p
                             (regexp-quote "outside")
                             project-buffer-list)))
              (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
              (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
              (delete-directory dir t))
            (let ((dir "/tmp/nemacs-project-kill-buffers-test"))
              (when (file-directory-p dir)
                (delete-directory dir t))
              (make-directory (concat dir "/sub") t)
              (write-region "main project\n" nil "/tmp/nemacs-buffer-store/main" nil 'silent)
              (write-region (concat dir "/sub/main.txt") nil "/tmp/nemacs-buffer-file-store/main" nil 'silent)
              (write-region "project buffer\n" nil "/tmp/nemacs-buffer-store/proj" nil 'silent)
              (write-region (concat dir "/sub/proj.txt") nil "/tmp/nemacs-buffer-file-store/proj" nil 'silent)
              (write-region "outside buffer\n" nil "/tmp/nemacs-buffer-store/outside" nil 'silent)
              (write-region "/tmp/nemacs-outside-kill-buffers-test.txt" nil "/tmp/nemacs-buffer-file-store/outside" nil 'silent)
              (write-region "9" nil "/tmp/nemacs-buffer-point-store/outside" nil 'silent)
              (write-region "3" nil "/tmp/nemacs-buffer-mark-store/outside" nil 'silent)
              (write-region "2" nil "/tmp/nemacs-buffer-window-start-store/outside" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-buffer-read-only-store/outside" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-buffer-modified-store/outside" nil 'silent)
              (write-region "main\nproj\noutside\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
              (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
              (write-region (concat dir "/sub/main.txt") nil "/tmp/nemacs-file" nil 'silent)
              (write-region "main project\n" nil "/tmp/nemacs-buf" nil 'silent)
              (write-region "project-kill-buffers" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
              (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "outside"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "outside buffer\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "/tmp/nemacs-outside-kill-buffers-test.txt"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal ""
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-store/main")))
              (should (equal ""
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-store/proj")))
              (should (equal "outside\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-list")))
              (should (= 9 (nemacs-gui-file-bridge-runtime-test--point-value)))
              (should (= 3 (nemacs-gui-file-bridge-runtime-test--mark-value)))
              (should (equal "00002"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))
              (delete-directory dir t))
            (let ((dir "/tmp/nemacs-list-directory-test"))
              (when (file-directory-p dir)
                (delete-directory dir t))
              (make-directory (concat dir "/subdir") t)
              (write-region "alpha\n" nil (concat dir "/alpha.txt") nil 'silent)
              (write-region "list-directory" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
              (write-region dir nil "/tmp/nemacs-arg" nil 'silent)
              (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
              (write-region "" nil "/tmp/nemacs-file" nil 'silent)
              (write-region "body\n" nil "/tmp/nemacs-buf" nil 'silent)
              (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
              (write-region "1" nil "/tmp/nemacs-mark" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "*Directory*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (let ((directory-buffer
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
                (should (string-match-p
                         (regexp-quote (concat "Directory " dir "\n"))
                         directory-buffer)))
              (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
              (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
              (delete-directory dir t))
            (let ((dir "/tmp/nemacs-dired-test"))
              (when (file-directory-p dir)
                (delete-directory dir t))
              (make-directory dir t)
              (write-region "dired" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
              (write-region dir nil "/tmp/nemacs-arg" nil 'silent)
              (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
              (write-region "" nil "/tmp/nemacs-file" nil 'silent)
              (write-region "body\n" nil "/tmp/nemacs-buf" nil 'silent)
              (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
              (write-region "1" nil "/tmp/nemacs-mark" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "*Directory*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (string-match-p
                       (regexp-quote (concat "Directory " dir "\n"))
                       (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (delete-directory dir t))
            (let ((dir "/tmp/nemacs-dired-jump-test"))
              (when (file-directory-p dir)
                (delete-directory dir t))
              (make-directory dir t)
              (write-region "file\n" nil (concat dir "/file.txt") nil 'silent)
              (write-region "dired-jump" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
              (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
              (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
              (write-region (concat dir "/file.txt") nil "/tmp/nemacs-file" nil 'silent)
              (write-region "body\n" nil "/tmp/nemacs-buf" nil 'silent)
              (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
              (write-region "1" nil "/tmp/nemacs-mark" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
              (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "*Directory*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (string-match-p
                       (regexp-quote (concat "Directory " dir "\n"))
                       (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (write-region "dired-jump-other-window" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
              (write-region (concat dir "/file.txt") nil "/tmp/nemacs-file" nil 'silent)
              (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "*Directory*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
	              (should (equal "vertical"
	                             (nemacs-gui-file-bridge-runtime-test--slurp
	                              "/tmp/nemacs-window-layout")))
	                (ert-info ("dired-jump-other-window selects other window")
	                (should (equal "1"
	                               (nemacs-gui-file-bridge-runtime-test--slurp
	                                "/tmp/nemacs-window-selected"))))
              (write-region "dired-other-window" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
              (write-region dir nil "/tmp/nemacs-arg" nil 'silent)
              (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "*Directory*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
	              (should (equal "vertical"
	                             (nemacs-gui-file-bridge-runtime-test--slurp
	                              "/tmp/nemacs-window-layout")))
			                (ert-info ("dired-other-window selects other window")
			                (should (equal "1"
			                               (nemacs-gui-file-bridge-runtime-test--slurp
			                                "/tmp/nemacs-window-selected"))))
              (write-region "dired-other-frame" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
              (write-region dir nil "/tmp/nemacs-arg" nil 'silent)
              (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
              (write-region "0\t1\t1" nil "/tmp/nemacs-frame-state" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "*Directory*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "single"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "0"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (ert-info ("dired-other-frame selects a new frame")
                (should (equal "1\t2\t2"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-frame-state"))))
              (write-region "dired-other-tab" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
              (write-region dir nil "/tmp/nemacs-arg" nil 'silent)
              (write-region "0\t1\t1" nil "/tmp/nemacs-tab-state" nil 'silent)
              (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "*Directory*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
	              (ert-info ("dired-other-tab selects a new tab")
	                (should (equal "1\t2\t2"
	                               (nemacs-gui-file-bridge-runtime-test--slurp
	                                "/tmp/nemacs-tab-state"))))
              (write-region "old entry\n" nil (concat dir "/ChangeLog") nil 'silent)
              (write-region "add-change-log-entry-other-window" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
              (write-region (concat dir "/file.txt") nil "/tmp/nemacs-file" nil 'silent)
              (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
              (write-region "body\n" nil "/tmp/nemacs-buf" nil 'silent)
              (write-region "body\n" nil "/tmp/nemacs-buffer-store/main" nil 'silent)
              (write-region (concat dir "/file.txt") nil "/tmp/nemacs-buffer-file-store/main" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-buffer-read-only-store/main" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
              (write-region "2026-06-09" nil "/tmp/nemacs-change-log-date" nil 'silent)
              (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "ChangeLog"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal (concat dir "/ChangeLog")
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (string-match-p
                       (regexp-quote "* file.txt: ")
                       (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (string-match-p
                       (regexp-quote "old entry\n")
                       (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-buf")))
              (should (equal "vertical"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (ert-info ("add-change-log-entry-other-window selects other window")
                (should (equal "1"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-window-selected"))))
              (ert-info ("ChangeLog buffer remains modified until saved")
                (should (equal "1"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-buffer-modified-store/ChangeLog"))))
		              (delete-directory dir t))
            (write-region "alpha\nbeta\nalpha beta\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "occur" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "alpha" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Occur*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "2 matches for \"alpha\" in buffer: main\n      1:alpha\n      3:alpha beta\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "alpha\nbeta\nalpha beta\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-store/main")))
            (should (string-match-p
                     (regexp-quote "*Occur*\n")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buffer-list")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "(fset 'alpha\n  (lambda () nil))\n(defun beta () nil)\n(setq gamma 1)\nplain\n"
                          nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "imenu" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Imenu*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal "Imenu index for buffer: main\n      1:alpha\n      3:beta\n      4:gamma\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "(fset 'alpha\n  (lambda () nil))\n(defun beta () nil)\n(setq gamma 1)\nplain\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-store/main")))
            (should (string-match-p
                     (regexp-quote "*Imenu*\n")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buffer-list")))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-read-only")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "switch-to-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "other" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "other changed\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 7 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 6 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "kill-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "other changed\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "other"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-store/main")))
            (should (equal "00000"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-point-store/main")))
            (should (equal "00000"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-mark-store/main")))
            (should (equal "00000"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-window-start-store/main")))
            (write-region "main replacement\n"
                          nil "/tmp/nemacs-buffer-store/main" nil 'silent)
            (write-region "/tmp/nemacs-main-file.txt"
                          nil "/tmp/nemacs-buffer-file-store/main" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-buffer-point-store/main" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-buffer-mark-store/main" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-buffer-window-start-store/main" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "main replacement\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "main"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-store/other")))
	            (should (equal "00000"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-point-store/other")))
	            (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 1 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (should (equal "00001"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-start")))
	            (write-region "other changed\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "/tmp/nemacs-other-file.txt" nil "/tmp/nemacs-file" nil 'silent)
	            (write-region "other" nil "/tmp/nemacs-buffer-name" nil 'silent)
	            (write-region "main\nother\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
	            (write-region "main text\n"
	                          nil "/tmp/nemacs-buffer-store/main" nil 'silent)
	            (write-region "/tmp/nemacs-main-file.txt"
	                          nil "/tmp/nemacs-buffer-file-store/main" nil 'silent)
	            (write-region "4" nil "/tmp/nemacs-buffer-point-store/main" nil 'silent)
	            (write-region "1" nil "/tmp/nemacs-buffer-mark-store/main" nil 'silent)
	            (write-region "1" nil "/tmp/nemacs-buffer-window-start-store/main" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-buffer-read-only-store/main" nil 'silent)
	            (write-region "kill-buffer-and-window" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "vertical" nil "/tmp/nemacs-window-layout" nil 'silent)
	            (write-region "1" nil "/tmp/nemacs-window-selected" nil 'silent)
	            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
	            (write-region "6" nil "/tmp/nemacs-mark" nil 'silent)
	            (write-region "3" nil "/tmp/nemacs-window-start" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "main text\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (should (equal "/tmp/nemacs-main-file.txt"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-file")))
	            (should (equal "main"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-name")))
	            (should (equal ""
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-store/other")))
	            (should (equal "single"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-layout")))
	            (should (equal "0"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-selected")))
	            (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 1 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (should (equal "00001"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-start")))
	            (write-region "other raw changed\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "/tmp/nemacs-other-file.txt" nil "/tmp/nemacs-file" nil 'silent)
	            (write-region "other" nil "/tmp/nemacs-buffer-name" nil 'silent)
	            (write-region "main\nother\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
	            (write-region "main raw text\n"
	                          nil "/tmp/nemacs-buffer-store/main" nil 'silent)
	            (write-region "/tmp/nemacs-main-raw-file.txt"
	                          nil "/tmp/nemacs-buffer-file-store/main" nil 'silent)
	            (write-region "5" nil "/tmp/nemacs-buffer-point-store/main" nil 'silent)
	            (write-region "2" nil "/tmp/nemacs-buffer-mark-store/main" nil 'silent)
	            (write-region "1" nil "/tmp/nemacs-buffer-window-start-store/main" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-buffer-read-only-store/main" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "C-x 4 0" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "horizontal" nil "/tmp/nemacs-window-layout" nil 'silent)
	            (write-region "1" nil "/tmp/nemacs-window-selected" nil 'silent)
	            (write-region "9" nil "/tmp/nemacs-point" nil 'silent)
	            (write-region "8" nil "/tmp/nemacs-mark" nil 'silent)
	            (write-region "4" nil "/tmp/nemacs-window-start" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "main raw text\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (should (equal "/tmp/nemacs-main-raw-file.txt"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-file")))
	            (should (equal "main"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-name")))
	            (should (equal ""
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-store/other")))
	            (should (equal "single"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-layout")))
	            (should (equal "0"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-selected")))
	            (should (= 5 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 2 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (should (equal "00001"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-start")))
	            (write-region "kill-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-file")))
            (should (equal "main"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (should (equal "00000"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "switch-to-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "tail   \n\tmid  \nclean\n"
                          nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "delete-trailing-whitespace"
                          nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "tail\n\tmid\nclean\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "next-line" 4)
            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "forward-char" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "abcdef\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "backward-char" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "beginning-of-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "end-of-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 7 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "aa\nbbb\nc\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "beginning-of-line" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "  alpha\n\tbeta\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "12" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "back-to-indentation" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 9 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "M-m" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "12" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 9 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "aa\nbbb\nc\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "end-of-line" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 6 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "move-beginning-of-line" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "move-end-of-line" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 6 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "one\ntwo\nthree\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "next-line" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "next-line" 5)
	            (write-region "previous-line" nil "/tmp/nemacs-cmd" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (write-region "abc\ndefghij\nxy\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "set-goal-column" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-goal-column" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "2"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-goal-column")))
	            (write-region "next-line" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (= 6 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (write-region "previous-line" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "12" nil "/tmp/nemacs-point" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (= 6 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (write-region "set-goal-column" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "4" nil "/tmp/nemacs-prefix-arg" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal ""
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-goal-column")))
	            (write-region "00\n01\n02\n03\n04\n05\n06\n07\n08\n09\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n"
	                          nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "scroll-up-command" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "scroll-up-command" 61)
            (should (equal "00033"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x <" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-hscroll" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "00008"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-hscroll")))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "scroll-right" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "00000"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-hscroll")))
            (write-region "00\n01\n02\n03\n04\n05\n06\n07\n08\n09\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n"
                          nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "recenter-top-bottom" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "61" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "recenter-top-bottom" 61)
            (should (equal "00033"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "move-to-window-line-top-bottom" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (write-region "" nil "/tmp/nemacs-last-command" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "move-to-window-line-top-bottom" 30)
            (should (equal "00000"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "move-to-window-line-top-bottom repeat" 0)
            (should (equal "00000"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "M-r" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (write-region "" nil "/tmp/nemacs-last-command" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "M-r" 30)
            (should (equal "00000"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "abcdef" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "forward-char" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "" nil "/tmp/nemacs-last-command" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "forward-char before repeat" 2)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x z" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "C-x z repeat" 3)
            (should (equal "forward-char"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-last-command")))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "00\n01\n02\n03\n04\n05\n06\n07\n08\n09\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n"
                          nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "reposition-window" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "61" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "reposition-window" 61)
            (should (equal "00033"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "recenter-other-window" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "recenter-other-window" 61)
            (should (equal "00033"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "scroll-down-command" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "scroll-down-command" 1)
            (should (equal "00000"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "scroll-other-window" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "scroll-other-window" 1)
            (should (equal "00033"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "scroll-other-window-down" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "scroll-other-window-down" 1)
            (should (equal "00000"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-M-v" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "C-M-v" 1)
            (should (equal "00033"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "C-M-S-v" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "C-M-S-v" 1)
            (should (equal "00000"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "C-M-l" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "61" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "C-M-l" 61)
            (should (equal "00033"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "C-M-S-l" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "C-M-S-l" 61)
            (should (equal "00033"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "keyboard-quit" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "keyboard-quit" 5)
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "keyboard-escape-quit" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "keyboard-escape-quit" 6)
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-prefix-arg")))
            (write-region "exit-recursive-edit" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "9" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (write-region "8" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "exit-recursive-edit" 8)
            (should (= 4 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-prefix-arg")))
            (write-region "abort-recursive-edit" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "9" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (write-region "8" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "abort-recursive-edit" 8)
            (should (= 4 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-prefix-arg")))
            (write-region "alpha beta alpha\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "isearch-forward" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "beta" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "isearch-forward" 10)
            (should (equal "00000"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-start")))
            (write-region "alpha" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "isearch-forward-next" 16)
            (write-region "missing" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "isearch-forward-missing" 16)
            (write-region "isearch-backward" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "beta" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "16" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "isearch-backward" 6)
            (write-region "alpha" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "isearch-backward-previous" 0)
		            (write-region "missing" nil "/tmp/nemacs-arg" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
		            (nemacs-gui-file-bridge-runtime-test--should-point
		             "isearch-backward-missing" 0)
		            (write-region "abc 123 def 45\n" nil "/tmp/nemacs-buf" nil 'silent)
		            (write-region "isearch-forward-regexp" nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region "[0-9]+" nil "/tmp/nemacs-arg" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
		            (nemacs-gui-file-bridge-runtime-test--should-point
		             "isearch-forward-regexp" 7)
		            (write-region "isearch-backward-regexp" nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region "[0-9]+" nil "/tmp/nemacs-arg" nil 'silent)
		            (write-region "15" nil "/tmp/nemacs-point" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
		            (nemacs-gui-file-bridge-runtime-test--should-point
		             "isearch-backward-regexp" 12)
		            (write-region "missing" nil "/tmp/nemacs-arg" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
		            (nemacs-gui-file-bridge-runtime-test--should-point
		             "isearch-backward-regexp-missing" 12)
		            (write-region "abc 123 def 45\n" nil "/tmp/nemacs-buf" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region "C-M-s" nil "/tmp/nemacs-keys" nil 'silent)
		            (write-region "[0-9]+" nil "/tmp/nemacs-arg" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
		            (nemacs-gui-file-bridge-runtime-test--should-point
		             "raw-isearch-forward-regexp" 7)
		            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region "C-M-r" nil "/tmp/nemacs-keys" nil 'silent)
		            (write-region "[0-9]+" nil "/tmp/nemacs-arg" nil 'silent)
		            (write-region "15" nil "/tmp/nemacs-point" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
		            (nemacs-gui-file-bridge-runtime-test--should-point
		             "raw-isearch-backward-regexp" 12)
                (write-region "alpha beta alpha beta\n" nil "/tmp/nemacs-buf" nil 'silent)
                (write-region "isearch-forward-symbol-at-point" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
                (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
                (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "isearch-forward-symbol-at-point-inside" 5)
                (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "isearch-forward-symbol-at-point-after" 5)
                (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "isearch-forward-symbol-at-point-before-next" 10)
                (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "M-s ." nil "/tmp/nemacs-keys" nil 'silent)
                (write-region "13" nil "/tmp/nemacs-point" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "raw-isearch-forward-symbol-at-point" 16)
                (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region "gamma delta gamma delta\n" nil "/tmp/nemacs-buf" nil 'silent)
                (write-region "isearch-forward-thing-at-point" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
                (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
                (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "isearch-forward-thing-at-point-inside" 5)
                (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "isearch-forward-thing-at-point-before-next" 11)
                (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "M-s M-." nil "/tmp/nemacs-keys" nil 'silent)
                (write-region "14" nil "/tmp/nemacs-point" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "raw-isearch-forward-thing-at-point" 17)
                (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region "xxfoo foo-bar foo_bar foobar\n" nil "/tmp/nemacs-buf" nil 'silent)
                (write-region "isearch-forward-symbol" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "foo" nil "/tmp/nemacs-arg" nil 'silent)
                (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
                (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "isearch-forward-symbol" 9)
                (write-region "foo-bar" nil "/tmp/nemacs-arg" nil 'silent)
                (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "isearch-forward-symbol-hyphen" 13)
                (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "M-s _" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region "foo_bar" nil "/tmp/nemacs-arg" nil 'silent)
                (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "raw-isearch-forward-symbol" 21)
                (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region "xxfoo foo-bar foobar foo bar\n" nil "/tmp/nemacs-buf" nil 'silent)
                (write-region "isearch-forward-word" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "foo" nil "/tmp/nemacs-arg" nil 'silent)
                (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
                (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "isearch-forward-word" 9)
                (write-region "foo bar" nil "/tmp/nemacs-arg" nil 'silent)
                (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "isearch-forward-word-separated" 13)
                (write-region "foobar foo bar\n" nil "/tmp/nemacs-buf" nil 'silent)
                (write-region "foo bar" nil "/tmp/nemacs-arg" nil 'silent)
                (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "isearch-forward-word-requires-separator" 14)
                (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "M-s w" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region "foo bar" nil "/tmp/nemacs-arg" nil 'silent)
                (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "raw-isearch-forward-word" 14)
                (write-region "alpha\nbeta\nalpha beta\n" nil "/tmp/nemacs-buf" nil 'silent)
                (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "M-s o" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region "beta" nil "/tmp/nemacs-arg" nil 'silent)
                (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
                (write-region "1" nil "/tmp/nemacs-mark" nil 'silent)
                (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (should (equal "*Occur*"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-buffer-name")))
                (should (equal "2 matches for \"beta\" in buffer: main\n      2:beta\n      3:alpha beta\n"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-buf")))
                (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
                (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
                (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "M-ESC ESC" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region "4" nil "/tmp/nemacs-prefix-arg" nil 'silent)
                (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
                (write-region "3" nil "/tmp/nemacs-mark" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "raw-keyboard-escape-quit" 7)
                (should (= 3 (nemacs-gui-file-bridge-runtime-test--mark-value)))
                (should (equal ""
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-prefix-arg")))
                (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "C-M-c" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region "4" nil "/tmp/nemacs-prefix-arg" nil 'silent)
                (write-region "8" nil "/tmp/nemacs-point" nil 'silent)
                (write-region "3" nil "/tmp/nemacs-mark" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "raw-exit-recursive-edit" 8)
                (should (= 3 (nemacs-gui-file-bridge-runtime-test--mark-value)))
                (should (equal ""
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-prefix-arg")))
                (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "C-]" nil "/tmp/nemacs-keys" nil 'silent)
                (write-region "4" nil "/tmp/nemacs-prefix-arg" nil 'silent)
                (write-region "8" nil "/tmp/nemacs-point" nil 'silent)
                (write-region "3" nil "/tmp/nemacs-mark" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image "(nemacs-gui-file-bridge-run)")
                (nemacs-gui-file-bridge-runtime-test--should-point
                 "raw-abort-recursive-edit" 8)
                (should (= 3 (nemacs-gui-file-bridge-runtime-test--mark-value)))
                (should (equal ""
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-prefix-arg")))
		            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
		            (write-region "C-M-%" nil "/tmp/nemacs-keys" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
		            (should (equal "1"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            "/tmp/nemacs-minibuffer-active")))
		            (should (equal "Query replace regexp: "
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            "/tmp/nemacs-minibuffer-prompt")))
		            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-minibuffer-state" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-minibuffer-prompt" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
		            (write-region "alpha beta alpha\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "replace-string" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "alpha" nil "/tmp/nemacs-arg" nil 'silent)
	            (write-region "omega" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "omega beta omega\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (nemacs-gui-file-bridge-runtime-test--should-point
	             "replace-string" 16)
	            (write-region "alpha beta alpha\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "beta" nil "/tmp/nemacs-arg" nil 'silent)
	            (write-region "B" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
	            (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
		            (should (equal "alpha B alpha\n"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            "/tmp/nemacs-buf")))
		            (nemacs-gui-file-bridge-runtime-test--should-point
		             "replace-string-from-point" 7)
		            (write-region "abc 123 def 45\n" nil "/tmp/nemacs-buf" nil 'silent)
		            (write-region "replace-regexp" nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region "[0-9]+" nil "/tmp/nemacs-arg" nil 'silent)
		            (write-region "N" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
		            (should (equal "abc N def N\n"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            "/tmp/nemacs-buf")))
		            (nemacs-gui-file-bridge-runtime-test--should-point
		             "replace-regexp" 11)
		            (write-region "xx yy x\n" nil "/tmp/nemacs-buf" nil 'silent)
		            (write-region "x+" nil "/tmp/nemacs-arg" nil 'silent)
		            (write-region "\\&!" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image "(nemacs-gui-file-bridge-run)")
			            (should (equal "xx! yy x!\n"
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            "/tmp/nemacs-buf")))
			            (nemacs-gui-file-bridge-runtime-test--should-point
			             "replace-regexp-whole-match" 9)
			            (write-region "abc 123 def 45\n" nil "/tmp/nemacs-buf" nil 'silent)
			            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
			            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
			            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
			            (nemacs-gui-file-bridge-runtime-test--run-ok
			             reader image
			             "(progn
			                (nl-write-file \"/tmp/nemacs-cmd\" \"query-replace-regexp\")
			                (nl-write-file \"/tmp/nemacs-arg\" \"[0-9]+\")
			                (nl-write-file \"/tmp/nemacs-minibuffer-arg\" \"N\")
			                (nl-write-file \"/tmp/nemacs-point\" \"0\")
			                (nemacs-gui-file-bridge-run)
			                (nl-write-file \"/tmp/nemacs-cmd\" \"\")
			                (nl-write-file \"/tmp/nemacs-keys\" \"n\")
			                (nemacs-gui-file-bridge-run)
			                (nl-write-file \"/tmp/nemacs-keys\" \"y\")
			                (nemacs-gui-file-bridge-run))")
			            (should (equal "abc 123 def N\n"
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            "/tmp/nemacs-buf")))
			            (should (= 13 (nemacs-gui-file-bridge-runtime-test--point-value)))
			            (should (equal "0"
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            "/tmp/nemacs-minibuffer-active")))
			            (write-region "abc 123 def 45\n" nil "/tmp/nemacs-buf" nil 'silent)
			            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
			            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
			            (nemacs-gui-file-bridge-runtime-test--run-ok
			             reader image
			             "(progn
			                (nl-write-file \"/tmp/nemacs-cmd\" \"query-replace-regexp\")
			                (nl-write-file \"/tmp/nemacs-arg\" \"[0-9]+\")
			                (nl-write-file \"/tmp/nemacs-minibuffer-arg\" \"N\")
			                (nl-write-file \"/tmp/nemacs-point\" \"0\")
			                (nemacs-gui-file-bridge-run)
			                (nl-write-file \"/tmp/nemacs-cmd\" \"\")
			                (nl-write-file \"/tmp/nemacs-keys\" \"!\")
			                (nemacs-gui-file-bridge-run))")
			            (should (equal "abc N def N\n"
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            "/tmp/nemacs-buf")))
			            (should (equal "0"
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            "/tmp/nemacs-minibuffer-active")))
				            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
				            (write-region "alpha beta alpha\n" nil "/tmp/nemacs-buf" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image
		             "(progn
		                (nl-write-file \"/tmp/nemacs-cmd\" \"query-replace\")
		                (nl-write-file \"/tmp/nemacs-arg\" \"alpha\")
		                (nl-write-file \"/tmp/nemacs-minibuffer-arg\" \"omega\")
		                (nl-write-file \"/tmp/nemacs-point\" \"0\")
		                (nemacs-gui-file-bridge-run)
		                (nl-write-file \"/tmp/nemacs-cmd\" \"\")
		                (nl-write-file \"/tmp/nemacs-keys\" \"n\")
		                (nemacs-gui-file-bridge-run)
		                (nl-write-file \"/tmp/nemacs-keys\" \"y\")
		                (nemacs-gui-file-bridge-run))")
		            (should (equal "alpha beta omega\n"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            "/tmp/nemacs-buf")))
	            (should (= 16 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (equal "0"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            "/tmp/nemacs-minibuffer-active")))
		            (write-region "alpha beta alpha\n" nil "/tmp/nemacs-buf" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image
		             "(progn
		                (nl-write-file \"/tmp/nemacs-cmd\" \"query-replace\")
		                (nl-write-file \"/tmp/nemacs-arg\" \"alpha\")
		                (nl-write-file \"/tmp/nemacs-minibuffer-arg\" \"omega\")
		                (nl-write-file \"/tmp/nemacs-point\" \"0\")
		                (nemacs-gui-file-bridge-run)
		                (nl-write-file \"/tmp/nemacs-cmd\" \"\")
		                (nl-write-file \"/tmp/nemacs-keys\" \"!\")
		                (nemacs-gui-file-bridge-run))")
	            (should (equal "omega beta omega\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
		            (should (equal "0"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            "/tmp/nemacs-minibuffer-active")))
                (let ((project-dir "/tmp/nemacs-project-query-replace-regexp-test")
                      (current-file "/tmp/nemacs-project-query-replace-regexp-test/sub/current.txt")
                      (target-file "/tmp/nemacs-project-query-replace-regexp-test/sub/nested/target.txt"))
                  (when (file-directory-p project-dir)
                    (delete-directory project-dir t))
                  (make-directory (file-name-directory target-file) t)
                  (write-region "no match here\n" nil current-file nil 'silent)
                  (write-region "alpha 123 beta\n" nil target-file nil 'silent)
                  (write-region "current\n" nil "/tmp/nemacs-buf" nil 'silent)
                  (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
                  (write-region current-file nil "/tmp/nemacs-file" nil 'silent)
                  (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                  (write-region "project-query-replace-regexp" nil "/tmp/nemacs-cmd" nil 'silent)
                  (write-region "[0-9]+" nil "/tmp/nemacs-arg" nil 'silent)
                  (write-region "N" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
                  (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
                  (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
                  (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
                  (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
                  (nemacs-gui-file-bridge-runtime-test--run-ok
                   reader image "(nemacs-gui-file-bridge-run)")
                  (should (equal target-file
                                 (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-file")))
                  (should (equal "1"
                                 (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-minibuffer-active")))
                  (should (equal "Query replacing regexp [0-9]+ with N: "
                                 (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-minibuffer-prompt")))
                  (should (= 6 (nemacs-gui-file-bridge-runtime-test--point-value)))
                  (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                  (write-region "y" nil "/tmp/nemacs-keys" nil 'silent)
                  (nemacs-gui-file-bridge-runtime-test--run-ok
                   reader image "(nemacs-gui-file-bridge-run)")
                  (should (equal "alpha N beta\n"
                                 (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-buf")))
                  (should (equal "0"
                                 (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-minibuffer-active")))
                  (when (file-directory-p project-dir)
                    (delete-directory project-dir t)))
		            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
		            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "split-window-right" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "vertical"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-layout")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-selected")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-split-delta")))
            (write-region "3" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (write-region "enlarge-window-horizontally" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "3"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-split-delta")))
            (write-region "1" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (write-region "enlarge-window-horizontally" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "2"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-split-delta")))
            (write-region "4" nil "/tmp/nemacs-prefix-arg" nil 'silent)
            (write-region "shrink-window-horizontally" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "6"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-split-delta")))
	            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
	            (write-region "other-window" nil "/tmp/nemacs-cmd" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "vertical"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-layout")))
	            (should (equal "1"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-selected")))
            (write-region "other-window" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-selected")))
            (write-region "split-window-below" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "horizontal"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-layout")))
	            (should (equal "0"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-split-delta")))
	            (write-region "2" nil "/tmp/nemacs-prefix-arg" nil 'silent)
	            (write-region "enlarge-window" nil "/tmp/nemacs-cmd" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "2"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-split-delta")))
	            (write-region "1" nil "/tmp/nemacs-window-selected" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
	            (write-region "enlarge-window" nil "/tmp/nemacs-cmd" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "1"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-split-delta")))
	            (write-region "balance-windows" nil "/tmp/nemacs-cmd" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
		            (should (equal "horizontal"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            "/tmp/nemacs-window-layout")))
	              (ert-info ("balance-windows preserves selected second horizontal window")
                  (should (equal "1"
                                 (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-window-selected"))))
	            (should (equal "0"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-split-delta")))
	            (write-region "one\ntwo\nthree\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
	            (write-region "fit-window-to-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (ert-info ("fit-window-to-buffer sizes selected top window from buffer lines")
	              (should (equal "2"
	                             (nemacs-gui-file-bridge-runtime-test--slurp
	                              "/tmp/nemacs-window-split-delta"))))
	            (write-region "1" nil "/tmp/nemacs-window-selected" nil 'silent)
	            (write-region "fit-window-to-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (ert-info ("fit-window-to-buffer sizes selected bottom window with inverse delta")
	              (should (equal "-2"
	                             (nemacs-gui-file-bridge-runtime-test--slurp
	                              "/tmp/nemacs-window-split-delta"))))
	            (write-region "vertical" nil "/tmp/nemacs-window-layout" nil 'silent)
	            (write-region "1" nil "/tmp/nemacs-window-split-delta" nil 'silent)
	            (write-region "fit-window-to-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (ert-info ("fit-window-to-buffer leaves vertical splits unchanged")
	              (should (equal "1"
	                             (nemacs-gui-file-bridge-runtime-test--slurp
	                              "/tmp/nemacs-window-split-delta"))))
	            (write-region "horizontal" nil "/tmp/nemacs-window-layout" nil 'silent)
	            (write-region "bogus-layout" nil "/tmp/nemacs-window-layout" nil 'silent)
	            (write-region "1" nil "/tmp/nemacs-window-selected" nil 'silent)
	            (write-region "shrink-window-if-larger-than-buffer"
	                          nil "/tmp/nemacs-cmd" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "single"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-layout")))
	            (should (equal "0"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-selected")))
	            (write-region "1" nil "/tmp/nemacs-window-selected" nil 'silent)
	            (write-region "delete-window" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "single"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-layout")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-selected")))
            (write-region "horizontal" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "delete-other-windows" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "single"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-layout")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-selected")))
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "split-root-window-right" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (ert-info ("split-root-window-right reuses vertical GUI window transport")
              (should (equal "vertical"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout"))))
            (write-region "split-root-window-below" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (ert-info ("split-root-window-below reuses horizontal GUI window transport")
              (should (equal "horizontal"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout"))))
            (write-region "1" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "7" nil "/tmp/nemacs-window-split-delta" nil 'silent)
            (write-region "delete-windows-on" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (ert-info ("delete-windows-on collapses the current two-window facade")
              (should (equal "single"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "0"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (should (equal "0"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-split-delta"))))
            (write-region "toggle-window-dedicated" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (ert-info ("toggle-window-dedicated writes the dedicated transport flag")
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-dedicated"))))
            (write-region "toggle-window-dedicated" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-dedicated")))
            (write-region "window-toggle-side-windows" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (ert-info ("window-toggle-side-windows writes the side-window visibility flag")
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-side-windows-visible"))))
            (write-region "window-toggle-side-windows" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-side-windows-visible")))
            (write-region "vertical" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "quit-window" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (ert-info ("quit-window removes the selected two-window facade pane")
              (should (equal "single"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "0"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected"))))
            (write-region "vertical" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-window-selected" nil 'silent)
            (write-region "0\t1\t1" nil "/tmp/nemacs-frame-state" nil 'silent)
            (write-region "tear-off-window" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (ert-info ("tear-off-window transfers the selected window facade to a frame")
              (should (equal "single"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "1\t2\t2"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-frame-state"))))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "C-x w 3" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (ert-info ("raw C-x w 3 dispatches through the runtime keymap")
              (should (equal "vertical"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout"))))
            (write-region "one two_three 4\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "forward-word" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "backward-word" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "13" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "backward-word" 4)
            (write-region "(foo (bar baz)) qux\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "forward-sexp" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "forward-sexp list" 15)
            (write-region "backward-sexp" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "15" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "backward-sexp list" 0)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-M-f" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-f" 14)
            (write-region "C-M-b" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "14" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-b" 5)
            (write-region "down-list" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "(foo (bar baz))\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "down-list" 1)
            (write-region "forward-list" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "(foo) (bar)\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "forward-list" 5)
            (write-region "backward-list" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "11" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "backward-list" 6)
            (write-region "backward-up-list" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "(foo (bar baz))\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "backward-up-list" 5)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-M-d" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "(foo (bar baz))\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-d" 1)
            (write-region "C-M-n" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "(foo) (bar)\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-n" 5)
            (write-region "C-M-p" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "11" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-p" 6)
            (write-region "C-M-u" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "(foo (bar baz))\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-u" 5)
            (write-region "beginning-of-defun" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "(defun foo ()\n  (bar))\n\n(defun baz ()\n  (qux))\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "16" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "beginning-of-defun" 0)
            (write-region "end-of-defun" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "end-of-defun" 23)
            (write-region "mark-defun" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "16" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "mark-defun" 0)
            (should (= 23 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "transpose-sexps" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "(foo) (bar) baz\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "(bar) (foo) baz\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "transpose-sexps" 11)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-M-a" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "(defun foo ()\n  (bar))\n\n(defun baz ()\n  (qux))\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "16" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-a" 0)
            (write-region "C-M-e" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-e" 23)
            (write-region "C-M-h" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "16" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-h" 0)
            (should (= 23 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "C-M-t" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "(foo) (bar) baz\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "(bar) (foo) baz\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-t" 11)
            (write-region "insert-parentheses" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "foo bar\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "foo () bar\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "insert-parentheses" 5)
            (write-region "move-past-close-and-reindent" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "(foo bar) baz\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "(foo bar)\nbaz\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "move-past-close-and-reindent" 10)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "M-(" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "foo bar\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "foo () bar\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "M-(" 5)
            (write-region "M-)" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "(foo bar) baz\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "(foo bar)\nbaz\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "M-)" 10)
            (write-region "add-global-abbrev" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "hw" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "hello" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "hw\thello\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-abbrev-table")))
            (write-region "expand-abbrev" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "hw" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "hello"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "expand-abbrev" 5)
            (write-region "add-mode-abbrev" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "mx" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "modeword" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "8" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (string-match-p
                     (regexp-quote "mx\tmodeword\n")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-abbrev-table")))
            (write-region "inverse-add-global-abbrev" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "expanded" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "ix" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (string-match-p
                     (regexp-quote "ix\texpanded\n")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-abbrev-table")))
            (write-region "inverse-add-mode-abbrev" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "mode-expanded" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "im" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (string-match-p
                     (regexp-quote "im\tmode-expanded\n")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-abbrev-table")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "M-'" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "foo bar" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 4 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "expand-jump-to-next-slot" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "a <> b <> c" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "expand-jump-to-next-slot" 3)
            (write-region "expand-jump-to-previous-slot" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "a <> b <> c" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "11" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "expand-jump-to-previous-slot" 8)
            (write-region "dabbrev-expand" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "alpha alphabet al" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "17" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "alpha alphabet alphabet"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "dabbrev-expand" 23)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "M-/" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "alpha alphabet al" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "17" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "alpha alphabet alphabet"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "M-/" 23)
            (write-region "dabbrev-completion" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "alpha alphabet al" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "17" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "alpha alphabet alphabet"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "dabbrev-completion" 23)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-M-/" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "alpha alphabet al" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "17" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "alpha alphabet alphabet"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-/" 23)
            (write-region "complete-symbol" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "alpha alphabet al" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "17" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "alpha alphabet alphabet"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "complete-symbol" 23)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-M-i" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "alpha alphabet al" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "17" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "alpha alphabet alphabet"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-i" 23)
            (write-region "calc-dispatch" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "source\n" nil "/tmp/nemacs-buf" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Calculator*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (string-match-p
                     (regexp-quote "Calculator\n")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (write-region "2C-two-columns" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "left\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Two-Column*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (string-match-p
                     (regexp-quote "Left buffer: main\nRight buffer: main")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (equal "vertical"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-layout")))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-selected")))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-read-only")))
            (write-region "right\n" nil "/tmp/nemacs-buffer-store/other" nil 'silent)
            (write-region "2C-associate-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "other" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "left\n" nil "/tmp/nemacs-buf" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (string-match-p
                     (regexp-quote "Right buffer: other")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (string-match-p
                     (regexp-quote "--- right ---\nright\n")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (write-region "2C-split" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "split\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "single" nil "/tmp/nemacs-window-layout" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-selected" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "vertical"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-window-layout")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x 6" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "command\n" nil "/tmp/nemacs-buf" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Two-Column*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (write-region "count-words-region" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "one two three\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "14" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-modeline" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "Region has 1 lines, 3 words, and 14 characters"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-modeline")))
            (nemacs-gui-file-bridge-runtime-test--should-point "count-words-region" 14)
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "M-=" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "one two three\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "14" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-modeline" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "Region has 1 lines, 3 words, and 14 characters"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-modeline")))
            (nemacs-gui-file-bridge-runtime-test--should-point "M-=" 14)
            (write-region "count-lines-page" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "aaa\n\f\nbbb\nccc\n\f\nddd\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-modeline" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "Page has 3 lines (2 + 2)"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-modeline")))
            (nemacs-gui-file-bridge-runtime-test--should-point "count-lines-page" 7)
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x l" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "aaa\n\f\nbbb\nccc\n\f\nddd\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-modeline" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "Page has 3 lines (2 + 2)"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-modeline")))
            (nemacs-gui-file-bridge-runtime-test--should-point "C-x l" 7)
            (write-region "mark-sexp" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "(foo (bar baz)) qux\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "mark-sexp" 0)
            (should (= 15 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "kill-sexp" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "(foo (bar baz)) qux\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            ;; A preceding project-query-replace-regexp shows *compilation*
            ;; (read-only); declare this scratch buffer writable so the kill
            ;; is not blocked (the front-end re-sends read-only per buffer).
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal " qux\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "(foo (bar baz))"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (nemacs-gui-file-bridge-runtime-test--should-point "kill-sexp" 0)
            (write-region "(foo (bar baz)) qux\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-M-@" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-@" 0)
            (should (= 15 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "C-M-SPC" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-SPC" 5)
            (should (= 14 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "C-M-k" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "(foo (bar baz)) qux\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "(foo ) qux\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "(bar baz)"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (nemacs-gui-file-bridge-runtime-test--should-point "C-M-k" 5)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "one two_three 4\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "kill-word" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "one  4\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "two_three"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (nemacs-gui-file-bridge-runtime-test--should-point "kill-word" 4)
            (write-region "one two three\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "transpose-words" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "two one three\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "transpose-words" 7)
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "one two three\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "M-t" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "two one three\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "M-t" 7)
            (write-region "one\ntwo\nthree\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "transpose-lines" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "two\none\nthree\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "transpose-lines" 8)
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "one\ntwo\nthree\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x C-t" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "two\none\nthree\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point "C-x C-t" 8)
            (write-region "one two\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "mark-word" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "mark-word" 0)
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "one two\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "M-@" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point "M-@" 4)
            (should (= 7 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "one\ntwo\nthree\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "kill-whole-line" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "one\nthree\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "two\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (nemacs-gui-file-bridge-runtime-test--should-point "kill-whole-line" 4)
            (write-region "one two\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "backward-kill-word" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "one \n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "two"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "backward-kill-word" 4)
            (write-region "one two three\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "zap-to-char" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "t" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "wo three\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "one t"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "zap-to-char" 0)
            (write-region "one two three\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (nemacs-gui-file-bridge-runtime-test--raw-key-form
              '("M-z" "t" "RET")))
            (should (equal "wo three\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "one t"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "M-z" 0)
            (write-region "aa\nbb\n\ncc\ndd\n\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "forward-paragraph" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "forward-paragraph" 6)
            (write-region "forward-paragraph" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "forward-paragraph-skip-blank" 13)
            (write-region "backward-paragraph" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "11" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "backward-paragraph" 7)
            (write-region "aa\nbb\n\ncc\ndd\n\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "mark-paragraph" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "11" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 7 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "mark-paragraph" 13)
            (write-region "aaaaaaaaaa bbbbbbbbbb cccccccccc dddddddddd eeeeeeeeee ffffffffff gggggggggg hhhhhhhhhh\n\n"
                          nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "fill-paragraph" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal
                     "aaaaaaaaaa bbbbbbbbbb cccccccccc dddddddddd eeeeeeeeee ffffffffff\ngggggggggg hhhhhhhhhh\n\n"
                     (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "fill-paragraph" 0)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             "(progn
                (nl-write-file \"/tmp/nemacs-buf\" \"alpha beta gamma delta epsilon\")
                (nl-write-file \"/tmp/nemacs-cmd\" \"set-fill-column\")
                (nl-write-file \"/tmp/nemacs-arg\" \"12\")
                (nl-write-file \"/tmp/nemacs-point\" \"0\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-cmd\" \"fill-paragraph\")
                (nl-write-file \"/tmp/nemacs-arg\" \"\")
                (nl-write-file \"/tmp/nemacs-point\" \"0\")
                (nemacs-gui-file-bridge-run))")
            (should (equal "alpha beta\ngamma delta\nepsilon"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             "(progn
                (nl-write-file \"/tmp/nemacs-buf\" \"  alpha\\n\")
                (nl-write-file \"/tmp/nemacs-cmd\" \"set-fill-prefix\")
                (nl-write-file \"/tmp/nemacs-arg\" \"\")
                (nl-write-file \"/tmp/nemacs-point\" \"2\")
                (nemacs-gui-file-bridge-run)
                (if (equal fill-prefix \"  \")
                    (nl-write-file \"/tmp/nemacs-fill-prefix-test\" \"ok\")
                  (nl-write-file \"/tmp/nemacs-fill-prefix-test\" \"bad\")))")
            (should (equal "ok"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-fill-prefix-test")))
            (write-region "One.  Two?  Three!\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "forward-sentence" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "forward-sentence" 4)
            (write-region "One. Two? Three!\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "forward-sentence" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "forward-sentence single-space" 16)
            (write-region "One.  Two?  Three!\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "backward-sentence" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "12" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "backward-sentence" 6)
            (write-region "One.  Two?  Three!\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "kill-sentence" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "One.    Three!\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "Two?"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (should (= 6 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "One.  Two?  Three!\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "backward-kill-sentence" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "10" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "One.    Three!\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "Two?"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (should (= 6 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "ab cd\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "transpose-chars" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "ba cd\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "transpose-chars" 2)
            (write-region "a \t  b\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "delete-horizontal-space" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "ab\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "delete-horizontal-space" 1)
	            (write-region "a \t  b\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "just-one-space" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "a b\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (nemacs-gui-file-bridge-runtime-test--should-point
	             "just-one-space" 2)
	            (write-region "not-modified" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "4" nil "/tmp/nemacs-prefix-arg" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (string-prefix-p
	                     "**"
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-modeline")))
	            (should (equal ""
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-prefix-arg")))
	            (write-region "not-modified" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (string-prefix-p
	                     "--"
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-modeline")))
	            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "M-~" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-prefix-arg" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (string-prefix-p
	                     "--"
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-modeline")))
	            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "a \t  b\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "cycle-spacing" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-last-command" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-cycle-spacing-action" nil 'silent)
	            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "a b\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (nemacs-gui-file-bridge-runtime-test--should-point
	             "cycle-spacing" 2)
	            (write-region "cycle-spacing" nil "/tmp/nemacs-cmd" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "ab\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (nemacs-gui-file-bridge-runtime-test--should-point
	             "cycle-spacing" 1)
	            (write-region "cycle-spacing" nil "/tmp/nemacs-cmd" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "a \t  b\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (nemacs-gui-file-bridge-runtime-test--should-point
	             "cycle-spacing" 3)
	            (write-region "a \t  b\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "M-SPC" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-last-command" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-cycle-spacing-action" nil 'silent)
	            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "a b\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (nemacs-gui-file-bridge-runtime-test--should-point
	             "cycle-spacing" 2)
	            (write-region "M-SPC" nil "/tmp/nemacs-keys" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "ab\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "foo\n  bar\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "delete-indentation" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "foo bar\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "delete-indentation" 3)
            (write-region "foo\n  bar\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "delete-indentation" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "foo\n  bar\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "foo\n\nbar\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "delete-indentation" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "foo\nbar\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "delete-indentation-empty-previous" 4)
            (write-region "alpha\nbeta\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "comment-line" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal ";; alpha\nbeta\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "comment-line" 4)
            (write-region "comment-line" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "alpha\nbeta\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "M-;" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "alpha                           ;\nbeta\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image
	             "(progn
	                (nl-write-file \"/tmp/nemacs-buf\" \"alpha\\n\")
	                (nl-write-file \"/tmp/nemacs-cmd\" \"comment-set-column\")
	                (nl-write-file \"/tmp/nemacs-keys\" \"\")
	                (nl-write-file \"/tmp/nemacs-point\" \"5\")
	                (nemacs-gui-file-bridge-run)
	                (nl-write-file \"/tmp/nemacs-cmd\" \"comment-dwim\")
	                (nl-write-file \"/tmp/nemacs-point\" \"0\")
	                (nemacs-gui-file-bridge-run)
	                (if (if (= comment-column 5)
	                        (equal (rdf \"/tmp/nemacs-buf\") \"alpha ;\\n\")
	                      nil)
	                    (nl-write-file \"/tmp/nemacs-comment-column-test\" \"ok\")
	                  (nl-write-file \"/tmp/nemacs-comment-column-test\" \"bad\")))")
	            (should (equal "ok"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-comment-column-test")))
	            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "one two\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "upcase-word" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "ONE two\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "upcase-word" 3)
            (write-region "ONE TWO\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "downcase-word" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "ONE two\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "downcase-word" 7)
            (write-region "mIXed case\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "capitalize-word" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "Mixed case\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (nemacs-gui-file-bridge-runtime-test--should-point
             "capitalize-word" 5)
            (write-region "abCd EF\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "upcase-region" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "aBCD EF\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 6 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "abCd EF\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "downcase-region" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "abcd eF\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 6 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 1 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "mIXed CASE, next_word\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "capitalize-region" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "21" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "Mixed Case, Next_word\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 21 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "keep\nzeta\nalpha\nmid\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "sort-lines" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "16" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "keep\nalpha\nzeta\nmid\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 5 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 16 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "abcd\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "delete-char" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "acd\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "backward-delete-char" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "cd\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "xy\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "delete-backward-char" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "y\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "ab\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "self-insert-command" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "X" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "aXb\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "undo" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "ab\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
	            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (write-region "undo-redo" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "aXb\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "ab\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "quoted-insert" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "Q" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "aQb\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "quoted-insert" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "\n" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "aQ\nb\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
	            (write-region "abcz\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
	            (write-region "ab\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "C-q" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image
	             "(progn
	                (nemacs-gui-file-bridge-run)
	                (nl-write-file \"/tmp/nemacs-keys\" \"X\")
	                (nemacs-gui-file-bridge-run))")
	            (should (equal "aXb\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
	            (write-region "C-q" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image
	             "(progn
	                (nemacs-gui-file-bridge-run)
	                (nl-write-file \"/tmp/nemacs-keys\" \"RET\")
	                (nemacs-gui-file-bridge-run))")
	            (should (equal "aX\nb\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
	            (write-region "abcz\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "indent-for-tab-command" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "abc     z\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 8 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abcz\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "tab-to-tab-stop" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "abc     z\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 8 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "M-i" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "abcz\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "abc     z\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 8 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "undo" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "abcz\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "indent-region" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "(foo\n(bar)\n(baz))\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "18" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "(foo\n (bar)\n (baz))\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 20 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "indent-rigidly" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "alpha\nbeta\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "11" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal " alpha\n beta\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 13 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-M-\\" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "(foo\n(bar)\n(baz))\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "18" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "(foo\n (bar)\n (baz))\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 20 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "C-x TAB" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "alpha\nbeta\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "11" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal " alpha\n beta\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 13 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "ab" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "newline" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a\nb"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "ab" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "electric-newline-and-maybe-indent"
                          nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a\n        b"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 10 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "ab" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "default-indent-new-line"
                          nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a\n        b"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 10 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "ab" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-j" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a\n        b"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 10 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "ab" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-M-j" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a\n        b"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 10 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "ab" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "M-j" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a\n        b"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 10 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "ab" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "open-line" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "a\nb"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (write-region "foo bar\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "split-line" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "foo \n    bar\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (write-region "foo bar\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "C-M-o" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "foo \n    bar\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
	            (write-region "a\n\n\nb\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "delete-blank-lines" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a\n\nb\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "a\n\n\nb\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "delete-blank-lines" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a\n\nb\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "a\n\nb\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "delete-blank-lines" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a\nb\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "a\n  \n\nb\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "delete-blank-lines" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a\nb\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "a\n\n\nb\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "C-x C-o" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a\n\nb\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "abc\ndef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "kill-line" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "a\ndef\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (equal "bc"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (write-region "yank" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "abc\ndef\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "yank" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "one" nil "/tmp/nemacs-kill" nil 'silent)
            (write-region "3:one3:two" nil "/tmp/nemacs-kill-ring" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-kill-ring-index" nil 'silent)
            (write-region "" nil "/tmp/nemacs-last-command" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "one"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "one"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "yank-pop" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "two"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "two"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill-ring-index")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "M-y" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "one"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "one"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "one two three\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "kill-word" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill-ring" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-kill-ring-index" nil 'silent)
            (write-region "" nil "/tmp/nemacs-last-command" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (write-region "append-next-kill" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "append-next-kill"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-last-command")))
            (write-region "kill-word" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal " three\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "one two"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (should (equal "7:one two"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill-ring")))
            (write-region "one two three\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "backward-kill-word" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill-ring" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-kill-ring-index" nil 'silent)
            (write-region "" nil "/tmp/nemacs-last-command" nil 'silent)
            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-M-w" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "append-next-kill"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-last-command")))
            (write-region "backward-kill-word" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal " three\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "one two"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (should (equal "7:one two"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill-ring")))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "set-mark-command" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-@" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (should (equal "00003"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-global-mark")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x C-SPC" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "3" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 5 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (should (equal "00005"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-global-mark")))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "pop-global-mark" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 5 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 1 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x SPC" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-rectangle-mark-mode" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-rectangle-mark-mode")))
            (write-region "C-x SPC" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-rectangle-mark-mode")))
            (write-region "C-x SPC" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-rectangle-mark-mode")))
            (write-region "C-g" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-rectangle-mark-mode")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x x t" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-truncate-lines" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-truncate-lines")))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "toggle-truncate-lines" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-truncate-lines")))
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "exchange-point-and-mark" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 5 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "mark-whole-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 7 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (write-region "aaa\n\f\nbbb\n\f\nccc\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "forward-page" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (= 11 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (write-region "backward-page" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (= 5 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 0 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (write-region "mark-page" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (= 5 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 11 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "delete-region" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "keep" nil "/tmp/nemacs-kill" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "aef\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "keep"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 1 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "copy-region-as-kill" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "abcdef\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
	            (should (equal "bcd"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-kill")))
	            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 4 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "kill-ring-save" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
	            (write-region "5" nil "/tmp/nemacs-mark" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image "(nemacs-gui-file-bridge-run)")
	            (should (equal "abcdef\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (should (equal "cde"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-kill")))
	            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (= 5 (nemacs-gui-file-bridge-runtime-test--mark-value)))
	            (write-region "kill-region" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "4" nil "/tmp/nemacs-mark" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "aef\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (equal "bcd"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-kill")))
            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (= 1 (nemacs-gui-file-bridge-runtime-test--mark-value)))
            (write-region "a\ndef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "kill-line" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "adef\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (let ((alternate-file
                   (make-temp-file "nemacs-gui-file-bridge-alternate-")))
              (unwind-protect
                  (progn
                    (write-region "alternate file\n" nil alternate-file nil 'silent)
                    (write-region "old buffer\n" nil "/tmp/nemacs-buf" nil 'silent)
                    (write-region "find-alternate-file" nil "/tmp/nemacs-cmd" nil 'silent)
                    (write-region save-file nil "/tmp/nemacs-file" nil 'silent)
                    (write-region alternate-file nil "/tmp/nemacs-arg" nil 'silent)
                    (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (equal "alternate file\n"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-buf")))
                    (should (equal alternate-file
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-file")))
	                    (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value))))
	                (when (file-exists-p alternate-file)
	                  (delete-file alternate-file))))
	            (let ((insert-source
	                   (make-temp-file "nemacs-gui-file-bridge-insert-")))
	              (unwind-protect
	                  (progn
	                    (write-region "INSERTED" nil insert-source nil 'silent)
	                    (write-region "left--right\n" nil "/tmp/nemacs-buf" nil 'silent)
	                    (write-region "insert-file" nil "/tmp/nemacs-cmd" nil 'silent)
	                    (write-region save-file nil "/tmp/nemacs-file" nil 'silent)
	                    (write-region insert-source nil "/tmp/nemacs-arg" nil 'silent)
	                    (write-region "5" nil "/tmp/nemacs-point" nil 'silent)
	                    (write-region "2" nil "/tmp/nemacs-mark" nil 'silent)
	                    (nemacs-gui-file-bridge-runtime-test--run-ok
	                     reader image "(nemacs-gui-file-bridge-run)")
	                    (should (equal "left-INSERTED-right\n"
	                                   (nemacs-gui-file-bridge-runtime-test--slurp
	                                    "/tmp/nemacs-buf")))
	                    (should (= 13 (nemacs-gui-file-bridge-runtime-test--point-value)))
		                    (should (= 2 (nemacs-gui-file-bridge-runtime-test--mark-value))))
		                (when (file-exists-p insert-source)
		                  (delete-file insert-source))))
	            (let ((read-only-file
	                   (make-temp-file "nemacs-gui-file-bridge-read-only-")))
	              (unwind-protect
	                  (progn
	                    (write-region "read only\n" nil read-only-file nil 'silent)
	                    (write-region "find-file-read-only" nil "/tmp/nemacs-cmd" nil 'silent)
	                    (write-region "" nil "/tmp/nemacs-file" nil 'silent)
	                    (write-region read-only-file nil "/tmp/nemacs-arg" nil 'silent)
	                    (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
	                    (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
	                    (nemacs-gui-file-bridge-runtime-test--run-ok
	                     reader image "(nemacs-gui-file-bridge-run)")
	                    (should (equal "read only\n"
	                                   (nemacs-gui-file-bridge-runtime-test--slurp
	                                    "/tmp/nemacs-buf")))
	                    (should (equal "1"
	                                   (nemacs-gui-file-bridge-runtime-test--slurp
	                                    "/tmp/nemacs-read-only")))
	                    (should (= 0 (nemacs-gui-file-bridge-runtime-test--point-value)))
	                    (write-region "self-insert-command" nil "/tmp/nemacs-cmd" nil 'silent)
	                    (write-region "X" nil "/tmp/nemacs-arg" nil 'silent)
	                    (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
	                    (nemacs-gui-file-bridge-runtime-test--run-ok
	                     reader image "(nemacs-gui-file-bridge-run)")
	                    (should (equal "read only\n"
	                                   (nemacs-gui-file-bridge-runtime-test--slurp
	                                    "/tmp/nemacs-buf")))
		                    (should (equal "read-only"
		                                   (nemacs-gui-file-bridge-runtime-test--slurp
		                                    "/tmp/nemacs-status")))
		                    (should (= 4 (nemacs-gui-file-bridge-runtime-test--point-value)))
		                    (when (file-exists-p "/tmp/nemacs-status")
		                      (delete-file "/tmp/nemacs-status"))
		                    (write-region "toggle-read-only" nil "/tmp/nemacs-cmd" nil 'silent)
		                    (nemacs-gui-file-bridge-runtime-test--run-ok
		                     reader image "(nemacs-gui-file-bridge-run)")
		                    (should (equal "0"
		                                   (nemacs-gui-file-bridge-runtime-test--slurp
		                                    "/tmp/nemacs-read-only")))
		                    (write-region "self-insert-command" nil "/tmp/nemacs-cmd" nil 'silent)
		                    (write-region "X" nil "/tmp/nemacs-arg" nil 'silent)
		                    (write-region "4" nil "/tmp/nemacs-point" nil 'silent)
		                    (nemacs-gui-file-bridge-runtime-test--run-ok
		                     reader image "(nemacs-gui-file-bridge-run)")
		                    (should (equal "readX only\n"
		                                   (nemacs-gui-file-bridge-runtime-test--slurp
		                                    "/tmp/nemacs-buf")))
		                    (should (= 5 (nemacs-gui-file-bridge-runtime-test--point-value)))
		                    (write-region "read-only-mode" nil "/tmp/nemacs-cmd" nil 'silent)
		                    (nemacs-gui-file-bridge-runtime-test--run-ok
		                     reader image "(nemacs-gui-file-bridge-run)")
		                    (should (equal "1"
		                                   (nemacs-gui-file-bridge-runtime-test--slurp
		                                    "/tmp/nemacs-read-only"))))
		                (when (file-exists-p read-only-file)
		                  (delete-file read-only-file))))
	            (let ((revert-file (make-temp-file "nemacs-gui-file-bridge-revert-")))
	              (unwind-protect
	                  (progn
	                    (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
	                    (write-region "disk wins\n" nil revert-file nil 'silent)
                    (write-region "dirty buffer\n" nil "/tmp/nemacs-buf" nil 'silent)
                    (write-region "revert-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
                    (write-region revert-file nil "/tmp/nemacs-file" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
                    (write-region "2" nil "/tmp/nemacs-point" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
	                    (should (equal "disk wins\n"
	                                   (nemacs-gui-file-bridge-runtime-test--slurp
	                                    "/tmp/nemacs-buf")))
	                    (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value))))
	                (when (file-exists-p revert-file)
	                  (delete-file revert-file))))
            (let ((revert-file (make-temp-file "nemacs-gui-file-bridge-revert-quick-")))
              (unwind-protect
                  (progn
                    (write-region "quick disk wins\n" nil revert-file nil 'silent)
                    (write-region "dirty quick buffer\n" nil "/tmp/nemacs-buf" nil 'silent)
                    (write-region "revert-buffer-quick" nil "/tmp/nemacs-cmd" nil 'silent)
                    (write-region revert-file nil "/tmp/nemacs-file" nil 'silent)
                    (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
                    (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (equal "quick disk wins\n"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-buf")))
                    (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value))))
                (when (file-exists-p revert-file)
                  (delete-file revert-file))))
            (let ((exit-file (make-temp-file "nemacs-gui-file-bridge-exit-")))
              (unwind-protect
                  (progn
                    (write-region "old disk\n" nil exit-file nil 'silent)
                    (write-region "exit saves\n" nil "/tmp/nemacs-buf" nil 'silent)
                    (write-region "save-buffers-kill-terminal"
                                  nil "/tmp/nemacs-cmd" nil 'silent)
                    (write-region exit-file nil "/tmp/nemacs-file" nil 'silent)
                    (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
                    (write-region "3" nil "/tmp/nemacs-point" nil 'silent)
                    (write-region "1" nil "/tmp/nemacs-mark" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-exit" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (equal "1"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-exit")))
                    (should (equal "exit saves\n"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    exit-file)))
                    (should (equal "main"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-buffer-name")))
                    (should (equal "exit saves\n"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-buffer-store/main")))
                    (should (= 3 (nemacs-gui-file-bridge-runtime-test--point-value)))
                    (should (= 1 (nemacs-gui-file-bridge-runtime-test--mark-value)))
                    (write-region "alias exit saves\n" nil exit-file nil 'silent)
                    (write-region "save-buffers-kill-emacs"
                                  nil "/tmp/nemacs-cmd" nil 'silent)
                    (write-region "kill emacs alias\n" nil "/tmp/nemacs-buf" nil 'silent)
                    (write-region exit-file nil "/tmp/nemacs-file" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-exit" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (equal "1"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-exit")))
                    (should (equal "kill emacs alias\n"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    exit-file)))
                    (write-region "plain kill saves\n" nil exit-file nil 'silent)
                    (write-region "kill-emacs" nil "/tmp/nemacs-cmd" nil 'silent)
                    (write-region "kill emacs command\n" nil "/tmp/nemacs-buf" nil 'silent)
                    (write-region "0" nil "/tmp/nemacs-exit" nil 'silent)
                    (nemacs-gui-file-bridge-runtime-test--run-ok
                     reader image "(nemacs-gui-file-bridge-run)")
                    (should (equal "1"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    "/tmp/nemacs-exit")))
                    (should (equal "kill emacs command\n"
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    exit-file))))
                (when (file-exists-p exit-file)
                  (delete-file exit-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p save-file)
          (delete-file save-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-error-statuses ()
  "Standalone file commands should report UI status for common failures."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (missing-file (make-temp-name "/tmp/nemacs-gui-file-bridge-missing-"))
          (denied-dir (make-temp-file "nemacs-gui-file-bridge-denied-" t))
          denied-file)
      (setq denied-file (expand-file-name "blocked.txt" denied-dir))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "initial\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region missing-file nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "find-file" nil "/tmp/nemacs-cmd" nil 'silent)
            (when (file-exists-p "/tmp/nemacs-status")
              (delete-file "/tmp/nemacs-status"))
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "file-not-found"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-status")))
            (should (equal "initial\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (write-region "" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "save-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (delete-file "/tmp/nemacs-status")
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "error"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-status")))
            (set-file-modes denied-dir #o555)
            (write-region denied-file nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "write-file" nil "/tmp/nemacs-cmd" nil 'silent)
            (delete-file "/tmp/nemacs-status")
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "permission-denied"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-status")))
            (write-region denied-file nil "/tmp/nemacs-file" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "save-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (delete-file "/tmp/nemacs-status")
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "permission-denied"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-status"))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-directory-p denied-dir)
          (set-file-modes denied-dir #o755)
          (delete-directory denied-dir t))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-minibuffer-owned-m-x ()
  "In one standalone NeLisp runtime, M-x should own text before dispatch."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             "(progn
                (nl-write-file \"/tmp/nemacs-keys\" \"M-x\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-keys\" \"f\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-keys\" \"o\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-keys\" \"r\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-keys\" \"w\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-keys\" \"a\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-keys\" \"r\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-keys\" \"d\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-keys\" \"-\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-keys\" \"c\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-keys\" \"h\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-keys\" \"a\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-keys\" \"r\")
                (nemacs-gui-file-bridge-run)
                (nl-write-file \"/tmp/nemacs-keys\" \"RET\")
                (nemacs-gui-file-bridge-run))")
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-active")))
	            (should (equal ""
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-minibuffer-state")))
	            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
              (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image
               (nemacs-gui-file-bridge-runtime-test--raw-key-form
                '("M-X")))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-minibuffer-active")))
              (should (equal "M-X "
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-minibuffer-prompt")))
              (should (string-match-p
                       "forward-char"
                       (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-minibuffer-candidates")))
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image
               (nemacs-gui-file-bridge-runtime-test--raw-key-form
                '("C-g")))
              (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image
               (nemacs-gui-file-bridge-runtime-test--raw-key-form
                '("M-X" "f" "o" "r" "w" "a" "r" "d" "-" "c" "h" "a" "r" "RET")))
              (should (equal "0"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-minibuffer-active")))
              (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
              (should (string-match-p
                       "extended-command-history\tforward-char"
                       (nemacs-gui-file-bridge-runtime-test--slurp
                        "/tmp/nemacs-minibuffer-history"))))
	        (when (file-exists-p image)
	          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-minibuffer-owned-help ()
  "In one standalone NeLisp runtime, C-h f should own text before dispatch."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (nemacs-gui-file-bridge-runtime-test--raw-key-form
              '("C-h f" "f" "o" "r" "w" "a" "r" "d" "-" "c" "h" "a" "r" "RET")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-active")))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-state")))
            (should (equal "*Help*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
	            (should (string-match-p
	                     "forward-char is a function"
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-buf")))
	            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
	            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image
	             (nemacs-gui-file-bridge-runtime-test--raw-key-form
	              '("C-h k" "C" "-" "q" "RET")))
	            (should (equal "*Help*"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-name")))
	            (should (string-match-p
	                     "C-q runs the command quoted-insert"
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-buf"))))
	        (when (file-exists-p image)
	          (delete-file image)))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-minibuffer-completes-m-x ()
  "In one standalone NeLisp runtime, TAB should complete M-x input."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image
	             (nemacs-gui-file-bridge-runtime-test--raw-key-form
	              '("M-x" "f" "o" "r" "TAB" "RET")))
	            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
	            (should (string-match-p
	                     "extended-command-history\tforward-char"
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-minibuffer-history")))
	            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
	            (write-region "" nil "/tmp/nemacs-kill" nil 'silent)
	            (write-region "abc\ndef\n" nil "/tmp/nemacs-buf" nil 'silent)
	            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
	            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image
	             (nemacs-gui-file-bridge-runtime-test--raw-key-form
	              '("M-x" "k" "i" "l" "l" "-" "l" "TAB" "RET")))
	            (should (equal "a\ndef\n"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buf")))
	            (should (equal "bc"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-kill")))
	            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
		            (should (string-match-p
		                     "extended-command-history\tkill-line"
		                     (nemacs-gui-file-bridge-runtime-test--slurp
		                      "/tmp/nemacs-minibuffer-history")))
		            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
		            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
		            (write-region "alpha beta alpha\n" nil "/tmp/nemacs-buf" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
		            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image
		             (nemacs-gui-file-bridge-runtime-test--raw-key-form
		              '("M-x" "r" "e" "p" "l" "a" "c" "e" "-" "s" "t" "r" "i" "n" "g" "RET"
		                "a" "l" "p" "h" "a" "RET"
		                "o" "m" "e" "g" "a" "RET")))
		            (should (equal "omega beta omega\n"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            "/tmp/nemacs-buf")))
		            (should (= 16 (nemacs-gui-file-bridge-runtime-test--point-value)))
		            (should (string-match-p
		                     "extended-command-history\treplace-string"
		                     (nemacs-gui-file-bridge-runtime-test--slurp
		                      "/tmp/nemacs-minibuffer-history")))
		            (should (string-match-p
		                     "replace-string\talpha"
		                     (nemacs-gui-file-bridge-runtime-test--slurp
		                      "/tmp/nemacs-minibuffer-history")))
				            (should (string-match-p
				                     "replace-string-to\tomega"
				                     (nemacs-gui-file-bridge-runtime-test--slurp
				                      "/tmp/nemacs-minibuffer-history")))
				            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
				            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
				            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
				            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
				            (write-region "abc 123 def 45\n" nil "/tmp/nemacs-buf" nil 'silent)
				            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
				            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
				            (nemacs-gui-file-bridge-runtime-test--run-ok
				             reader image
				             (nemacs-gui-file-bridge-runtime-test--raw-key-form
				              '("M-x" "r" "e" "p" "l" "a" "c" "e" "-" "r" "e" "g" "e" "x" "p" "RET"
				                "[" "0" "-" "9" "]" "+" "RET"
				                "N" "RET")))
				            (should (equal "abc N def N\n"
				                           (nemacs-gui-file-bridge-runtime-test--slurp
				                            "/tmp/nemacs-buf")))
				            (should (= 11 (nemacs-gui-file-bridge-runtime-test--point-value)))
				            (should (string-match-p
				                     "extended-command-history\treplace-regexp"
				                     (nemacs-gui-file-bridge-runtime-test--slurp
				                      "/tmp/nemacs-minibuffer-history")))
				            (should (string-match-p
				                     "replace-regexp\t\\[0-9\\]+"
				                     (nemacs-gui-file-bridge-runtime-test--slurp
				                      "/tmp/nemacs-minibuffer-history")))
					            (should (string-match-p
					                     "replace-regexp-to\tN"
					                     (nemacs-gui-file-bridge-runtime-test--slurp
					                      "/tmp/nemacs-minibuffer-history")))
					            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
					            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
					            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
					            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
					            (write-region "abc 123 def 45\n" nil "/tmp/nemacs-buf" nil 'silent)
					            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
					            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
					            (nemacs-gui-file-bridge-runtime-test--run-ok
					             reader image
					             (nemacs-gui-file-bridge-runtime-test--raw-key-form
					              '("M-x" "q" "u" "e" "r" "y" "-" "r" "e" "p" "l" "a" "c" "e" "-"
					                "r" "e" "g" "e" "x" "p" "RET"
					                "[" "0" "-" "9" "]" "+" "RET"
					                "N" "RET"
					                "n" "y")))
					            (should (equal "abc 123 def N\n"
					                           (nemacs-gui-file-bridge-runtime-test--slurp
					                            "/tmp/nemacs-buf")))
					            (should (equal "0"
					                           (nemacs-gui-file-bridge-runtime-test--slurp
					                            "/tmp/nemacs-minibuffer-active")))
					            (should (string-match-p
					                     "extended-command-history\tquery-replace-regexp"
					                     (nemacs-gui-file-bridge-runtime-test--slurp
					                      "/tmp/nemacs-minibuffer-history")))
					            (should (string-match-p
					                     "query-replace-regexp\t\\[0-9\\]+"
					                     (nemacs-gui-file-bridge-runtime-test--slurp
					                      "/tmp/nemacs-minibuffer-history")))
					            (should (string-match-p
					                     "query-replace-regexp-to\tN"
					                     (nemacs-gui-file-bridge-runtime-test--slurp
					                      "/tmp/nemacs-minibuffer-history")))
					            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
					            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
					            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
			            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
			            (write-region "alpha beta alpha\n" nil "/tmp/nemacs-buf" nil 'silent)
			            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
			            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
			            (nemacs-gui-file-bridge-runtime-test--run-ok
			             reader image
			             (nemacs-gui-file-bridge-runtime-test--raw-key-form
			              '("M-x" "q" "u" "e" "r" "y" "-" "r" "e" "p" "l" "a" "c" "e" "RET"
			                "a" "l" "p" "h" "a" "RET"
			                "o" "m" "e" "g" "a" "RET"
			                "n" "y")))
			            (should (equal "alpha beta omega\n"
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            "/tmp/nemacs-buf")))
			            (should (equal "0"
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            "/tmp/nemacs-minibuffer-active")))
			            (should (string-match-p
			                     "extended-command-history\tquery-replace"
			                     (nemacs-gui-file-bridge-runtime-test--slurp
			                      "/tmp/nemacs-minibuffer-history")))
			            (should (string-match-p
			                     "query-replace\talpha"
			                     (nemacs-gui-file-bridge-runtime-test--slurp
			                      "/tmp/nemacs-minibuffer-history")))
			            (should (string-match-p
			                     "query-replace-to\tomega"
			                     (nemacs-gui-file-bridge-runtime-test--slurp
			                      "/tmp/nemacs-minibuffer-history"))))
	        (when (file-exists-p image)
	          (delete-file image)))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-minibuffer-require-match-contract ()
  "In one standalone NeLisp runtime, minibuffer entry kind should be visible."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (find-file-state
           (make-temp-file "nemacs-gui-file-bridge-require-find-"))
          (mx-state
           (make-temp-file "nemacs-gui-file-bridge-require-mx-"))
          (goto-state
           (make-temp-file "nemacs-gui-file-bridge-require-goto-"))
          (buffer-state
           (make-temp-file "nemacs-gui-file-bridge-require-buffer-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "main\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (nl-write-file \"/tmp/nemacs-keys\" \"C-x C-f\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file %S (if emacs-minibuffer-gui-require-match \"1\" \"0\"))
                 (nl-write-file \"/tmp/nemacs-keys\" \"C-g\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file \"/tmp/nemacs-keys\" \"M-x\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file %S (if emacs-minibuffer-gui-require-match \"1\" \"0\"))
                 (nl-write-file \"/tmp/nemacs-keys\" \"C-g\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file \"/tmp/nemacs-keys\" \"M-g g\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file %S (if emacs-minibuffer-gui-require-match \"1\" \"0\"))
                 (nl-write-file \"/tmp/nemacs-keys\" \"C-g\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file \"/tmp/nemacs-keys\" \"C-x b\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file %S (if emacs-minibuffer-gui-require-match \"1\" \"0\")))"
              find-file-state mx-state goto-state buffer-state))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            find-file-state)))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            mx-state)))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            goto-state)))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            buffer-state)))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-require-match"))))
        (when (file-exists-p image)
          (delete-file image))
        (dolist (file (list find-file-state mx-state goto-state buffer-state))
          (when (file-exists-p file)
            (delete-file file)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-minibuffer-standard-entry-args ()
  "In standalone NeLisp, standard minibuffer entry arguments should map to GUI state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (read-state
           (make-temp-file "nemacs-gui-file-bridge-read-args-"))
          (complete-state
           (make-temp-file "nemacs-gui-file-bridge-complete-args-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (read-from-minibuffer \"Arg prompt: \" \"seed\")
                 (files--write-minibuffer-state)
                 (nl-write-file %S
                                (concat files--minibuffer-prompt
                                        \"\\t\"
                                        files--minibuffer-text
                                        \"\\t\"
                                        (if emacs-minibuffer-gui-require-match \"1\" \"0\")))
                 (completing-read \"Pick: \"
                                  (list \"alpha\" (cons \"beta\" \"ignored\") 'bravo \"gamma\")
                                  nil
                                  t
                                  \"b\")
                 (files--write-minibuffer-state)
                 (nl-write-file %S
                                (concat files--minibuffer-prompt
                                        \"\\t\"
                                        files--minibuffer-text
                                        \"\\t\"
                                        files--minibuffer-candidates
                                        \"\\t\"
                                        (if emacs-minibuffer-gui-require-match \"1\" \"0\"))))"
              read-state complete-state))
            (should (equal "Arg prompt: \tseed\t0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            read-state)))
            (should (string-match-p
                     (regexp-quote "Pick: \tb\tbeta\nbravo\n\t1")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      complete-state)))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-require-match"))))
        (when (file-exists-p image)
          (delete-file image))
        (dolist (file (list read-state complete-state))
          (when (file-exists-p file)
            (delete-file file)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-keymap-lookup-contract ()
  "In standalone NeLisp, raw key dispatch should be table-backed."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
	      (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
	          (image (nemacs-gui-file-bridge-runtime-test--write-image))
		          (lookup-state
		           (make-temp-file "nemacs-gui-file-bridge-keymap-lookup-"))
		          (minibuffer-state
		           (make-temp-file "nemacs-gui-file-bridge-keymap-mini-"))
			          (find-other-state
			           (make-temp-file "nemacs-gui-file-bridge-keymap-find-other-"))
			          (find-other-alt-state
			           (make-temp-file "nemacs-gui-file-bridge-keymap-find-other-alt-"))
			          (read-only-other-state
			           (make-temp-file "nemacs-gui-file-bridge-keymap-ro-other-"))
			          (goto-state
	           (make-temp-file "nemacs-gui-file-bridge-keymap-goto-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
	                 (setq files--bridge-keys \"C-f\")
	                 (nl-write-file %S (files--lookup-key-sequence))
	                 (setq files--bridge-keys \"C-w\")
	                 (nl-write-file %S (files--lookup-key-sequence))
		                 (setq files--bridge-keys \"M-w\")
		                 (nl-write-file %S (files--lookup-key-sequence))
		                 (setq files--bridge-keys \"C-q\")
		                 (nl-write-file %S (files--lookup-key-sequence))
		                 (setq files--bridge-keys \"C-x C-u\")
	                 (nl-write-file %S (files--lookup-key-sequence))
	                 (setq files--bridge-keys \"C-x C-l\")
	                 (nl-write-file %S (files--lookup-key-sequence))
		                 (setq files--bridge-keys \"C-x u\")
		                 (nl-write-file %S (files--lookup-key-sequence))
			                 (setq files--bridge-keys \"C-_\")
			                 (nl-write-file %S (files--lookup-key-sequence))
			                 (setq files--bridge-keys \"C-?\")
			                 (nl-write-file %S (files--lookup-key-sequence))
			                 (setq files--bridge-keys \"C-M-_\")
					                 (nl-write-file %S (files--lookup-key-sequence))
					                 (setq files--bridge-keys \"C-x 4 0\")
					                 (nl-write-file %S (files--lookup-key-sequence))
                                     (setq files--bridge-keys \"C-x C-j\")
                                     (nl-write-file %S (files--lookup-key-sequence))
                                     (setq files--bridge-keys \"C-x 4 C-j\")
                                     (nl-write-file %S (files--lookup-key-sequence))
					                 (setq files--bridge-keys \"C-x C-f\")
		                 (files--maybe-start-minibuffer-from-keymap)
	                 (files--write-minibuffer-state)
		                 (nl-write-file %S
		                                (concat files--minibuffer-purpose
		                                        \"\\t\"
		                                        files--minibuffer-prompt
		                                        \"\\t\"
		                                        (if files--minibuffer-active \"1\" \"0\")))
		                 (setq files--minibuffer-active nil)
		                 (setq files--bridge-keys \"C-x 4 C-f\")
		                 (files--maybe-start-minibuffer-from-keymap)
		                 (files--write-minibuffer-state)
		                 (nl-write-file %S
		                                (concat files--minibuffer-purpose
		                                        \"\\t\"
		                                        files--minibuffer-prompt
		                                        \"\\t\"
		                                        (if files--minibuffer-active \"1\" \"0\")))
		                 (setq files--minibuffer-active nil)
		                 (setq files--bridge-keys \"C-x 4 f\")
		                 (files--maybe-start-minibuffer-from-keymap)
		                 (files--write-minibuffer-state)
			                 (nl-write-file %S
			                                (concat files--minibuffer-purpose
			                                        \"\\t\"
			                                        files--minibuffer-prompt
			                                        \"\\t\"
			                                        (if files--minibuffer-active \"1\" \"0\")))
			                 (setq files--minibuffer-active nil)
			                 (setq files--bridge-keys \"C-x 4 r\")
			                 (files--maybe-start-minibuffer-from-keymap)
			                 (files--write-minibuffer-state)
			                 (nl-write-file %S
			                                (concat files--minibuffer-purpose
			                                        \"\\t\"
			                                        files--minibuffer-prompt
			                                        \"\\t\"
			                                        (if files--minibuffer-active \"1\" \"0\")))
			                 (setq files--minibuffer-active nil)
                             (setq files--bridge-keys \"C-x C-d\")
                             (files--maybe-start-minibuffer-from-keymap)
                             (files--write-minibuffer-state)
	                             (nl-write-file %S
	                                            (concat files--minibuffer-purpose
	                                                    \"\\t\"
	                                                    files--minibuffer-prompt
	                                                    \"\\t\"
	                                                    (if files--minibuffer-active \"1\" \"0\")))
	                             (setq files--minibuffer-active nil)
                                 (setq files--bridge-keys \"C-x d\")
                                 (files--maybe-start-minibuffer-from-keymap)
                                 (files--write-minibuffer-state)
                                 (nl-write-file %S
                                                (concat files--minibuffer-purpose
                                                        \"\\t\"
                                                        files--minibuffer-prompt
                                                        \"\\t\"
                                                        (if files--minibuffer-active \"1\" \"0\")))
                                 (setq files--minibuffer-active nil)
	                             (setq files--bridge-keys \"C-x 4 d\")
                             (files--maybe-start-minibuffer-from-keymap)
                             (files--write-minibuffer-state)
                             (nl-write-file %S
                                            (concat files--minibuffer-purpose
                                                    \"\\t\"
                                                    files--minibuffer-prompt
                                                    \"\\t\"
                                                    (if files--minibuffer-active \"1\" \"0\")))
                             (setq files--minibuffer-active nil)
				                 (setq files--bridge-keys \"M-g M-g\")
	                 (files--maybe-start-minibuffer-from-keymap)
	                 (files--write-minibuffer-state)
	                 (nl-write-file %S
	                                (concat files--minibuffer-purpose
	                                        \"\\t\"
	                                        files--minibuffer-prompt
	                                        \"\\t\"
	                                        (if files--minibuffer-active \"1\" \"0\"))))"
		              lookup-state
				              (concat lookup-state ".cw")
				              (concat lookup-state ".mw")
				              (concat lookup-state ".cq")
				              (concat lookup-state ".cxcu")
					              (concat lookup-state ".cxcl")
					              (concat lookup-state ".cxu")
						              (concat lookup-state ".cu")
							              (concat lookup-state ".cquestion")
							              (concat lookup-state ".cmunderscore")
							              (concat lookup-state ".cx40")
                                          (concat lookup-state ".cxcj")
                                          (concat lookup-state ".cx4cj")
					              minibuffer-state
				              find-other-state
				              find-other-alt-state
				              read-only-other-state
		                              (concat lookup-state ".cxcd")
                                      (concat lookup-state ".cxd")
	                                  (concat lookup-state ".cx4d")
						              goto-state))
	            (should (equal "forward-char"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            lookup-state)))
	            (should (equal "kill-region"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            (concat lookup-state ".cw"))))
		            (should (equal "kill-ring-save"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            (concat lookup-state ".mw"))))
		            (should (equal "quoted-insert"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            (concat lookup-state ".cq"))))
		            (should (equal "upcase-region"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            (concat lookup-state ".cxcu"))))
	            (should (equal "downcase-region"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            (concat lookup-state ".cxcl"))))
		            (should (equal "undo"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            (concat lookup-state ".cxu"))))
		            (should (equal "undo"
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            (concat lookup-state ".cu"))))
			            (should (equal "undo-redo"
			                           (nemacs-gui-file-bridge-runtime-test--slurp
			                            (concat lookup-state ".cquestion"))))
				            (should (equal "undo-redo"
				                           (nemacs-gui-file-bridge-runtime-test--slurp
				                            (concat lookup-state ".cmunderscore"))))
					            (should (equal "kill-buffer-and-window"
					                           (nemacs-gui-file-bridge-runtime-test--slurp
					                            (concat lookup-state ".cx40"))))
                                (should (equal "dired-jump"
                                               (nemacs-gui-file-bridge-runtime-test--slurp
                                                (concat lookup-state ".cxcj"))))
                                (should (equal "dired-jump-other-window"
                                               (nemacs-gui-file-bridge-runtime-test--slurp
                                                (concat lookup-state ".cx4cj"))))
			            (should (equal "find-file\tFind file: \t1"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            minibuffer-state)))
			            (should (equal "find-file-other-window\tFind file in other window: \t1"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            find-other-state)))
			            (should (equal "find-file-other-window\tFind file in other window: \t1"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            find-other-alt-state)))
			            (should (equal "find-file-read-only-other-window\tFind file read-only in other window: \t1"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            read-only-other-state)))
                        (let ((list-directory-state
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                (concat lookup-state ".cxcd"))))
		                          (should (equal "list-directory\tList directory: \t1"
		                                         list-directory-state)))
                            (let ((dired-state
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    (concat lookup-state ".cxd"))))
                              (should (equal "dired\tDired directory: \t1"
                                             dired-state)))
	                            (let ((dired-other-state
                                   (nemacs-gui-file-bridge-runtime-test--slurp
                                    (concat lookup-state ".cx4d"))))
                              (should (equal "dired-other-window\tDired directory in other window: \t1"
                                             dired-other-state)))
		            (should (equal "goto-line\tGoto line: \t1"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            goto-state))))
	        (when (file-exists-p image)
	          (delete-file image))
	        (dolist (file (list lookup-state
	                            (concat lookup-state ".cw")
	                            (concat lookup-state ".mw")
	                            (concat lookup-state ".cq")
	                            (concat lookup-state ".cxcu")
	                            (concat lookup-state ".cxcl")
	                            (concat lookup-state ".cxu")
		                            (concat lookup-state ".cu")
		                            (concat lookup-state ".cquestion")
				                            (concat lookup-state ".cmunderscore")
				                            (concat lookup-state ".cx40")
	                                            (concat lookup-state ".cxcj")
	                                            (concat lookup-state ".cx4cj")
	                                            (concat lookup-state ".cxcd")
                                            (concat lookup-state ".cxd")
		                                            (concat lookup-state ".cx4d")
				                            minibuffer-state
				                            find-other-state
				                            find-other-alt-state
				                            read-only-other-state
				                            goto-state))
          (when (file-exists-p file)
            (delete-file file)))))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-keymap-override-dispatch ()
  "In standalone NeLisp, raw key dispatch should obey the keymap table."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             "(progn
                (setq files--keymap-source \"C-f\tend-of-buffer\n\")
                (nl-write-file \"/tmp/nemacs-keys\" \"C-f\")
                (nemacs-gui-file-bridge-run))")
            (nemacs-gui-file-bridge-runtime-test--should-point
             "overridden C-f dispatch"
             7))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-describe-key-candidates-from-keymap ()
  "In standalone NeLisp, C-h k candidates should be derived from keymaps."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (candidate-file (make-temp-file "nemacs-gui-file-bridge-key-candidates-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (nl-write-file \"/tmp/nemacs-keys\" \"C-h k\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file %S files--minibuffer-candidates))"
              candidate-file))
            (let ((candidates
                   (split-string
                    (nemacs-gui-file-bridge-runtime-test--slurp candidate-file)
                    "\n" t)))
              (dolist (candidate '("C-x C-f" "C-q" "M-g M-g"))
                (should (member candidate candidates)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p candidate-file)
          (delete-file candidate-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-minibuffer-owned-switch-buffer ()
  "In one standalone NeLisp runtime, C-x b should own text and history."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (candidate-file (make-temp-file "nemacs-gui-file-bridge-candidates-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "main\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (nl-write-file \"/tmp/nemacs-keys\" \"C-x b\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file %S files--minibuffer-candidates)
                 (nl-write-file \"/tmp/nemacs-keys\" \"o\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file \"/tmp/nemacs-keys\" \"t\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file \"/tmp/nemacs-keys\" \"h\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file \"/tmp/nemacs-keys\" \"e\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file \"/tmp/nemacs-keys\" \"r\")
                 (nemacs-gui-file-bridge-run)
                 (nl-write-file \"/tmp/nemacs-keys\" \"RET\")
                 (nemacs-gui-file-bridge-run))"
              candidate-file))
            (should (equal "other"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (string-match-p
                     "^main$"
                     (nemacs-gui-file-bridge-runtime-test--slurp candidate-file)))
	            (should (string-match-p
	                     "switch-to-buffer\tother"
	                     (nemacs-gui-file-bridge-runtime-test--slurp
	                      "/tmp/nemacs-minibuffer-history")))
	            (nemacs-gui-file-bridge-runtime-test--run-ok
	             reader image
	             "(progn
	                 (nl-write-file \"/tmp/nemacs-window-layout\" \"single\")
	                 (nl-write-file \"/tmp/nemacs-window-selected\" \"0\")
	                 (nl-write-file \"/tmp/nemacs-keys\" \"C-x 4 b\")
	                 (nemacs-gui-file-bridge-run)
	                 (nl-write-file \"/tmp/nemacs-keys\" \"m\")
	                 (nemacs-gui-file-bridge-run)
	                 (nl-write-file \"/tmp/nemacs-keys\" \"a\")
	                 (nemacs-gui-file-bridge-run)
	                 (nl-write-file \"/tmp/nemacs-keys\" \"i\")
	                 (nemacs-gui-file-bridge-run)
	                 (nl-write-file \"/tmp/nemacs-keys\" \"n\")
	                 (nemacs-gui-file-bridge-run)
	                 (nl-write-file \"/tmp/nemacs-keys\" \"RET\")
	                 (nemacs-gui-file-bridge-run))")
	            (should (equal "main"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-buffer-name")))
	            (should (equal "vertical"
	                           (nemacs-gui-file-bridge-runtime-test--slurp
	                            "/tmp/nemacs-window-layout")))
                (ert-info ("balance-windows preserves selected second horizontal window")
		              (should (equal "1"
		                             (nemacs-gui-file-bridge-runtime-test--slurp
		                              "/tmp/nemacs-window-selected"))))
			            (should (string-match-p
			                     "switch-to-buffer-other-window\tmain"
			                     (nemacs-gui-file-bridge-runtime-test--slurp
			                      "/tmp/nemacs-minibuffer-history")))
                (write-region "main\nproj\noutside\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
                (write-region "/tmp/nemacs-project-switch-test/main.txt"
                              nil "/tmp/nemacs-file" nil 'silent)
                (write-region "/tmp/nemacs-project-switch-test/main.txt"
                              nil "/tmp/nemacs-buffer-file-store/main" nil 'silent)
                (write-region "/tmp/nemacs-project-switch-test/proj.txt"
                              nil "/tmp/nemacs-buffer-file-store/proj" nil 'silent)
                (write-region "/tmp/nemacs-outside-switch-test.txt"
                              nil "/tmp/nemacs-buffer-file-store/outside" nil 'silent)
                (nemacs-gui-file-bridge-runtime-test--run-ok
                 reader image
                 (format
                  "(progn
                      (nl-write-file \"/tmp/nemacs-keys\" \"C-x p b\")
                      (nemacs-gui-file-bridge-run)
                      (nl-write-file %S files--minibuffer-candidates)
                      (nl-write-file \"/tmp/nemacs-keys\" \"C-g\")
                      (nemacs-gui-file-bridge-run))"
                  candidate-file))
                (should (string-match-p
                         "^main$"
                         (nemacs-gui-file-bridge-runtime-test--slurp candidate-file)))
                (should (string-match-p
                         "^proj$"
                         (nemacs-gui-file-bridge-runtime-test--slurp candidate-file)))
                (should-not (string-match-p
                             "^outside$"
                             (nemacs-gui-file-bridge-runtime-test--slurp candidate-file)))
	                (nemacs-gui-file-bridge-runtime-test--run-ok
	                 reader image
                 "(progn
                     (nl-write-file \"/tmp/nemacs-window-selected\" \"0\")
                     (nl-write-file \"/tmp/nemacs-keys\" \"C-x 4 C-o\")
                     (nemacs-gui-file-bridge-run)
                     (nl-write-file \"/tmp/nemacs-keys\" \"o\")
                     (nemacs-gui-file-bridge-run)
                     (nl-write-file \"/tmp/nemacs-keys\" \"t\")
                     (nemacs-gui-file-bridge-run)
                     (nl-write-file \"/tmp/nemacs-keys\" \"h\")
                     (nemacs-gui-file-bridge-run)
                     (nl-write-file \"/tmp/nemacs-keys\" \"e\")
                     (nemacs-gui-file-bridge-run)
                     (nl-write-file \"/tmp/nemacs-keys\" \"r\")
                     (nemacs-gui-file-bridge-run)
                     (nl-write-file \"/tmp/nemacs-keys\" \"RET\")
                     (nemacs-gui-file-bridge-run))")
                (should (equal "other"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-buffer-name")))
                (should (equal "vertical"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-window-layout")))
                (should (equal "1"
                               (nemacs-gui-file-bridge-runtime-test--slurp
                                "/tmp/nemacs-window-selected")))
                (should (string-match-p
                         "display-buffer\tother"
                         (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-minibuffer-history")))
		            (nemacs-gui-file-bridge-runtime-test--run-ok
		             reader image
		             "(progn
		                 (nl-write-file \"/tmp/nemacs-keys\" \"C-x x r\")
		                 (nemacs-gui-file-bridge-run)
		                 (nl-write-file \"/tmp/nemacs-keys\" \"w\")
		                 (nemacs-gui-file-bridge-run)
		                 (nl-write-file \"/tmp/nemacs-keys\" \"o\")
		                 (nemacs-gui-file-bridge-run)
		                 (nl-write-file \"/tmp/nemacs-keys\" \"r\")
		                 (nemacs-gui-file-bridge-run)
		                 (nl-write-file \"/tmp/nemacs-keys\" \"k\")
		                 (nemacs-gui-file-bridge-run)
		                 (nl-write-file \"/tmp/nemacs-keys\" \"RET\")
		                 (nemacs-gui-file-bridge-run))")
		            (should (equal "work"
		                           (nemacs-gui-file-bridge-runtime-test--slurp
		                            "/tmp/nemacs-buffer-name")))
		            (should (string-match-p
		                     "rename-buffer\twork"
		                     (nemacs-gui-file-bridge-runtime-test--slurp
		                      "/tmp/nemacs-minibuffer-history"))))
	        (when (file-exists-p image)
          (delete-file image))
	        (when (file-exists-p candidate-file)
	          (delete-file candidate-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-display-prefix ()
  "Display prefix commands should affect the next buffer-displaying command."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (target-file (make-temp-file "nemacs-gui-display-prefix-")))
      (unwind-protect
          (progn
            (write-region "prefix file\n" nil target-file nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--with-transport
              (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
              (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
              (write-region "seed\n" nil "/tmp/nemacs-buf" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
              (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
              (write-region "main\nother\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image
               (format
                "(progn
                   (nl-write-file \"/tmp/nemacs-window-layout\" \"single\")
                   (nl-write-file \"/tmp/nemacs-window-selected\" \"0\")
                   (nl-write-file \"/tmp/nemacs-keys\" \"C-x 4 4\")
                   (nemacs-gui-file-bridge-run)
                   (nl-write-file \"/tmp/nemacs-keys\" \"C-x C-f\")
                   (nl-write-file \"/tmp/nemacs-minibuffer-text\" %S)
                   (nemacs-gui-file-bridge-run))"
                target-file))
              (should (equal "prefix file\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "vertical"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image
               "(progn
                  (nl-write-file \"/tmp/nemacs-window-layout\" \"single\")
                  (nl-write-file \"/tmp/nemacs-window-selected\" \"0\")
                  (nl-write-file \"/tmp/nemacs-keys\" \"C-x 4 1\")
                  (nemacs-gui-file-bridge-run)
                  (nl-write-file \"/tmp/nemacs-keys\" \"C-x 4 b\")
                  (nl-write-file \"/tmp/nemacs-minibuffer-text\" \"main\")
                  (nemacs-gui-file-bridge-run))")
              (should (equal "main"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "single"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "0"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (write-region "0\t1\t1" nil "/tmp/nemacs-tab-state" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image
               (format
                "(progn
                   (nl-write-file \"/tmp/nemacs-window-layout\" \"single\")
                   (nl-write-file \"/tmp/nemacs-window-selected\" \"0\")
                   (nl-write-file \"/tmp/nemacs-keys\" \"C-x t t\")
                   (nemacs-gui-file-bridge-run)
                   (nl-write-file \"/tmp/nemacs-keys\" \"C-x C-f\")
                   (nl-write-file \"/tmp/nemacs-minibuffer-text\" %S)
                   (nemacs-gui-file-bridge-run))"
                target-file))
              (should (equal "prefix file\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
	              (should (equal "1\t2\t2"
	                             (nemacs-gui-file-bridge-runtime-test--slurp
	                              "/tmp/nemacs-tab-state")))
	              (write-region "0\t1\t1" nil "/tmp/nemacs-frame-state" nil 'silent)
	              (nemacs-gui-file-bridge-runtime-test--run-ok
	               reader image
	               (format
	                "(progn
	                   (nl-write-file \"/tmp/nemacs-window-layout\" \"single\")
	                   (nl-write-file \"/tmp/nemacs-window-selected\" \"0\")
	                   (nl-write-file \"/tmp/nemacs-keys\" \"C-x 5 5\")
	                   (nemacs-gui-file-bridge-run)
	                   (nl-write-file \"/tmp/nemacs-keys\" \"C-x C-f\")
	                   (nl-write-file \"/tmp/nemacs-minibuffer-text\" %S)
	                   (nemacs-gui-file-bridge-run))"
	                target-file))
	              (should (equal "prefix file\n"
	                             (nemacs-gui-file-bridge-runtime-test--slurp
	                              "/tmp/nemacs-buf")))
	              (should (equal "single"
	                             (nemacs-gui-file-bridge-runtime-test--slurp
	                              "/tmp/nemacs-window-layout")))
	              (should (equal "0"
	                             (nemacs-gui-file-bridge-runtime-test--slurp
	                              "/tmp/nemacs-window-selected")))
	              (should (equal "1\t2\t2"
	                             (nemacs-gui-file-bridge-runtime-test--slurp
	                              "/tmp/nemacs-frame-state")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p target-file)
          (delete-file target-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-redisplay-state ()
  "In standalone NeLisp, redisplay cursor/modeline state should be returned."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-f" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abc\ndef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (string-match-p
                     "point\t00001"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-cursor")))
            (should (string-match-p
                     "line\t00001"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-cursor")))
            (should (string-match-p
                     "column\t00001"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-cursor")))
            (should (string-match-p
                     "--  main"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-modeline")))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-x x f" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "abc\ndef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (when (file-exists-p "/tmp/nemacs-status")
              (delete-file "/tmp/nemacs-status"))
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "abc\ndef\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should-not (file-exists-p "/tmp/nemacs-status"))
            (should (string-match-p
                     "--  main"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-modeline"))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/source-shape-face-spans-contract ()
  "M12: the bridge source should carry the face-span/fontset decision path."
  (let ((source
         (nemacs-gui-file-bridge-runtime-test--slurp
          nemacs-gui-file-bridge-runtime-test--source)))
    (dolist (needle '("(fset 'files--write-face-spans-state"
                      "(fset 'files--face-keyword-p"
                      "(fset 'files--symbol-char-p"
                      "(fset 'files--face-span-line"
                      "(fset 'files--elisp-buffer-p"
                      "\"nemacs-face-spans\""
                      "\"nemacs-font\""
                      "(files--write-face-spans-state)"))
      (ert-info ((format "face-span contract %s" needle))
        (should (string-match-p (regexp-quote needle) source))))))

(defun nemacs-gui-file-bridge-runtime-test--face-span-forms ()
  "Extract the M12 face-span `fset' forms from the bridge source."
  (let ((forms nil))
    (with-temp-buffer
      (insert-file-contents nemacs-gui-file-bridge-runtime-test--source)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form) (eq (car form) 'fset)
                         (memq (cadr (cadr form))
                               '(files--elisp-buffer-p
                                 files--face-keyword-p
                                 files--symbol-char-p
                                 files--face-span-line
                                 files--write-face-spans-state)))
                (push form forms))))
        (end-of-file nil)))
    (nreverse forms)))

(ert-deftest nemacs-gui-file-bridge-runtime-test/host-face-span-decision-path ()
  "M12 host ERT: face selection + color resolution over a bounded region.
Evaluates only the M12 forms from the bridge source under host Emacs with
the transport write stubbed, then asserts the resolved spans and the
fontset decision for an elisp buffer and a CJK buffer."
  (let ((forms (nemacs-gui-file-bridge-runtime-test--face-span-forms))
        (out (make-hash-table :test 'equal)))
    (should (= 5 (length forms)))
    (cl-letf (((symbol-function 'nl-write-file)
               (lambda (path text) (puthash path text out))))
      (defvar files--current-file-name)
      (defvar files--window-start)
      (defvar files--face-span-cap)
      (defvar files--face-spans)
      (defvar files--face-comment-color)
      (defvar files--face-string-color)
      (defvar files--face-keyword-color)
      (defvar files--font-default-name)
      (defvar files--font-cjk-name)
      (defvar files--font-name)
      (defvar files--font-script)
      (defvar files--face-spans-file)
      (defvar files--font-file)
      (defvar files--buffer-string)
      (setq files--current-file-name "/tmp/nemacs-face-demo.el"
            files--window-start 0
            files--face-span-cap 2048
            files--face-spans ""
            files--face-comment-color "#b22222"
            files--face-string-color "#8b2252"
            files--face-keyword-color "#a020f0"
            files--font-default-name "fixed"
            files--font-cjk-name "-*-fixed-medium-r-normal--14-*-*-*-*-*-iso10646-1"
            files--font-name ""
            files--font-script ""
            files--face-spans-file "spans"
            files--font-file "font"
            files--buffer-string
            "(defun foo ()\n  \"a \\\"str\\\"\" ; comment here\n  (setq x 1))\n")
      (dolist (form forms) (eval form nil))
      (files--write-face-spans-state)
      (let ((spans (gethash "spans" out))
            (font (gethash "font" out)))
        ;; offsets: defun keyword at [1,6), the string literal at
        ;; [16,27), the line comment at [28,42), setq at [46,50).
        (should (string-match-p "^1\t6\tfont-lock-keyword-face\t#a020f0$" spans))
        (should (string-match-p "^16\t27\tfont-lock-string-face\t#8b2252$" spans))
        (should (string-match-p "^28\t42\tfont-lock-comment-face\t#b22222$" spans))
        (should (string-match-p "^46\t50\tfont-lock-keyword-face\t#a020f0$" spans))
        (should (string-match-p "^name\tfixed$" font))
        (should (string-match-p "^script\tlatin$" font)))
      ;; CJK buffer with a non-elisp name: no spans, cjk fontset pick.
      (setq files--current-file-name "/tmp/nemacs-face-demo.txt"
            files--buffer-string "日本語テキスト\n")
      (files--write-face-spans-state)
      (should (equal "" (gethash "spans" out)))
      (should (string-match-p "^script\tcjk$" (gethash "font" out)))
      (should (string-match-p "iso10646" (gethash "font" out))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-face-spans ()
  "M12: the standalone bridge should emit resolved face spans + fontset pick."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            ;; Scenario 1: elisp buffer -> keyword/string/comment spans
            ;; with resolved colors, latin fontset.
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-f" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "/tmp/nemacs-face-demo.el" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "(defun foo ()\n  \"str\" ; note\n  (setq x 1))\n"
                          nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((spans (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-face-spans"))
                  (font (nemacs-gui-file-bridge-runtime-test--slurp
                         "/tmp/nemacs-font")))
              (should (string-match-p "1\t6\tfont-lock-keyword-face\t#a020f0" spans))
              (should (string-match-p "font-lock-string-face\t#8b2252" spans))
              (should (string-match-p "font-lock-comment-face\t#b22222" spans))
              (should (string-match-p "name\tfixed" font))
              (should (string-match-p "script\tlatin" font)))
            ;; Scenario 2: CJK text buffer -> no spans, cjk fontset pick
            ;; (reader strings are raw bytes; the 3-byte UTF-8 lead drives
            ;; the decision).
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-e" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "/tmp/nemacs-face-demo.txt" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "日本語テキスト\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "" (nemacs-gui-file-bridge-runtime-test--slurp
                               "/tmp/nemacs-face-spans")))
            (let ((font (nemacs-gui-file-bridge-runtime-test--slurp
                         "/tmp/nemacs-font")))
              (should (string-match-p "script\tcjk" font))
              (should (string-match-p "iso10646" font))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-raw-key-ignores-stale-command ()
  "In standalone NeLisp, raw key transport should take priority over old commands."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "end-of-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-f" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abcdef\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (= 1 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (string-match-p
                     "point\t00001"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-cursor")))
            (should (string= "" (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-cmd")))
            (should (string= "" (nemacs-gui-file-bridge-runtime-test--slurp
                                  "/tmp/nemacs-keys"))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-elisp-eval-commands ()
  "In standalone NeLisp, GUI bridge should evaluate simple Elisp forms."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "(+ 1 2)\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (nemacs-gui-file-bridge-runtime-test--raw-key-form
              '("C-x C-e")))
            (should (string-match-p
                     "=> 3"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-modeline")))
            (should (= 7 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (nemacs-gui-file-bridge-runtime-test--raw-key-form
              '("M-:" "(" "+" " " "2" " " "3" ")" "RET")))
            (should (string-match-p
                     "=> 5"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-modeline")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-active"))))
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "eval-expression\t(+ 2 3)\nread-expression-history\t(+ 2 3)\n"
                          nil "/tmp/nemacs-minibuffer-history" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (nemacs-gui-file-bridge-runtime-test--raw-key-form
              '("C-x ESC ESC")))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-active")))
            (should (equal "eval-expression"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-purpose")))
            (should (equal "(+ 2 3)"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-state")))
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (nemacs-gui-file-bridge-runtime-test--raw-key-form
              '("RET")))
            (should (string-match-p
                     "=> 5"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-modeline")))
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "eval-expression\t(+ 2 3)\nread-expression-history\t(+ 2 3)\n"
                          nil "/tmp/nemacs-minibuffer-history" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (nemacs-gui-file-bridge-runtime-test--raw-key-form
              '("C-x M-:")))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-active")))
            (should (equal "(+ 2 3)"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-minibuffer-state")))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-insert-char ()
  "In standalone NeLisp, C-x 8 RET should insert a hex codepoint."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "xy\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (nemacs-gui-file-bridge-runtime-test--raw-key-form
              '("C-x 8 RET" "4" "1" "RET")))
            (should (equal "xAy\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 2 (nemacs-gui-file-bridge-runtime-test--point-value)))
            (should (string-match-p
                     "insert-char\t41"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-minibuffer-history"))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-modeline-readonly-modified-prefix ()
  "Mode-line prefix should distinguish clean/read-only/modified/both states."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (plain (make-temp-file "nemacs-gui-modeline-plain-"))
          (ro (make-temp-file "nemacs-gui-modeline-ro-"))
          (mod (make-temp-file "nemacs-gui-modeline-mod-"))
          (romod (make-temp-file "nemacs-gui-modeline-romod-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq files--buffer-name \"main\")
                 (setq files--buffer-string \"x\")
                 (setq files--point 0)
                 (setq files--modeline-override \"\")
                 (setq files--current-file-name nil)
                 (setq files--buffer-read-only-p nil)
                 (setq files--buffer-modified-p nil)
                 (files--write-redisplay-state)
                 (nl-write-file %S files--modeline-string)
                 (setq files--buffer-read-only-p t)
                 (setq files--buffer-modified-p nil)
                 (files--write-redisplay-state)
                 (nl-write-file %S files--modeline-string)
                 (setq files--buffer-read-only-p nil)
                 (setq files--buffer-modified-p t)
                 (files--write-redisplay-state)
                 (nl-write-file %S files--modeline-string)
                 (setq files--buffer-read-only-p t)
                 (setq files--buffer-modified-p t)
                 (files--write-redisplay-state)
                 (nl-write-file %S files--modeline-string))"
              plain ro mod romod))
            (should (string-prefix-p
                     "--" (nemacs-gui-file-bridge-runtime-test--slurp plain)))
            (should (string-prefix-p
                     "%%" (nemacs-gui-file-bridge-runtime-test--slurp ro)))
            (should (string-prefix-p
                     "**" (nemacs-gui-file-bridge-runtime-test--slurp mod)))
            (should (string-prefix-p
                     "%*" (nemacs-gui-file-bridge-runtime-test--slurp romod))))
        (dolist (f (list image plain ro mod romod))
          (when (and f (file-exists-p f)) (delete-file f)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-modified-survives-buffer-switch ()
  "A dirty buffer should stay dirty after switching away and back."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (other-flag (make-temp-file "nemacs-gui-switch-other-"))
          (main-flag (make-temp-file "nemacs-gui-switch-main-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq files--buffer-name \"main\")
                 (setq files--buffer-string \"main body\")
                 (setq files--current-file-name nil)
                 (setq files--point 0)
                 (setq files--mark 0)
                 (setq files--window-start 0)
                 (setq files--buffer-read-only-p nil)
                 (setq files--buffer-modified-p t)
                 (setq files--bridge-arg \"other\")
                 (files--switch-to-buffer)
                 (nl-write-file %S (if files--buffer-modified-p \"1\" \"0\"))
                 (setq files--bridge-arg \"main\")
                 (files--switch-to-buffer)
                 (nl-write-file %S (if files--buffer-modified-p \"1\" \"0\")))"
              other-flag main-flag))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp other-flag)))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp main-flag))))
        (dolist (f (list image other-flag main-flag))
          (when (and f (file-exists-p f)) (delete-file f)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-modeline-save-revert-write-lifecycle ()
  "Dirty edits show **, and save/revert/write-file return the mode-line to --."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (file1 (make-temp-file "nemacs-gui-lifecycle-a-"))
          (file2 (make-temp-file "nemacs-gui-lifecycle-b-"))
          (dirty (make-temp-file "nemacs-gui-lifecycle-dirty-"))
          (saved (make-temp-file "nemacs-gui-lifecycle-saved-"))
          (reverted (make-temp-file "nemacs-gui-lifecycle-reverted-"))
          (written (make-temp-file "nemacs-gui-lifecycle-written-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq files--buffer-name \"main\")
                 (setq files--modeline-override \"\")
                 (setq files--current-file-name %S)
                 (setq files--buffer-string \"\")
                 (setq files--point 0)
                 (setq files--buffer-read-only-p nil)
                 (setq files--buffer-modified-p nil)
                 (insert \"hello\")
                 (files--write-redisplay-state)
                 (nl-write-file %S files--modeline-string)
                 (save-buffer)
                 (files--write-redisplay-state)
                 (nl-write-file %S files--modeline-string)
                 (insert \" more\")
                 (revert-buffer)
                 (files--write-redisplay-state)
                 (nl-write-file %S files--modeline-string)
                 (insert \" x\")
                 (setq files--bridge-arg %S)
                 (write-file)
                 (files--write-redisplay-state)
                 (nl-write-file %S files--modeline-string))"
              file1 dirty saved reverted file2 written))
            (should (string-prefix-p
                     "**" (nemacs-gui-file-bridge-runtime-test--slurp dirty)))
            (should (string-prefix-p
                     "--" (nemacs-gui-file-bridge-runtime-test--slurp saved)))
            (should (string-prefix-p
                     "--" (nemacs-gui-file-bridge-runtime-test--slurp reverted)))
            (should (string-prefix-p
                     "--" (nemacs-gui-file-bridge-runtime-test--slurp written)))
            (should (equal "hello"
                           (nemacs-gui-file-bridge-runtime-test--slurp file1)))
            (should (equal "hello x"
                           (nemacs-gui-file-bridge-runtime-test--slurp file2))))
        (dolist (f (list image file1 file2 dirty saved reverted written))
          (when (and f (file-exists-p f)) (delete-file f)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-find-file-read-only-modeline ()
  "find-file-read-only should load content read-only and show the %% mode-line."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (source (make-temp-file "nemacs-gui-readonly-src-"))
          (ml (make-temp-file "nemacs-gui-readonly-ml-"))
          (flag (make-temp-file "nemacs-gui-readonly-flag-"))
          (buf (make-temp-file "nemacs-gui-readonly-buf-")))
      (unwind-protect
          (progn
            (write-region "locked text\n" nil source nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--with-transport
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image
               (format
                "(progn
                   (setq files--buffer-name \"main\")
                   (setq files--modeline-override \"\")
                   (setq files--display-prefix-action \"\")
                   (setq files--current-file-name nil)
                   (setq files--buffer-string \"\")
                   (setq files--point 0)
                   (setq files--buffer-read-only-p nil)
                   (setq files--buffer-modified-p nil)
                   (setq files--bridge-status \"ok\")
                   (setq files--bridge-arg %S)
                   (find-file-read-only)
                   (files--write-redisplay-state)
                   (nl-write-file %S files--modeline-string)
                   (nl-write-file %S (if files--buffer-read-only-p \"1\" \"0\"))
                   (nl-write-file %S files--buffer-string))"
                source ml flag buf))
              (should (string-prefix-p
                       "%%" (nemacs-gui-file-bridge-runtime-test--slurp ml)))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp flag)))
              (should (equal "locked text\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp buf)))))
        (dolist (f (list image source ml flag buf))
          (when (and f (file-exists-p f)) (delete-file f)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-kmacro-set-insert-counter ()
  "kmacro-set-counter then kmacro-insert-counter should render and auto-increment."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "ab" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            ;; No prefix: set-counter -> 1; each insert-counter renders the
            ;; counter at point and bumps it (1, then 2).
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (nemacs-gui-file-bridge-runtime-test--raw-key-form
              '("C-x C-k C-c" "C-x C-k TAB" "C-x C-k TAB")))
            (should (equal "12ab"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf"))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-kmacro-add-counter ()
  "kmacro-add-counter should add to the counter before insertion."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "ab" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            ;; No prefix: set-counter -> 1; add-counter (+1) -> 2; insert -> "2".
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (nemacs-gui-file-bridge-runtime-test--raw-key-form
              '("C-x C-k C-c" "C-x C-k C-a" "C-x C-k TAB")))
            (should (equal "2ab"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf"))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-utf8-roundtrip ()
  "UTF-8 files should round-trip byte-for-byte through find-file/save-buffer."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (src (make-temp-file "nemacs-m4-utf8-src-"))
          (dst (make-temp-file "nemacs-m4-utf8-dst-")))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'utf-8))
              (write-region "café 日本語 🎌 end\n" nil src nil 'silent))
            (nemacs-gui-file-bridge-runtime-test--with-transport
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image
               (format
                "(progn
                   (setq files--buffer-read-only-p nil)
                   (setq files--bridge-arg %S) (files--find-file-core)
                   (setq files--current-file-name %S) (save-buffer))"
                src dst))
              (should (= 0 (call-process "cmp" nil nil nil "-s" src dst)))))
        (dolist (f (list image src dst))
          (when (and f (file-exists-p f)) (delete-file f)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-binary-roundtrip ()
  "Binary files (incl NUL / high bytes) should round-trip without corruption."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (src (make-temp-file "nemacs-m4-bin-src-"))
          (dst (make-temp-file "nemacs-m4-bin-dst-")))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'binary))
              (write-region (apply #'unibyte-string
                                   (list 0 1 2 127 128 200 255 10 65 66 0 9))
                            nil src nil 'silent))
            (nemacs-gui-file-bridge-runtime-test--with-transport
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image
               (format
                "(progn
                   (setq files--buffer-read-only-p nil)
                   (setq files--bridge-arg %S) (files--find-file-core)
                   (setq files--current-file-name %S) (save-buffer))"
                src dst))
              (should (= 0 (call-process "cmp" nil nil nil "-s" src dst)))))
        (dolist (f (list image src dst))
          (when (and f (file-exists-p f)) (delete-file f)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-coding-input-method-unsupported ()
  "Coding-system and input-method commands should signal unsupported, not no-op."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "x" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (dolist (cmd '("toggle-input-method"
                           "set-buffer-file-coding-system"
                           "universal-coding-system-argument"
                           "set-language-environment"))
              (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
              (write-region cmd nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
              (when (file-exists-p "/tmp/nemacs-status")
                (delete-file "/tmp/nemacs-status"))
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (equal "unsupported"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-status")))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-vc-git-diff-log ()
  "In a Git repo, project-vc-dir / vc-diff / vc-print-log share one root and
report real Git state, diff, and log (M2 Project/Git close-gate)."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (skip-unless (executable-find "git"))
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (repo (make-temp-file "nemacs-m2-git-" t))
          file)
      (unwind-protect
          (progn
            (setq file (expand-file-name "tracked.txt" repo))
            (call-process "git" nil nil nil "-C" repo "init" "-q")
            (call-process "git" nil nil nil "-C" repo "config" "user.email" "t@example.com")
            (call-process "git" nil nil nil "-C" repo "config" "user.name" "Tester")
            (write-region "line one\n" nil file nil 'silent)
            (call-process "git" nil nil nil "-C" repo "add" "tracked.txt")
            (call-process "git" nil nil nil "-C" repo "commit" "-q" "-m" "seed-commit")
            ;; uncommitted modification
            (write-region "line one\nline two\n" nil file nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--with-transport
              ;; project-vc-dir -> status buffer rooted at REPO
              (write-region "" nil "/tmp/nemacs-buf" nil 'silent)
              (write-region file nil "/tmp/nemacs-file" nil 'silent)
              (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
              (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
              (write-region "project-vc-dir" nil "/tmp/nemacs-cmd" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (let ((vc (nemacs-gui-file-bridge-runtime-test--slurp "/tmp/nemacs-buf")))
                (should (string-match-p (concat "VC root: " (regexp-quote repo)) vc))
                (should (string-match-p "tracked.txt" vc)))
              ;; vc-diff -> unified diff containing the new line
              (write-region "" nil "/tmp/nemacs-buf" nil 'silent)
              (write-region file nil "/tmp/nemacs-file" nil 'silent)
              (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
              (write-region "vc-diff" nil "/tmp/nemacs-cmd" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (let ((diff (nemacs-gui-file-bridge-runtime-test--slurp "/tmp/nemacs-buf")))
                (should (string-match-p (concat "VC root: " (regexp-quote repo)) diff))
                (should (string-match-p "diff --git" diff))
                (should (string-match-p "line two" diff)))
              ;; vc-print-log -> contains the seed commit subject
              (write-region "" nil "/tmp/nemacs-buf" nil 'silent)
              (write-region file nil "/tmp/nemacs-file" nil 'silent)
              (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
              (write-region "vc-print-log" nil "/tmp/nemacs-cmd" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)")
              (should (string-match-p "seed-commit"
                                      (nemacs-gui-file-bridge-runtime-test--slurp
                                       "/tmp/nemacs-buf")))))
        (when (file-exists-p image) (delete-file image))
        (when (file-directory-p repo) (delete-directory repo t))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-occur ()
  "occur should list matching lines with line numbers in an *Occur* buffer."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             "(progn
                (setq files--buffer-name \"main\")
                (setq files--buffer-string \"alpha line\\nbeta line\\nalpha again\\ngamma\\n\")
                (setq files--bridge-arg \"alpha\")
                (occur)
                (nl-write-file (progn (setq files--transport-name \"nemacs-buf\") (files--transport-path)) files--buffer-string)
                (nl-write-file (progn (setq files--transport-name \"nemacs-buffer-name\") (files--transport-path)) files--buffer-name))")
            (should (equal "*Occur*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (let ((occ (nemacs-gui-file-bridge-runtime-test--slurp "/tmp/nemacs-buf")))
              (should (string-match-p "2 matches for" occ))
              (should (string-match-p "1:alpha line" occ))
              (should (string-match-p "3:alpha again" occ))
              (should-not (string-match-p "beta line" occ))))
        (when (file-exists-p image)
          (delete-file image))))))

(provide 'nemacs-gui-file-bridge-runtime-test)

;;; nemacs-gui-file-bridge-runtime-test.el ends here
