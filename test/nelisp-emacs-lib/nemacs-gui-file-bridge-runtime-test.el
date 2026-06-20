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

(defconst nemacs-gui-file-bridge-runtime-test--prelude
  (expand-file-name
   "../nelisp/scripts/nelisp-stdlib-prelude.el"
   nemacs-gui-file-bridge-runtime-test--repo-root))

(defconst nemacs-gui-file-bridge-runtime-test--fileio-gui
  (expand-file-name
   "src/emacs-fileio-gui.el"
   nemacs-gui-file-bridge-runtime-test--repo-root))

(defconst nemacs-gui-file-bridge-runtime-test--dired-gui
  (expand-file-name
   "src/emacs-dired-min-gui.el"
   nemacs-gui-file-bridge-runtime-test--repo-root))

(defconst nemacs-gui-file-bridge-runtime-test--info-gui
  (expand-file-name
   "src/emacs-info.el"
   nemacs-gui-file-bridge-runtime-test--repo-root))

(defconst nemacs-gui-file-bridge-runtime-test--help-gui
  (expand-file-name
   "src/emacs-help-gui.el"
   nemacs-gui-file-bridge-runtime-test--repo-root))

(defvar nemacs-gui-file-bridge-runtime-test--profile-enabled
  (getenv "NEMACS_GUI_BRIDGE_PROFILE")
  "Non-nil means emit timing lines for standalone GUI bridge tests.")

(defvar nemacs-gui-file-bridge-runtime-test--profile-run-count 0
  "Counter for profiled `exec-runtime-image' subprocess calls.")

(defun nemacs-gui-file-bridge-runtime-test--profile-log (format-string &rest args)
  "Emit a profile line using FORMAT-STRING and ARGS when profiling is enabled."
  (when nemacs-gui-file-bridge-runtime-test--profile-enabled
    (princ
     (concat "[gui-bridge-profile] "
             (apply #'format format-string args)
             "\n"))))

(defun nemacs-gui-file-bridge-runtime-test--profile-form-summary (form)
  "Return a compact one-line summary of standalone FORM."
  (let ((summary (replace-regexp-in-string "[\n\t ]+" " " form)))
    (if (> (length summary) 96)
        (concat (substring summary 0 96) "...")
      summary)))

(defun nemacs-gui-file-bridge-runtime-test--profile-transport-value (path)
  "Return transport PATH contents for profiling, or an empty string."
  (if (file-exists-p path)
      (condition-case nil
          (nemacs-gui-file-bridge-runtime-test--slurp path)
        (error ""))
    ""))

(defun nemacs-gui-file-bridge-runtime-test--profile-transport-summary ()
  "Return compact command transport state for profile output."
  (mapconcat
   #'identity
   (list
    (format "cmd=%S"
            (nemacs-gui-file-bridge-runtime-test--profile-transport-value
             "/tmp/nemacs-cmd"))
    (format "keys=%S"
            (nemacs-gui-file-bridge-runtime-test--profile-transport-value
             "/tmp/nemacs-keys"))
    (format "arg=%S"
            (nemacs-gui-file-bridge-runtime-test--profile-transport-value
             "/tmp/nemacs-arg"))
    (format "mb-text=%S"
            (nemacs-gui-file-bridge-runtime-test--profile-transport-value
             "/tmp/nemacs-minibuffer-text")))
   " "))

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
  (let ((image (make-temp-file "nemacs-gui-file-bridge-" nil ".nlri"))
        (start (float-time)))
    (with-temp-file image
      (insert ";;; nelisp-runtime-image source-v1\n(progn\n")
      (when (file-readable-p nemacs-gui-file-bridge-runtime-test--prelude)
        (insert-file-contents nemacs-gui-file-bridge-runtime-test--prelude)
        (goto-char (point-max)))
      (insert-file-contents nemacs-gui-file-bridge-runtime-test--fileio-gui)
      (goto-char (point-max))
      (insert-file-contents nemacs-gui-file-bridge-runtime-test--dired-gui)
      (goto-char (point-max))
      (insert-file-contents nemacs-gui-file-bridge-runtime-test--info-gui)
      (goto-char (point-max))
      (insert-file-contents nemacs-gui-file-bridge-runtime-test--help-gui)
      (goto-char (point-max))
      (insert-file-contents nemacs-gui-file-bridge-runtime-test--source)
      (goto-char (point-max))
      (insert "\n)\n"))
    (nemacs-gui-file-bridge-runtime-test--profile-log
     "image-write seconds=%.3f bytes=%s path=%s"
     (- (float-time) start)
     (file-attribute-size (file-attributes image))
     image)
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
                   "/tmp/nemacs-face-theme"
                   "/tmp/nemacs-view"
                   "/tmp/nemacs-view-point"
                   "/tmp/nemacs-view-start"
                   "/tmp/nemacs-toolbar-click"
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
                           "/tmp/nemacs-ime-pending"
                           "/tmp/nemacs-ime-seg"
                           "/tmp/nemacs-ime-cands"
                           "/tmp/nemacs-ime-idx"
                           "/tmp/nemacs-ime-reading"
                           "/tmp/nemacs-ime-learn"
                           "/tmp/nemacs-ime-okuri"
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

(defvar nemacs-gui-file-bridge-runtime-test--persistent-runner nil
  "Plist describing the active persistent standalone runner, or nil.")

(defvar nemacs-gui-file-bridge-runtime-test--persistent-runner-seq 0
  "Monotonic request id for the persistent standalone runner.")

(defvar nemacs-gui-file-bridge-runtime-test--persistent-runner-poll-interval 0.001
  "Seconds between host-side persistent runner file polls.")

(defvar nemacs-gui-file-bridge-runtime-test--persistent-runner-publish-delay 0.005
  "Seconds to let host-side transport writes settle before publishing a request.")

(defun nemacs-gui-file-bridge-runtime-test--wait-for
    (predicate timeout &optional interval)
  "Poll PREDICATE for up to TIMEOUT seconds; return its last value.
When INTERVAL is nil, poll every 0.1s."
  (let ((deadline (+ (float-time) timeout))
        (sleep-interval (or interval 0.1))
        (value nil))
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (sleep-for sleep-interval))
    value))

(defun nemacs-gui-file-bridge-runtime-test--runner-file (runner key)
  "Return RUNNER file path stored under KEY."
  (plist-get (plist-get runner :files) key))

(defun nemacs-gui-file-bridge-runtime-test--persistent-runner-form (files)
  "Return standalone source for a persistent FORM evaluator using FILES."
  (format
   (concat
    "(progn\n"
    "  (setq nemacs-test-runner-last \"\")\n"
    "  (setq nemacs-test-runner-stop nil)\n"
    "  (fset 'nemacs-test-runner-idle-sleep\n"
    "        (lambda ()\n"
    "          (if (fboundp 'syscall-direct)\n"
    "              (let ((ts (alloc-bytes 16 8)))\n"
    "                (ptr-write-u64 ts 0 0)\n"
    "                (ptr-write-u64 ts 8 1000000)\n"
    "                (syscall-direct 35 ts 0 0 0 0 0))\n"
    "            nil)))\n"
    "  (fset 'nemacs-test-runner-reset-runtime-state\n"
    "        (lambda ()\n"
    "          (setq files--bridge-session-active nil)\n"
    "          (setq files--bridge-session-initialized nil)\n"
    "          (setq files--bridge-session-stop nil)\n"
    "          (setq files--bridge-session-idle-count 0)\n"
    "          (setq files--bridge-session-request-count 0)\n"
    "          (setq files--bridge-status \"ok\")\n"
    "          (setq files--bridge-writeback-lane \"normal\")\n"
    "          (setq files--bridge-command nil)\n"
    "          (setq files--bridge-effective-command \"\")\n"
    "          (setq files--bridge-target \"\")\n"
    "          (setq files--bridge-arg \"\")\n"
    "          (setq files--bridge-keys \"\")\n"
    "          (setq files--bridge-minibuffer-text \"\")\n"
    "          (setq files--bridge-minibuffer-arg \"\")\n"
    "          (setq files--prefix-arg \"\")\n"
    "          (setq files--query-replace-active nil)\n"
    "          (setq files--query-replace-regexp-p nil)\n"
    "          (setq files--query-replace-from \"\")\n"
    "          (setq files--query-replace-to \"\")\n"
    "          (setq files--minibuffer-active nil)\n"
    "          (setq files--minibuffer-prompt \"\")\n"
    "          (setq files--minibuffer-text \"\")\n"
    "          (setq files--minibuffer-cursor 0)\n"
    "          (setq files--minibuffer-purpose \"\")\n"
    "          (setq files--minibuffer-history \"\")\n"
    "          (setq files--minibuffer-candidates \"\")\n"
    "          nil))\n"
    "  (nl-write-file %S \"1\")\n"
    "  (while (not nemacs-test-runner-stop)\n"
    "    (setq nemacs-test-runner-shutdown (rdf %S))\n"
    "    (if (equal nemacs-test-runner-shutdown \"1\")\n"
    "        (setq nemacs-test-runner-stop t)\n"
    "      nil)\n"
    "    (setq nemacs-test-runner-request (rdf %S))\n"
    "    (if (if (not (equal nemacs-test-runner-request \"\"))\n"
    "            (not (equal nemacs-test-runner-request nemacs-test-runner-last))\n"
    "          nil)\n"
    "        (progn\n"
    "          (setq nemacs-test-runner-last nemacs-test-runner-request)\n"
    "          (setq nemacs-test-runner-form (rdf %S))\n"
    "          (setq nemacs-test-runner-status \"0\")\n"
    "          (setq nemacs-test-runner-error \"\")\n"
    "          (nemacs-test-runner-reset-runtime-state)\n"
    "          (condition-case err\n"
    "              (nelisp--eval-source-string nemacs-test-runner-form)\n"
    "            (error\n"
    "             (setq nemacs-test-runner-status \"1\")\n"
    "             (setq nemacs-test-runner-error (format \"%%S\" err))))\n"
    "          (nl-write-file %S nemacs-test-runner-status)\n"
    "          (nl-write-file %S \"\")\n"
    "          (nl-write-file %S nemacs-test-runner-error)\n"
    "          (nl-write-file %S nemacs-test-runner-request))\n"
    "      (nemacs-test-runner-idle-sleep)))\n"
    "  (nl-write-file %S \"0\"))")
   (plist-get files :ready)
   (plist-get files :shutdown)
   (plist-get files :request)
   (plist-get files :form)
   (plist-get files :status)
   (plist-get files :stdout)
   (plist-get files :stderr)
   (plist-get files :response)
   (plist-get files :ready)))

(defun nemacs-gui-file-bridge-runtime-test--persistent-runner-start (reader image)
  "Start a persistent standalone runner for READER and IMAGE."
  (let* ((dir (make-temp-file "nemacs-gui-file-bridge-runner-" t))
         (files (list :ready (expand-file-name "ready" dir)
                      :shutdown (expand-file-name "shutdown" dir)
                      :request (expand-file-name "request" dir)
                      :response (expand-file-name "response" dir)
                      :form (expand-file-name "form.el" dir)
                      :status (expand-file-name "status" dir)
                      :stdout (expand-file-name "stdout" dir)
                      :stderr (expand-file-name "stderr" dir)))
         (buffer (generate-new-buffer " *nemacs-gui-file-bridge-runner*"))
         (form (nemacs-gui-file-bridge-runtime-test--persistent-runner-form
                files))
         proc runner)
    (dolist (key '(:ready :shutdown :request :response :form :status
                   :stdout :stderr))
      (write-region "" nil (plist-get files key) nil 'silent))
    (setq proc
          (start-process "nemacs-gui-file-bridge-runner" buffer reader
                         "exec-runtime-image" image form))
    (setq runner (list :proc proc :buffer buffer :dir dir :files files))
    (unless (nemacs-gui-file-bridge-runtime-test--wait-for
             (lambda ()
               (and (process-live-p proc)
                    (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            (plist-get files :ready)))))
             60 nemacs-gui-file-bridge-runtime-test--persistent-runner-poll-interval)
      (let ((log (and (buffer-live-p buffer)
                      (with-current-buffer buffer (buffer-string)))))
        (when (process-live-p proc)
          (delete-process proc))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (when (file-directory-p dir)
          (delete-directory dir t))
        (ert-fail (format "persistent runner did not become ready:\n%s"
                          log))))
    runner))

(defun nemacs-gui-file-bridge-runtime-test--persistent-runner-stop (runner)
  "Stop RUNNER and remove its temporary files."
  (let ((proc (plist-get runner :proc))
        (buffer (plist-get runner :buffer))
        (dir (plist-get runner :dir)))
    (when (and runner (process-live-p proc))
      (write-region "1" nil
                    (nemacs-gui-file-bridge-runtime-test--runner-file
                     runner :shutdown)
                    nil 'silent)
      (nemacs-gui-file-bridge-runtime-test--wait-for
       (lambda () (not (process-live-p proc)))
       5 nemacs-gui-file-bridge-runtime-test--persistent-runner-poll-interval)
      (when (process-live-p proc)
        (delete-process proc)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (when (and dir (file-directory-p dir))
      (delete-directory dir t))))

(defmacro nemacs-gui-file-bridge-runtime-test--with-persistent-runner
    (reader image &rest body)
  "Run BODY with `--run-image' requests served by one standalone process."
  (declare (indent 2) (debug t))
  `(let ((nemacs-gui-file-bridge-runtime-test--persistent-runner
          (nemacs-gui-file-bridge-runtime-test--persistent-runner-start
           ,reader ,image)))
     (unwind-protect
         (progn ,@body)
       (nemacs-gui-file-bridge-runtime-test--persistent-runner-stop
        nemacs-gui-file-bridge-runtime-test--persistent-runner))))

(defun nemacs-gui-file-bridge-runtime-test--persistent-runner-run (form)
  "Evaluate FORM through the active persistent standalone runner."
  (catch 'result
    (let* ((runner nemacs-gui-file-bridge-runtime-test--persistent-runner)
           (proc (plist-get runner :proc))
           (run-id (cl-incf nemacs-gui-file-bridge-runtime-test--profile-run-count))
           (seq (number-to-string
                 (cl-incf nemacs-gui-file-bridge-runtime-test--persistent-runner-seq)))
           (start (float-time))
           (transport-summary
            (and nemacs-gui-file-bridge-runtime-test--profile-enabled
                 (nemacs-gui-file-bridge-runtime-test--profile-transport-summary)))
           status stdout stderr)
      (unless (and runner (process-live-p proc))
        (ert-fail "persistent runner is not live"))
      (write-region "" nil
                    (nemacs-gui-file-bridge-runtime-test--runner-file
                     runner :response)
                    nil 'silent)
      (write-region "" nil
                    (nemacs-gui-file-bridge-runtime-test--runner-file
                     runner :status)
                    nil 'silent)
      (write-region "" nil
                    (nemacs-gui-file-bridge-runtime-test--runner-file
                     runner :stdout)
                    nil 'silent)
      (write-region "" nil
                    (nemacs-gui-file-bridge-runtime-test--runner-file
                     runner :stderr)
                    nil 'silent)
      (write-region form nil
                    (nemacs-gui-file-bridge-runtime-test--runner-file
                     runner :form)
                    nil 'silent)
      (sleep-for
       nemacs-gui-file-bridge-runtime-test--persistent-runner-publish-delay)
      (write-region seq nil
                    (nemacs-gui-file-bridge-runtime-test--runner-file
                     runner :request)
                    nil 'silent)
      (unless (nemacs-gui-file-bridge-runtime-test--wait-for
               (lambda ()
                 (equal seq
                        (nemacs-gui-file-bridge-runtime-test--slurp
                         (nemacs-gui-file-bridge-runtime-test--runner-file
                          runner :response))))
               120
               nemacs-gui-file-bridge-runtime-test--persistent-runner-poll-interval)
        (setq stderr
              (if (buffer-live-p (plist-get runner :buffer))
                  (with-current-buffer (plist-get runner :buffer)
                    (buffer-string))
                ""))
        (setq status 124)
        (nemacs-gui-file-bridge-runtime-test--profile-log
         "persistent-runner id=%d seconds=%.3f status=%S timeout=t form=%S transport=%s"
         run-id (- (float-time) start) status
         (nemacs-gui-file-bridge-runtime-test--profile-form-summary form)
         transport-summary)
        (throw 'result (list :status status :stdout "" :stderr stderr)))
      (setq status
            (string-to-number
             (nemacs-gui-file-bridge-runtime-test--slurp
              (nemacs-gui-file-bridge-runtime-test--runner-file
               runner :status))))
      (setq stdout
            (nemacs-gui-file-bridge-runtime-test--slurp
             (nemacs-gui-file-bridge-runtime-test--runner-file
              runner :stdout)))
      (setq stderr
            (nemacs-gui-file-bridge-runtime-test--slurp
             (nemacs-gui-file-bridge-runtime-test--runner-file
              runner :stderr)))
      (nemacs-gui-file-bridge-runtime-test--profile-log
       "persistent-runner id=%d seconds=%.3f status=%S stdout-bytes=%d stderr-bytes=%d form=%S transport=%s"
       run-id
       (- (float-time) start)
       status
       (length (or stdout ""))
       (length (or stderr ""))
       (nemacs-gui-file-bridge-runtime-test--profile-form-summary form)
       transport-summary)
      (list :status status :stdout stdout :stderr stderr))))

(defun nemacs-gui-file-bridge-runtime-test--run-image (reader image form)
  "Run READER against IMAGE with FORM and return captured stdout/stderr/status."
  (if nemacs-gui-file-bridge-runtime-test--persistent-runner
      (nemacs-gui-file-bridge-runtime-test--persistent-runner-run form)
    (let ((stdout-file (make-temp-file "nemacs-gui-file-bridge-stdout-"))
        (stderr-file (make-temp-file "nemacs-gui-file-bridge-stderr-"))
        (start (float-time))
        (run-id (cl-incf nemacs-gui-file-bridge-runtime-test--profile-run-count))
        (transport-summary
         (and nemacs-gui-file-bridge-runtime-test--profile-enabled
              (nemacs-gui-file-bridge-runtime-test--profile-transport-summary)))
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
          (nemacs-gui-file-bridge-runtime-test--profile-log
           "exec-runtime-image id=%d seconds=%.3f status=%S stdout-bytes=%d stderr-bytes=%d form=%S transport=%s"
           run-id
           (- (float-time) start)
           status
           (length (or stdout ""))
           (length (or stderr ""))
           (nemacs-gui-file-bridge-runtime-test--profile-form-summary form)
           transport-summary)
          (list :status status :stdout stdout :stderr stderr))
      (when (file-exists-p stdout-file)
        (delete-file stdout-file))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file))))))

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
	                      "emacs-command-loop-gui-command-registered-p"
                          "emacs-command-loop-gui-command-accepted-p"
	                      "emacs-command-loop-gui-read-only-command-p"
                      "emacs-command-loop-gui-prefix-command-p"
                      "emacs-command-loop-gui-prefix-repeat-command-p"
                      "emacs-command-loop-gui-prefix-inverted-command"
                      "emacs-command-loop-gui-prefix-arg-number"
                      "emacs-command-loop-gui-prefix-digit-key"
                      "emacs-command-loop-gui-universal-argument"
                      "emacs-command-loop-gui-digit-argument"
                      "emacs-command-loop-gui-negative-argument"
                      "emacs-command-loop-gui-execute-with-prefix-arg"
                      "emacs-command-loop-gui-undo-save-command-p"
                      "emacs-command-loop-gui-save-undo-if-needed"
                      "files--command-loop-save-undo-if-needed-current-context"
                      ":save-undo-state"
                      "emacs-command-loop-gui-register-backend"
                      ":current-command"
                      "files--command-loop-backend-current-command"
                      ":current-command 'files--command-loop-backend-current-command"
                      ":current-effective-command"
                      ":current-keys"
                      ":current-prefix-arg"
                      ":current-minibuffer-arg"
                      "files--command-loop-backend-current-minibuffer-arg"
                      "files--command-loop-backend-set-command"
                      "emacs-command-loop-gui-ingest-request-context"
                      ":error-status-p"
                      "files--command-loop-backend-error-status-p"
                      ":clear-command-request"
                      "files--command-loop-backend-clear-command-request"
                      "emacs-command-loop-gui-keymap-command"
                      "emacs-command-loop-gui-lookup-key-sequence-from-sources"
                      "emacs-command-loop-gui-call-interactively-current-context"
                      "emacs-command-loop-gui-command-execute-current-context"
                      "emacs-command-loop-gui-execute-extended-command-current-context"
                      "emacs-command-loop-gui-project-command"
                      "emacs-command-loop-gui-call-interactively-context"
                      "emacs-command-loop-gui-command-execute-context"
                      "emacs-command-loop-gui-dispatch-current-context"
                      "emacs-command-loop-gui-dispatch-context"
                      "emacs-command-loop-gui-dispatch-key-request"
                      "emacs-command-loop-gui-dispatch-key-request-current-context"
                      "emacs-command-loop-gui-after-key-dispatch"
                      "emacs-command-loop-gui-run-request-current-context"
                      "files--command-loop-run-request-current-context"
                      "files--command-loop-ensure-backend"
                      "files--command-loop-set-command-arg"
                      "emacs-command-loop-gui-writeback-command-name"
                      "emacs-command-loop-gui-write-post-command-state"
                      "emacs-command-loop-gui-lane-writeback-spec"
                      "emacs-command-loop-gui-writeback-spec-flag"
                      "emacs-command-loop-gui-write-lane-state"
                      "files--command-loop-writeback-current-lane"
                      ":write-minibuffer-state"
                      "files--command-loop-backend-write-minibuffer-state"
                      ":write-status-state"
                      "files--command-loop-backend-write-status-state"
                      ":write-buffer-state"
                      "files--command-loop-backend-write-buffer-state"
                      ":write-read-only-one-state"
                      "files--command-loop-backend-write-read-only-one-state"
                      ":mark-written-state"
                      "files--command-loop-backend-mark-written-state"
                      ":write-frame-state"
                      "files--command-loop-backend-write-frame-state"
                      ":after-key-dispatch"
                      "files--command-loop-backend-after-key-dispatch"
                      "emacs-command-loop-gui-before-command"
                      "emacs-command-loop-gui-finish-command"
                      "(fset 'emacs-command-loop-gui-call-interactively-context"
                      "emacs-command-loop-gui-self-insert-key-text"
                      "emacs-command-loop-gui-key-dispatch-spec"
                      "emacs-command-loop-gui-minibuffer-active-p"
                      "emacs-command-loop-gui-minibuffer-handle-key"
                      "emacs-command-loop-gui-maybe-start-minibuffer"
                      ":minibuffer-mode-keymap-source"
                      ":minibuffer-keymap-source"
                      "files--command-loop-backend-minibuffer-active-p"
                      "files--command-loop-backend-minibuffer-keymap-source"
                      "files--command-loop-backend-maybe-start-minibuffer"
                      "files--command-loop-backend-lookup-key-sequence"
                      "(fset 'files--command-loop-backend-call-adapted-command"
                      ":call-adapted-command"
                      ":clear-cycle-spacing-state"
                      "emacs-minibuffer-gui-register-backend"
                      "emacs-minibuffer-gui-filtered-candidates-for-purpose"
                      "emacs-minibuffer-gui-start-purpose-read"
                      "emacs-minibuffer-gui-start-from-keymap"
                      "emacs-minibuffer-gui-start-spec-from-keymaps"
                      "emacs-minibuffer-gui-maybe-start-from-keymap"
                      "emacs-minibuffer-gui-maybe-start-from-keymaps"
                      "emacs-minibuffer-gui-start-current-context"
                      "emacs-minibuffer-gui-maybe-start-current-context"
                      "emacs-minibuffer-gui-handle-key"
                      "emacs-minibuffer-gui-handle-key-current-context"
                      "emacs-minibuffer-gui-purpose-uses-read-p"
                      "emacs-minibuffer-gui-keymap-entry"
                      "emacs-minibuffer-gui-extended-command-followup"
                      "emacs-minibuffer-gui-extended-command-commit-spec"
                      "emacs-minibuffer-gui-replace-followup"
                      "emacs-minibuffer-gui-replace-commit-command"
                      "emacs-minibuffer-gui-command-commit-spec"
                      ":finish-read 'files--minibuffer-finish"
                      ":insert-text 'files--minibuffer-gui-backend-insert-text"
                      ":delete-backward-char"
                      "emacs-fileio-gui-register-backend"
                      "emacs-fileio-gui-refresh-context-from-backend"
                      ":current-arg"
                      ":current-status"
                      ":current-read-only-p"
                      ":current-display-action"
                      "emacs-fileio-gui-command-spec"
                      "emacs-fileio-gui-current-context-command"
                      "'find-file \"same\" nil"
                      "'find-alternate-file"
                      "'project-find-file"
                      "'project-or-external-find-file"
                      "'save-buffer"
                      "'save-some-buffers"
                      "'write-file"
                      "'insert-file"
                      "'insert-buffer"
                      "'revert-buffer"
                      "'switch-to-buffer \"same\""
                      "'display-buffer \"other\""
                      "'rename-buffer"
                      "'kill-buffer"
                      "'kill-buffer-and-window"
                      "'list-buffers"
                      "'project-list-buffers"
	                      "'project-kill-buffers"
	                      "emacs-fileio-gui-find-file-core"
		                      "emacs-fileio-gui-save-buffer-core"
		                      "emacs-fileio-gui-switch-to-buffer-command"
		                      "emacs-fileio-gui-kill-buffer-command"
		                      "emacs-fileio-gui-list-buffers-command"
		                      "emacs-fileio-gui-writeback-spec"
	                      "emacs-fileio-gui-writeback-spec-flag"
	                      "emacs-fileio-gui-writeback-state"
	                      ":write-buffer-state"
	                      "files--fileio-backend-write-buffer-state"
	                      ":mark-written-state"
	                      "files--fileio-backend-mark-written-state"
	                      "files--fileio-writeback-current-context"
	                      "files--fileio-core-delegating"
	                      "(fset 'files--find-file-core"
	                      "(fset 'files--save-buffer-core"
	                      "(fset 'files--switch-to-buffer"
                      "(fset 'files--kill-buffer-core"
                      "(fset 'files--list-buffers-core"
                      "(fset 'call-interactively"
	                      "(fset 'command-execute"
	                              "(fset 'execute-extended-command"
			                      "(fset 'execute-extended-command-for-buffer"
                                  "(fset 'call-process"
                                  "nelisp-process-call-process"
		                              "(fset 'shell-command"
                                  "(fset 'shell-command-core"
                                  "emacs-shell-command-gui-register-backend"
                                  "emacs-shell-command-gui-shell-command"
                                  "emacs-shell-command-gui-project-shell-command"
                                  "emacs-shell-command-gui-async-shell-command"
                                  "emacs-shell-command-gui-project-compile"
	                              "(fset 'shell-command-on-region"
	                              "(fset 'async-shell-command"
                                  "(fset 'async-shell-command-core"
                                  "(fset 'project-shell-command"
                                  "(fset 'project-shell-command-core"
                                  "(fset 'project-async-shell-command"
                                  "(fset 'project-async-shell-command-core"
                                  "(fset 'project-shell"
                                  "(fset 'project-eshell"
                                  "(fset 'project-compile"
                                  "(fset 'project-compile-core"
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
                                      "(fset 'files--show-static-help-core"
                                      "(fset 'files--help-sync-context"
                                      "(fset 'files--help-install-backend"
                                      "(fset 'files--help-run-core"
                                      "emacs-help-gui-show-help-buffer"
                                      "emacs-help-gui-register-backend"
                                      "emacs-help-gui-set-context"
                                      "emacs-help-gui-refresh-context-from-backend"
                                      ":current-arg"
                                      ":current-file-name"
                                      ":buffer-read-only-p"
                                      ":keymap-source"
                                      "emacs-help-gui-describe-function-core"
                                      "emacs-help-gui-describe-function"
                                      "emacs-help-gui-current-context-command"
                                      "emacs-help-gui-describe-variable-core"
                                      "emacs-help-gui-describe-variable"
                                      "emacs-help-gui-describe-key-core"
                                      "emacs-help-gui-describe-key"
                                      "emacs-help-gui-describe-key-briefly-core"
                                      "emacs-help-gui-describe-bindings-core"
                                      "emacs-help-gui-describe-bindings"
                                      "emacs-help-gui-help-for-help"
                                      "emacs-help-gui-where-is-core"
                                      "emacs-help-gui-where-is"
                                      "emacs-help-gui-describe-command"
                                      "emacs-help-gui-static-command"
                                      "emacs-help-gui-apropos-command"
                                      "emacs-help-gui-apropos-documentation"
				                      "(fset 'describe-coding-system"
				                      "(fset 'describe-input-method"
				                      "(fset 'describe-language-environment"
				                      "(fset 'apropos-command"
				                      "(fset 'apropos-documentation"
				                      "(fset 'view-echo-area-messages"
				                      "(fset 'scratch-buffer"
				                      "(fset 'messages-buffer"
				                      "(fset 'warnings-buffer"
				                      "(fset 'emacs-special-buffers-ensure-buffer"
				                      "(fset 'emacs-special-buffers-append-to-buffer"
				                      "(fset 'emacs-special-buffers-switch-to-buffer"
				                      "(fset 'emacs-special-buffers-message"
				                      "(fset 'emacs-special-buffers-display-warning"
				                      "(fset 'message"
				                      "(fset 'display-warning"
				                      "(fset 'about-emacs"
                                      "(fset 'files--write-toolbar-state-core"
                                      "(fset 'files--toolbar-menu-for-label-core"
                                      "(fset 'files--handle-toolbar-click"
                                      "emacs-toolbar-gui-register-backend"
                                      "emacs-toolbar-gui-write-state"
                                      "emacs-toolbar-gui-handle-click"
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
                                      "emacs-help-gui-writeback-spec"
                                      "emacs-help-gui-writeback-spec-flag"
                                      "emacs-help-gui-writeback-state"
                                      "files--help-writeback-current-context"
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
                                      "(fset 'files--info-sync-context"
                                      "(fset 'files--info-install-backend"
                                      "(fset 'files--info-run-core"
                                      "emacs-info-gui-set-context"
                                      "emacs-info-gui-refresh-context-from-backend"
                                      "emacs-info-gui-register-backend"
                                      ":current-arg"
                                      ":current-file"
                                      ":current-node"
                                      "emacs-info-gui-info-core"
                                      "emacs-info-gui-info-command"
                                      "emacs-info-gui-current-context-command"
                                      "emacs-info-gui-render-node"
                                      "emacs-info-gui-goto-pointer"
                                      "emacs-info-gui-next-command"
                                      "emacs-info-gui-prev-command"
                                      "emacs-info-gui-up-command"
                                      "emacs-info-gui-emacs-manual-command"
                                      "emacs-info-gui-display-manual-command"
                                      "emacs-info-gui-view-order-manuals-command"
                                      "emacs-info-gui-goto-emacs-command-node-command"
                                      "emacs-info-gui-goto-emacs-key-command-node-command"
                                      "emacs-info-gui-lookup-symbol-command"
                                      "emacs-info-gui-writeback-spec"
                                      "emacs-info-gui-writeback-spec-flag"
                                      "emacs-info-gui-writeback-state"
                                      "files--info-writeback-current-context"
                                      "emacs-info-gui-info"
                                      "emacs-info-gui-next"
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
                                      "(fset 'files--dired-sync-context"
                                      "(fset 'files--dired-install-backend"
                                      "emacs-dired-min-gui-set-context"
                                      "emacs-dired-min-gui-refresh-context-from-backend"
                                      "emacs-dired-min-gui-register-backend"
                                      ":current-directory"
                                      ":current-target"
                                      ":current-status"
                                      ":project-directory"
                                      "emacs-dired-min-gui-dired-command"
                                      "emacs-dired-min-gui-current-context-command"
                                      "'dired \"same\""
                                      "'dired-jump \"same\""
                                      "'project-find-dir \"same\""
                                      "'project-dired \"same\""
                                      "emacs-dired-min-gui-apply-mark-core"
                                      "emacs-dired-min-gui-do-flagged-delete-core"
                                      "emacs-dired-min-gui-do-rename-core"
                                      "emacs-dired-min-gui-do-copy-core"
                                      "emacs-dired-min-gui-dired"
                                      "emacs-dired-min-gui-writeback-spec"
                                      "emacs-dired-min-gui-writeback-spec-flag"
                                      "emacs-dired-min-gui-writeback-state"
                                      ":write-modeline-state"
                                      "files--fileio-backend-write-modeline-state"
                                      "files--dired-writeback-current-context"
                                      "files--dired-core-delegating"
                                      "(fset 'dired"
                                      "(fset 'dired-jump"
                                      "(fset 'dired-jump-other-window"
                                      "(fset 'dired-other-window"
                                      "(fset 'dired-other-frame"
                                              "(fset 'dired-other-tab"
                                      "(fset 'files--dired-backend-directory-buffer-p"
                                      "(fset 'files--dired-backend-name-at-point"
                                      "(fset 'files--dired-backend-marks-text"
                                      "(fset 'files--dired-backend-expand-name"
                                      "(fset 'files--dired-backend-directory-p"
                                      "(fset 'files--dired-backend-delete-file"
                                      "(fset 'files--dired-backend-rename-file"
                                      "(fset 'files--dired-backend-file-exists-p"
                                      "(fset 'files--dired-backend-project-directory"
                                      "(fset 'files--dired-backend-read-file"
                                      "(fset 'files--dired-backend-write-file"
                                      "(fset 'files--dired-backend-remove-mark"
                                      "(fset 'files--dired-backend-set-mark"
                                      "(fset 'files--dired-backend-write-marks-state"
                                      "(fset 'files--dired-backend-rerender"
                                      "(fset 'files--dired-backend-next-line"
                                      "(fset 'files--dired-backend-set-status"
                                      "(fset 'files--dired-backend-set-modeline"
                                      "(fset 'files--dired-do-flagged-delete-core"
                                      "(fset 'files--dired-do-rename-core"
                                      "(fset 'files--dired-do-copy-core"
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
                                              "emacs-command-loop-gui-project-command"
                                              "(fset 'project-other-window-command"
                                              "(fset 'project-other-tab-command"
                                              "(fset 'project-other-frame-command"
                                              "(fset 'project-switch-project"
									                      "(fset 'switch-to-buffer"
                                              "(fset 'switch-to-buffer-other-window"
                                              "(fset 'switch-to-buffer-other-tab"
                                              "(fset 'project-switch-to-buffer"
	                                              "emacs-fileio-gui-switch-to-buffer-command"
	                                              "emacs-fileio-gui-kill-buffer-command"
	                                              "emacs-fileio-gui-list-buffers-command"
	                                              "emacs-fileio-gui-kill-buffer"
                                              "emacs-fileio-gui-list-buffers"
                                              "(fset 'files--fileio-backend-save-some-buffers"
                                              "(fset 'files--fileio-backend-insert-file"
                                              "(fset 'files--fileio-backend-insert-buffer"
                                              "(fset 'files--fileio-backend-revert-buffer"
                                              "(fset 'files--fileio-backend-rename-buffer"
                                              "(fset 'files--fileio-backend-kill-buffer"
                                              "(fset 'files--fileio-backend-list-buffers"
                                              "(fset 'files--revert-buffer-core"
			                                          "(fset 'rename-buffer"
                                              "(fset 'files--rename-buffer-core"
	                                          "(fset 'rename-uniquely"
	                                          "(fset 'clone-buffer"
	                                          "(fset 'clone-indirect-buffer-other-window"
                                              "(fset 'files--kill-buffer-core"
							                      "(fset 'kill-buffer"
					                      "(fset 'kill-buffer-and-window"
                                              "(fset 'project-kill-buffers"
                                              "(fset 'files--list-buffers-core"
						                      "(fset 'list-buffers"
                                              "(fset 'files--project-list-buffers-core"
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
		                      "emacs-command-loop-gui-read-only-command-p"
		                      "(fset 'revert-buffer"
                          "(fset 'revert-buffer-quick"
                      "(command-execute)"
	                      "(fset 'nemacs-gui-file-bridge-run"))
	      (should (string-match-p (regexp-quote needle) source)))
	    (dolist (needle '("emacs-fileio-gui-find-file-command"
	                      "emacs-fileio-gui-find-file-read-only-command"
	                      "emacs-fileio-gui-save-buffer-command"
	                      "emacs-fileio-gui-save-some-buffers-command"
	                      "emacs-fileio-gui-write-file-command"
	                      "emacs-fileio-gui-insert-file-command"
	                      "emacs-fileio-gui-insert-buffer-command"
	                      "emacs-fileio-gui-display-buffer-command"
	                      "emacs-fileio-gui-revert-buffer-command"
	                      "emacs-fileio-gui-rename-buffer-command"
	                      "emacs-fileio-gui-kill-buffer-and-window-command"
	                      "emacs-fileio-gui-project-list-buffers-command"
	                      "emacs-fileio-gui-project-kill-buffers-command"))
	      (should-not (string-match-p (regexp-quote needle) source)))
	    (dolist (needle '("(setq files--transport-dir \"/tmp\")"
                      "(fset 'files--transport-path"
                      "files--transport-name"))
      (should (string-match-p (regexp-quote needle) source)))
    (should (string-match-p "Runtime lambdas intentionally avoid" source))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/generated-image-includes-family-runtimes ()
  "The generated source-v1 bridge image should load family runtimes first."
  (let ((image (nemacs-gui-file-bridge-runtime-test--write-image)))
    (unwind-protect
        (let* ((text (nemacs-gui-file-bridge-runtime-test--slurp image))
               (bridge-pos
                (string-match
                 (regexp-quote ";;; nemacs-gui-file-bridge-runtime.el")
                 text)))
          (dolist (needle '(";;; emacs-fileio-gui.el --- GUI bridge file/buffer adapter"
                            ";;; emacs-dired-min-gui.el --- GUI bridge Dired adapter"
                            ";;; emacs-info.el --- Minimal Info runtime"
                            ";;; emacs-help-gui.el --- GUI bridge Help adapter"))
            (should (string-match-p (regexp-quote needle) text)))
          (dolist (needle '("(defun emacs-fileio-gui-register-backend"
                            "(defun emacs-dired-min-gui-register-backend"
                            "(defun emacs-info-gui-register-backend"
                            "(defun emacs-help-gui-register-backend"))
            (should (< (string-match (regexp-quote needle) text)
                       bridge-pos))))
      (when (file-exists-p image)
        (delete-file image)))))

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
    (dolist (needle '("(if (eq files--bridge-command 'find-file) (find-file) nil)"
                      "(if (eq files--bridge-command 'describe-function) (describe-function) nil)"
                      "(if (eq files--bridge-command 'execute-extended-command) (execute-extended-command) nil)"))
      (ert-info ((format "Bridge-local call-interactively dispatch %s"
                         needle))
        (should-not (string-match-p (regexp-quote needle) source))))
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
	                      "emacs-minibuffer-gui-candidates-for-purpose"
	                      "(fset 'emacs-minibuffer-gui-begin-read"
	                      "(fset 'emacs-minibuffer-gui-complete"
                          "(defun files--minibuffer-gui-backend-buffer-candidates"
                          "(defun files--minibuffer-gui-backend-project-buffer-candidates"
                          "(defun files--minibuffer-gui-backend-extended-command-candidates"
                          "(defun files--minibuffer-gui-backend-key-candidates"
                          ":buffer-candidates"
                          ":project-buffer-candidates"
                          ":extended-command-candidates"
                          ":key-candidates"
		                      ":lookup-key-sequence"
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
			    (dolist (needle '("files--fileio-writeback-current-context"
                                  "(if (equal cmd \"find-file\")"
				                  "(if (equal cmd \"find-file-other-window\")"
				                  "(if (equal cmd \"find-file-read-only-other-window\")"
                                  "(if (equal cmd \"find-file-other-frame\")"
                                  "(if (equal cmd \"find-file-read-only-other-frame\")"
			                      "(if (equal cmd \"find-file-other-tab\")"
		                          "(if (equal cmd \"find-file-read-only-other-tab\")"
                                  "(if (equal cmd \"project-find-file\")"
                                  "(if (equal cmd \"save-buffer\")"
		                          "(if (equal cmd \"write-file\")"
                                  "(if (equal cmd \"switch-to-buffer\")"
					              "(if (equal cmd \"switch-to-buffer-other-window\")"
                                  "(if (equal cmd \"switch-to-buffer-other-frame\")"
			                      "(if (equal cmd \"switch-to-buffer-other-tab\")"
                                  "(if (equal cmd \"project-switch-to-buffer\")"
                                  "(if (equal cmd \"project-list-buffers\")"
			                      "(if (equal cmd \"display-buffer\")"
	                              "(if (equal cmd \"display-buffer-other-frame\")"
		                          "(if (equal cmd \"rename-buffer\")"
			                      "(if (equal cmd \"kill-buffer\")"
                                  "(if (equal cmd \"project-kill-buffers\")"
	                                      "(if (equal cmd \"project-find-dir\")"
                                      "(if (equal cmd \"project-dired\")"
                                      "(if (equal cmd \"project-switch-project\")"
	                                      "(if (equal cmd \"add-change-log-entry-other-window\")"
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
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-text" nil 'silent)
            (write-region "" nil "/tmp/nemacs-minibuffer-arg" nil 'silent)
            (write-region "seed\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
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
				            (should (equal "*Messages*"
				                           (nemacs-gui-file-bridge-runtime-test--slurp
				                            "/tmp/nemacs-buffer-name")))
				            (should (equal "1"
				                           (nemacs-gui-file-bridge-runtime-test--slurp
				                            "/tmp/nemacs-read-only")))
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

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-org-face-spans ()
  "M17: org headings and TODO/DONE keywords get resolved face spans."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-f" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "/tmp/nemacs-face-demo.org" nil "/tmp/nemacs-file" nil 'silent)
            (write-region "* TODO check\nplain text\n** DONE x\n"
                          nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((spans (nemacs-gui-file-bridge-runtime-test--slurp
                          "/tmp/nemacs-face-spans")))
              ;; "* TODO check\n": TODO keyword at [2,6), heading [0,12)
              (should (string-match-p "2\t6\torg-todo\t#ff6347" spans))
              (should (string-match-p "0\t12\torg-level\t#1e90ff" spans))
              ;; "** DONE x" at offset 24: DONE keyword at [27,31)
              (should (string-match-p "27\t31\torg-done\t#98fb98" spans))
              ;; plain line gets no span
              (should-not (string-match-p "13\t" spans))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-pkg-transpile-in-image ()
  "M19-2: transpiled user packages are callable inside the editor image."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (init-dir (make-temp-file "nemacs-m192-init" t))
          (pkg-dir (make-temp-file "nemacs-m192-pkg" t)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region (concat "(defun s-join (separator strings)\n"
                                  "  (mapconcat (lambda (x) x) strings separator))\n"
                                  "(provide 's)\n")
                          nil (expand-file-name "s.el" pkg-dir)
                          nil 'silent)
            (write-region (concat "(defun -map (function list)\n"
                                  "  (mapcar function list))\n"
                                  "(provide 'dash)\n")
                          nil (expand-file-name "dash.el" pkg-dir)
                          nil 'silent)
            (write-region (format "(add-to-list 'load-path %S)\n"
                                  pkg-dir)
                          nil (expand-file-name "early-init.el" init-dir)
                          nil 'silent)
            (write-region "(require 's)\n(require 'dash)\n"
                          nil (expand-file-name "init.el" init-dir)
                          nil 'silent)
            (load (expand-file-name "scripts/nemacs-wrap-init.el"
                                    nemacs-gui-file-bridge-runtime-test--repo-root)
                  nil t)
            (should (= 3 (nemacs-wrap-init
                          "/tmp/nemacs-init-wrapped"
                          (expand-file-name "early-init.el" init-dir)
                          (expand-file-name "init.el" init-dir))))
            (should (file-readable-p "/tmp/nemacs-init-wrapped-pkgs-lowered"))
            (when (file-exists-p "/tmp/nemacs-init-report")
              (delete-file "/tmp/nemacs-init-report"))
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-f" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abc" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             "(progn (nemacs-gui-file-bridge-run) (nl-write-file \"/tmp/nemacs-m192-probe\" (s-join \"-\" (-map (lambda (x) (number-to-string x)) (list 1 2 3)))))")
            (should (equal "1-2-3"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-m192-probe"))))
        (when (file-exists-p "/tmp/nemacs-m192-probe")
          (delete-file "/tmp/nemacs-m192-probe"))
        (dolist (f '("/tmp/nemacs-init-wrapped"
                     "/tmp/nemacs-init-wrapped-packages"
                     "/tmp/nemacs-init-wrapped-pkgs-lowered"
                     "/tmp/nemacs-init-report"))
          (when (file-exists-p f)
            (delete-file f)))
        (when (file-directory-p init-dir)
          (delete-directory init-dir t))
        (when (file-directory-p pkg-dir)
          (delete-directory pkg-dir t))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-ime-romaji-compose ()
  "M19-3: C-\\ enables the romaji IME; keys compose hiragana."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (copy-file (expand-file-name
                        "src/nemacs-ime-romaji.tsv"
                        nemacs-gui-file-bridge-runtime-test--repo-root)
                       "/tmp/nemacs-ime-table" t)
            (write-region "" nil "/tmp/nemacs-ime-pending" nil 'silent)
            (write-region "" nil "/tmp/nemacs-input-method" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
            (write-region "" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            ;; enable via C-\ (toggles files--input-method to non-empty)
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-\\" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "default"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-input-method")))
            (dolist (k '("k" "a" "n" "n" "n" "i"))
              (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region k nil "/tmp/nemacs-keys" nil 'silent)
              (nemacs-gui-file-bridge-runtime-test--run-ok
               reader image "(nemacs-gui-file-bridge-run)"))
            (should (equal "かんに"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf"))))
        (dolist (f '("/tmp/nemacs-ime-pending" "/tmp/nemacs-input-method"))
          (when (file-exists-p f)
            (write-region "" nil f nil 'silent)))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-view-slice ()
  "M20: buffers beyond the view cap reach the GUI as a rebased slice
on the view channel while nemacs-buf keeps the full round-trip text."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let* ((reader (nemacs-gui-file-bridge-runtime-test--reader))
           (image (nemacs-gui-file-bridge-runtime-test--write-image))
           (content (mapconcat (lambda (i) (format "line-%05d" i))
                               (number-sequence 1 10000) "\n"))
           (ws (* 11 9000))   ; line-09001 starts here (11 bytes per line)
           (pt (+ ws 25)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-f" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region content nil "/tmp/nemacs-buf" nil 'silent)
            (write-region (number-to-string pt) nil "/tmp/nemacs-point" nil 'silent)
            (write-region (number-to-string ws) nil "/tmp/nemacs-window-start" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            ;; full text still round-trips on nemacs-buf
            (should (equal content
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            ;; the GUI view is the window-start slice, rebased
            (let ((view (nemacs-gui-file-bridge-runtime-test--slurp
                         "/tmp/nemacs-view")))
              (should (equal (substring content ws
                                        (min (length content) (+ ws 49152)))
                             view))
              (should (string-prefix-p "line-09001" view)))
            (should (equal (number-to-string (+ 25 1))
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-view-point")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-view-start"))))
        (dolist (f '("/tmp/nemacs-view" "/tmp/nemacs-view-point"
                     "/tmp/nemacs-view-start"))
          (when (file-exists-p f)
            (delete-file f)))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-ime-kanji-convert ()
  "M19-3b: SPC converts the segment, SPC cycles, a letter commits.
Hermetic via a PATH-injected fake curl (the M11 stub-ssh pattern)."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let* ((reader (nemacs-gui-file-bridge-runtime-test--reader))
           (image (nemacs-gui-file-bridge-runtime-test--write-image))
           (fakebin (make-temp-file "ime-fakecurl" t))
           (process-environment
            (cons (concat "PATH=" fakebin ":" (getenv "PATH"))
                  process-environment)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (with-temp-file (expand-file-name "curl" fakebin)
              (insert "#!/bin/bash\nprintf '[[\"きょう\",[\"今日\",\"京\"]]]'\n"))
            (set-file-modes (expand-file-name "curl" fakebin) #o755)
            (copy-file (expand-file-name
                        "src/nemacs-ime-romaji.tsv"
                        nemacs-gui-file-bridge-runtime-test--repo-root)
                       "/tmp/nemacs-ime-table" t)
            (write-region "japanese" nil "/tmp/nemacs-input-method" nil 'silent)
            (dolist (f '("/tmp/nemacs-ime-pending" "/tmp/nemacs-ime-seg"
                         "/tmp/nemacs-ime-cands" "/tmp/nemacs-ime-idx"))
              (write-region "" nil f nil 'silent))
            (write-region "0" nil "/tmp/nemacs-minibuffer-active" nil 'silent)
            (write-region "" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (cl-flet ((key (k)
                        (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
                        (write-region k nil "/tmp/nemacs-keys" nil 'silent)
                        (nemacs-gui-file-bridge-runtime-test--run-ok
                         reader image "(nemacs-gui-file-bridge-run)")))
              (dolist (k '("k" "y" "o" "u")) (key k))
              (should (equal "きょう"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (key "SPC")
              (should (equal "今日"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (key "SPC")
              (should (equal "京"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (dolist (k '("d" "e")) (key k))
              (should (equal "京で"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))))
        (write-region "" nil "/tmp/nemacs-input-method" nil 'silent)
        (dolist (f '("/tmp/nemacs-ime-pending" "/tmp/nemacs-ime-seg"
                     "/tmp/nemacs-ime-cands" "/tmp/nemacs-ime-idx"))
          (when (file-exists-p f)
            (write-region "" nil f nil 'silent)))
        (when (file-directory-p fakebin)
          (delete-directory fakebin t))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-cjk-cursor-cells ()
  "M16: the cursor transport carries display cells (CJK = 2 cells)."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-b" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            ;; 日本語 = 9 bytes; C-b from byte 6 moves back one character to
            ;; byte 3, so the cell walk counts 日 = 2 cells.
            (write-region "日本語\nabc\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "6" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((cursor (nemacs-gui-file-bridge-runtime-test--slurp
                           "/tmp/nemacs-cursor")))
              (should (string-match-p "cells\t2" cursor))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-user-init-lane ()
  "M15: wrapped ~/.nemacs.d init forms load with per-form isolation
and the bridge reports applied/skipped instead of dying silently."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (init-dir (make-temp-file "nemacs-init-test" t)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            ;; Fixture init: three applicable forms + one the substrate
            ;; cannot apply.
            (write-region "(setq nemacs-theme-bg \"#000000\")\n"
                          nil (expand-file-name "early-init.el" init-dir)
                          nil 'silent)
            (write-region (concat "(setq fill-column 84)\n"
                                  "(this-function-does-not-exist 1)\n"
                                  "(defun my-init-fn (x) (* x 2))\n")
                          nil (expand-file-name "init.el" init-dir)
                          nil 'silent)
            ;; Generate the wrapper with the REAL generator.
            (load (expand-file-name "scripts/nemacs-wrap-init.el"
                                    nemacs-gui-file-bridge-runtime-test--repo-root)
                  nil t)
            (should (= 4 (nemacs-wrap-init
                          "/tmp/nemacs-init-wrapped"
                          (expand-file-name "early-init.el" init-dir)
                          (expand-file-name "init.el" init-dir))))
            (when (file-exists-p "/tmp/nemacs-init-report")
              (delete-file "/tmp/nemacs-init-report"))
            ;; Any bridge run loads the wrapper once per mtime.
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-f" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "abc\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (let ((report (nemacs-gui-file-bridge-runtime-test--slurp
                           "/tmp/nemacs-init-report")))
              (should (string-match-p "total\t4" report))
              (should (string-match-p "applied\t3" report))
              (should (string-match-p "skipped\t1" report))
              (should (string-match-p
                       (regexp-quote "this-function-does-not-exist") report)))
            ;; The applied setq is live in the same runtime image flow:
            ;; customize renders the init.el value.
            (write-region "customize-variable" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "fill-column" nil "/tmp/nemacs-arg" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (string-match-p
                     (regexp-quote "Value: 84")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf"))))
        (dolist (f '("/tmp/nemacs-init-wrapped" "/tmp/nemacs-init-report"))
          (when (file-exists-p f)
            (delete-file f)))
        (when (file-directory-p init-dir)
          (delete-directory init-dir t))
        (when (file-exists-p image)
          (delete-file image))))))

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

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-info-direct-core-runtime-adapter ()
  "Standalone direct `info' should enter the Info GUI runtime core."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file (make-temp-file "nemacs-gui-info-core-runtime-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq emacs-info-gui-arg \"\")
                 (setq emacs-info-gui-file \"\")
                 (setq emacs-info-gui-node \"\")
                 (setq emacs-info-gui-status \"ok\")
                 (setq emacs-info-gui-buffer-name \"\")
                 (fset 'emacs-info-gui-set-context
                       (lambda (&rest _plist)
                         (setq emacs-info-gui-arg files--bridge-arg)))
                 (fset 'emacs-info-gui-info-core
                       (lambda ()
                         (nl-write-file %S emacs-info-gui-arg)
                         (setq emacs-info-gui-status \"ok\")
                         (setq emacs-info-gui-file emacs-info-gui-arg)
                         (setq emacs-info-gui-node \"Top\")
                         (setq emacs-info-gui-buffer-name \"*info*\")
                         \"*info*\"))
                 (setq files--bridge-arg \"/tmp/nemacs-info-core-probe.info\")
                 (info))"
              probe-file))
            (should (equal "/tmp/nemacs-info-core-probe.info"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (dolist (f (list image probe-file))
          (when (and f (file-exists-p f))
            (delete-file f)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-info-current-context-command-wrappers ()
  "Standalone Info command wrappers should prefer current-context helpers."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file (make-temp-file "nemacs-gui-info-current-context-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq probe-log \"\")
                 (fset 'append-log
                       (lambda (text)
                         (setq probe-log
                               (concat probe-log text \"\\n\"))))
                 (fset 'emacs-info-gui-current-context-command
                       (lambda (command &optional action)
                         (append-log
                          (concat (symbol-name command)
                                  \":\"
                                  (if action action \"\")))
                         \"*info*\"))
                 (info)
                 (info-other-window)
                 (Info-next)
                 (Info-prev)
                 (Info-up)
                 (info-emacs-manual)
                 (info-display-manual)
                 (view-order-manuals)
                 (Info-goto-emacs-command-node)
                 (Info-goto-emacs-key-command-node)
                 (info-lookup-symbol)
                 (nl-write-file %S probe-log))"
              probe-file))
            (should
             (equal
              "info:same\ninfo-other-window:other\nInfo-next:\nInfo-prev:\nInfo-up:\ninfo-emacs-manual:\ninfo-display-manual:\nview-order-manuals:\nInfo-goto-emacs-command-node:\nInfo-goto-emacs-key-command-node:\ninfo-lookup-symbol:\n"
              (nemacs-gui-file-bridge-runtime-test--slurp probe-file))))
        (dolist (f (list image probe-file))
          (when (and f (file-exists-p f))
            (delete-file f)))))))

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

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-insert-file-runtime-adapter ()
  "In standalone NeLisp, `insert-file' should go through fileio GUI runtime."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (source (make-temp-file "nemacs-gui-file-bridge-insert-file-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "INSERT" nil source nil 'silent)
            (write-region "insert-file" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region source nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "ab" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "aINSERTb"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 7 (nemacs-gui-file-bridge-runtime-test--point-value))))
        (when (file-exists-p source)
          (delete-file source))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-insert-buffer-runtime-adapter ()
  "In standalone NeLisp, `insert-buffer' should go through fileio GUI runtime."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "other text\n" nil "/tmp/nemacs-buffer-store/other" nil 'silent)
            (write-region "insert-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "other" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "before after\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "7" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "before other text\nafter\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buf")))
            (should (= 18 (nemacs-gui-file-bridge-runtime-test--point-value))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-save-some-buffers-runtime-adapter ()
  "In standalone NeLisp, `save-some-buffers' should go through fileio GUI runtime."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (main-file (make-temp-file "nemacs-gui-save-some-main-"))
          (other-file (make-temp-file "nemacs-gui-save-some-other-"))
          (readonly-file (make-temp-file "nemacs-gui-save-some-readonly-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "old main\n" nil main-file nil 'silent)
            (write-region "old other\n" nil other-file nil 'silent)
            (write-region "old readonly\n" nil readonly-file nil 'silent)
            (write-region "save-some-buffers" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main changed\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region main-file nil "/tmp/nemacs-file" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (write-region "main\nother\nreadonly\n" nil "/tmp/nemacs-buffer-list" nil 'silent)
            (write-region "other changed\n" nil "/tmp/nemacs-buffer-store/other" nil 'silent)
            (write-region other-file nil "/tmp/nemacs-buffer-file-store/other" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-buffer-read-only-store/other" nil 'silent)
            (write-region "readonly changed\n" nil "/tmp/nemacs-buffer-store/readonly" nil 'silent)
            (write-region readonly-file nil "/tmp/nemacs-buffer-file-store/readonly" nil 'silent)
            (write-region "1" nil "/tmp/nemacs-buffer-read-only-store/readonly" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "main changed\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            main-file)))
            (should (equal "other changed\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            other-file)))
            (should (equal "old readonly\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            readonly-file))))
        (dolist (file (list main-file other-file readonly-file image))
          (when (file-exists-p file)
            (delete-file file)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-before-command-policy ()
  "In standalone NeLisp, command-loop runtime should clear cycle-spacing state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "forward-char" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "abc" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-window-start" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "one" nil "/tmp/nemacs-cycle-spacing-action" nil 'silent)
            (write-region "2" nil "/tmp/nemacs-cycle-spacing-point" nil 'silent)
            (write-region "  " nil "/tmp/nemacs-cycle-spacing-whitespace" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-cycle-spacing-action")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-cycle-spacing-point")))
            (should (equal ""
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-cycle-spacing-whitespace")))
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--cycle-spacing-action \"again\")
                       (files--command-loop-backend-before-command
                        'describe-key)
                       (nl-write-file
                        \"/tmp/nemacs-cycle-spacing-before-command-probe\"
                        files--cycle-spacing-action))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal ""
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-cycle-spacing-before-command-probe")))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-self-insert-key-policy ()
  "In standalone NeLisp, command-loop runtime should own self-insert key policy."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-loop-self-insert-policy"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (nl-write-file
                      \"/tmp/nemacs-command-loop-self-insert-policy\"
                      (concat
                       (emacs-command-loop-gui-self-insert-key-text \"a\")
                       \"\\t\"
                       (emacs-command-loop-gui-self-insert-key-text \"SPC\")
                       \"\\t\"
                       (if (emacs-command-loop-gui-self-insert-key-text \"C-x\")
                           \"bad\"
                         \"nil\"))))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "a\t \tnil"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

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
            (nemacs-gui-file-bridge-runtime-test--with-persistent-runner
                reader image
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
              (should (equal "outside\n*scratch*\n*Messages*\n*Warnings*\n"
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
                  (delete-file exit-file))))))
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

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-special-buffers ()
  "Standalone bridge should expose scratch, messages, and warnings buffers."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image)))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "*scratch*" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "switch-to-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*scratch*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (string-match-p
                     "This buffer is for text"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (equal "0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-read-only")))
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             "(progn
                (files--refresh-transport-derived-paths)
                (files--ensure-standard-special-buffers)
                (message \"bridge message %s\" \"one\")
                (display-warning 'nemacs \"bridge warning\" 'warning))")
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "C-h e" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Messages*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (string-match-p
                     "bridge message one"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (string-match-p
                     "Warning \\[nemacs\\]: bridge warning"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf")))
            (should (equal "1"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-read-only")))
            (write-region "warnings-buffer" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "*Warnings*"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-buffer-name")))
            (should (string-match-p
                     "Warning \\[nemacs\\]: bridge warning"
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      "/tmp/nemacs-buf"))))
        (when (file-exists-p image)
          (delete-file image))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-vc-keymap-coverage ()
  "Standalone key lookup should cover the host Emacs C-x v VC prefix."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (state (make-temp-file "nemacs-gui-file-bridge-vc-keymap-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
            (write-region "" nil "/tmp/nemacs-arg" nil 'silent)
            (write-region "" nil "/tmp/nemacs-keys" nil 'silent)
            (write-region "x\n" nil "/tmp/nemacs-buf" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-point" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-mark" nil 'silent)
            (write-region "0" nil "/tmp/nemacs-read-only" nil 'silent)
            (write-region "main" nil "/tmp/nemacs-buffer-name" nil 'silent)
            (when (file-exists-p "/tmp/nemacs-status")
              (delete-file "/tmp/nemacs-status"))
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq files--bridge-keys \"C-x v d\")
                 (nl-write-file %S (files--lookup-key-sequence))
                 (setq files--bridge-keys \"C-x v L\")
                 (nl-write-file %S (files--lookup-key-sequence))
                 (setq files--bridge-keys \"C-x v M D\")
                 (nl-write-file %S (files--lookup-key-sequence))
                 (setq files--bridge-keys \"C-x v v\")
                 (nl-write-file %S (files--lookup-key-sequence)))"
              state
              (concat state ".root-log")
              (concat state ".mergebase")
              (concat state ".next")))
            (should (equal "vc-dir"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            state)))
            (should (equal "vc-print-root-log"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            (concat state ".root-log"))))
            (should (equal "vc-diff-mergebase"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            (concat state ".mergebase"))))
            (should (equal "vc-next-action"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            (concat state ".next"))))
            (write-region "C-x v v" nil "/tmp/nemacs-keys" nil 'silent)
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image "(nemacs-gui-file-bridge-run)")
            (should (equal "unsupported"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            "/tmp/nemacs-status"))))
        (when (file-exists-p image)
          (delete-file image))
        (dolist (file (list state
                            (concat state ".root-log")
                            (concat state ".mergebase")
                            (concat state ".next")))
          (when (file-exists-p file)
            (delete-file file)))))))

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

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-help-description-core-runtime-adapter ()
  "Standalone direct Help descriptions should enter GUI runtime cores."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (function-probe (make-temp-file "nemacs-gui-help-function-core-"))
          (variable-probe (make-temp-file "nemacs-gui-help-variable-core-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq emacs-help-gui-arg \"\")
                 (setq emacs-help-gui-current-file-name \"\")
                 (setq emacs-help-gui-buffer-name \"\")
                 (setq emacs-help-gui-buffer-read-only-p nil)
                 (fset 'emacs-help-gui-set-context
                       (lambda (&rest _plist)
                         (setq emacs-help-gui-arg files--bridge-arg)))
                 (fset 'emacs-help-gui-describe-function-core
                       (lambda ()
                         (nl-write-file %S emacs-help-gui-arg)
                         (cons \"Function Core\" \"function core body\")))
                 (fset 'emacs-help-gui-describe-variable-core
                       (lambda ()
                         (nl-write-file %S emacs-help-gui-arg)
                         (cons \"Variable Core\" \"variable core body\")))
                 (setq files--bridge-arg \"forward-char\")
                 (describe-function)
                 (setq files--bridge-arg \"buffer-file-name\")
                 (describe-variable))"
              function-probe variable-probe))
            (should (equal "forward-char"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            function-probe)))
            (should (equal "buffer-file-name"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            variable-probe))))
        (dolist (file (list image function-probe variable-probe))
          (when (and file (file-exists-p file))
            (delete-file file)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-help-keymap-core-runtime-adapter ()
  "Standalone direct keymap Help commands should enter GUI runtime cores."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (key-probe (make-temp-file "nemacs-gui-help-key-core-"))
          (brief-probe (make-temp-file "nemacs-gui-help-brief-core-"))
          (bindings-probe (make-temp-file "nemacs-gui-help-bindings-core-"))
          (where-probe (make-temp-file "nemacs-gui-help-where-core-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq emacs-help-gui-arg \"\")
                 (fset 'emacs-help-gui-set-context
                       (lambda (&rest _plist)
                         (setq emacs-help-gui-arg files--bridge-arg)))
                 (fset 'emacs-help-gui-describe-key-core
                       (lambda ()
                         (nl-write-file %S emacs-help-gui-arg)
                         (cons \"Key Core\" \"key core body\")))
                 (fset 'emacs-help-gui-describe-key-briefly-core
                       (lambda ()
                         (nl-write-file %S emacs-help-gui-arg)
                         (cons \"Brief Core\" \"brief core body\")))
                 (fset 'emacs-help-gui-describe-bindings-core
                       (lambda ()
                         (nl-write-file %S \"bindings\")
                         (cons \"Bindings Core\" \"bindings core body\")))
                 (fset 'emacs-help-gui-where-is-core
                       (lambda ()
                         (nl-write-file %S emacs-help-gui-arg)
                         (cons \"Where Core\" \"where core body\")))
                 (setq files--bridge-arg \"C-x C-f\")
                 (describe-key)
                 (setq files--bridge-arg \"C-x C-s\")
                 (describe-key-briefly)
                 (describe-bindings)
                 (setq files--bridge-arg \"find-file\")
                 (where-is))"
              key-probe brief-probe bindings-probe where-probe))
            (should (equal "C-x C-f"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            key-probe)))
            (should (equal "C-x C-s"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            brief-probe)))
            (should (equal "bindings"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            bindings-probe)))
            (should (equal "find-file"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            where-probe))))
        (dolist (file (list image key-probe brief-probe bindings-probe
                            where-probe))
          (when (and file (file-exists-p file))
            (delete-file file)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-help-current-context-command-wrappers ()
  "Standalone Help command wrappers should prefer current-context helpers."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file (make-temp-file "nemacs-gui-help-current-context-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq probe-log \"\")
                 (fset 'append-log
                       (lambda (text)
                         (setq probe-log
                               (concat probe-log text \"\\n\"))))
                 (fset 'emacs-help-gui-current-context-command
                       (lambda (command &optional static-command)
                         (append-log
                          (concat (symbol-name command)
                                  \":\"
                                  (if static-command
                                      (symbol-name static-command)
                                    \"\")))
                         \"*Help*\"))
                 (describe-function)
                 (describe-variable)
                 (describe-key)
                 (describe-key-briefly)
                 (describe-bindings)
                 (where-is)
                 (help-for-help)
                 (describe-command)
                 (describe-package)
                 (describe-symbol)
                 (apropos-command)
                 (apropos-documentation)
                 (finder-by-keyword)
                 (nl-write-file %S probe-log))"
              probe-file))
            (should
             (equal
              "describe-function:\ndescribe-variable:\ndescribe-key:\ndescribe-key-briefly:\ndescribe-bindings:\nwhere-is:\nhelp-for-help:\ndescribe-command:\ndescribe-package:\ndescribe-symbol:\napropos-command:\napropos-documentation:\nfinder-by-keyword:finder-by-keyword\n"
              (nemacs-gui-file-bridge-runtime-test--slurp probe-file))))
        (dolist (file (list image probe-file))
          (when (and file (file-exists-p file))
            (delete-file file)))))))

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
                                 files--org-buffer-p
                                 files--info-token-at
                                 files--face-spans-org
                                 files--face-keyword-p
                                 files--symbol-char-p
                                 files--face-span-line
                                 files--face-theme-path
                                 files--face-theme-field
                                 files--load-face-theme
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
    (should (= 11 (length forms)))
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
      (defvar files--font-default-cell-width)
      (defvar files--font-cjk-cell-width)
      (defvar files--font-name)
      (defvar files--font-script)
      (defvar files--face-spans-file)
      (defvar files--view-rebase)
      (defvar files--face-theme-loaded)
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
            files--font-default-cell-width 9
            files--font-cjk-cell-width 6
            files--face-org-heading-color "#1e90ff"
            files--face-org-todo-color "#ff6347"
            files--face-org-done-color "#98fb98"
            files--view-rebase 0
            files--face-theme-loaded t
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
              (should (string-match-p "script\tlatin" font))
              (should (string-match-p "cw\t9" font)))
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
              (should (string-match-p "iso10646" font))
              (should (string-match-p "normal-ja" font))
              (should (string-match-p "cw\t6" font))))
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

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-fileio-core-delegates-to-runtime ()
  "Standalone direct file/buffer cores should enter `emacs-fileio-gui'."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (find-flag (make-temp-file "nemacs-gui-fileio-find-"))
          (save-flag (make-temp-file "nemacs-gui-fileio-save-"))
          (switch-flag (make-temp-file "nemacs-gui-fileio-switch-"))
          (kill-flag (make-temp-file "nemacs-gui-fileio-kill-"))
          (list-flag (make-temp-file "nemacs-gui-fileio-list-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (fset 'emacs-fileio-gui-find-file-core
                       (lambda ()
                         (nl-write-file %S \"find\")
                         (setq files--current-file-name files--bridge-arg)
                         files--current-file-name))
                 (fset 'emacs-fileio-gui-save-buffer-core
                       (lambda ()
                         (nl-write-file %S \"save\")
                         files--current-file-name))
                 (fset 'emacs-fileio-gui-switch-to-buffer-command
                       (lambda (&optional action)
                         (nl-write-file %S
                                        (if (equal action \"same\")
                                            \"switch\"
                                          \"bad-switch\"))))
                 (fset 'emacs-fileio-gui-kill-buffer-command
                       (lambda ()
                         (nl-write-file %S \"kill\")))
                 (fset 'emacs-fileio-gui-list-buffers-command
                       (lambda ()
                         (nl-write-file %S \"list\")))
                 (setq files--bridge-arg \"/tmp/nemacs-fileio-probe\")
                 (files--find-file-core)
                 (files--save-buffer-core)
                 (setq files--bridge-arg \"notes\")
                 (files--switch-to-buffer)
                 (setq files--bridge-arg \"notes\")
                 (files--kill-buffer-core)
                 (files--list-buffers-core))"
              find-flag save-flag switch-flag kill-flag list-flag))
            (should (equal "find"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            find-flag)))
            (should (equal "save"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            save-flag)))
            (should (equal "switch"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            switch-flag)))
            (should (equal "kill"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            kill-flag)))
            (should (equal "list"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            list-flag))))
        (dolist (f (list image find-flag save-flag switch-flag kill-flag
                         list-flag))
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

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-dired-list-directory-runtime-adapter ()
  "Standalone direct `list-directory' should enter the Dired GUI helper."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file (make-temp-file "nemacs-gui-dired-list-runtime-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (fset 'emacs-dired-min-gui-dired-command
                       (lambda (&optional action)
                         (nl-write-file %S
                                        (if (equal action \"same\")
                                            \"list\"
                                          \"bad-action\"))
                         \"*Directory*\"))
                 (setq files--bridge-arg \"/tmp\")
                 (list-directory))"
              probe-file))
            (should (equal "list"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (dolist (f (list image probe-file))
          (when (and f (file-exists-p f)) (delete-file f)))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-dired-current-context-command-wrappers ()
  "Standalone Dired command wrappers should prefer current-context helpers."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file (make-temp-file "nemacs-gui-dired-current-context-")))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (nemacs-gui-file-bridge-runtime-test--run-ok
             reader image
             (format
              "(progn
                 (setq probe-log \"\")
                 (fset 'append-log
                       (lambda (text)
                         (setq probe-log
                               (concat probe-log text \"\\n\"))))
                 (fset 'emacs-dired-min-gui-current-context-command
                       (lambda (command &optional action)
                         (if (eq command 'dired)
                             (append-log (concat \"dired:\" action))
                           nil)
                         (if (eq command 'dired-jump)
                             (append-log (concat \"jump:\" action))
                           nil)
                         (if (eq command 'project-find-dir)
                             (append-log (concat \"project-find-dir:\"
                                                 action))
                           nil)
                         (if (eq command 'project-dired)
                             (append-log (concat \"project-dired:\"
                                                 action))
                           nil)
                         (if (eq command 'dired-mark)
                             (append-log \"mark\")
                           nil)
                         (if (eq command 'dired-unmark)
                             (append-log \"unmark\")
                           nil)
                         (if (eq command 'dired-flag-file-deletion)
                             (append-log \"flag\")
                           nil)
                         (if (eq command 'dired-do-flagged-delete)
                             (append-log \"delete\")
                           nil)
                         (if (eq command 'dired-do-rename)
                             (append-log \"rename\")
                           nil)
                         (if (eq command 'dired-do-copy)
                             (append-log \"copy\")
                           nil)
                         \"*Directory*\"))
                 (dired)
                 (dired-other-window)
                 (dired-other-frame)
                 (dired-other-tab)
                 (dired-jump)
                 (dired-jump-other-window)
                 (project-find-dir)
                 (project-dired)
                 (dired-mark)
                 (dired-unmark)
                 (dired-flag-file-deletion)
                 (dired-do-flagged-delete)
                 (dired-do-rename)
                 (dired-do-copy)
                 (nl-write-file %S probe-log))"
              probe-file))
            (should (equal "dired:same\ndired:other\ndired:frame\ndired:tab\njump:same\njump:other\nproject-find-dir:same\nproject-dired:same\nmark\nunmark\nflag\ndelete\nrename\ncopy\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (dolist (f (list image probe-file))
          (when (and f (file-exists-p f)) (delete-file f)))))))

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
        ;; toggle-input-method now PERSISTS its state (M19-3) — leave
        ;; the IM off so later tests' letter keys self-insert instead
        ;; of feeding the romaji composer
        (write-region "" nil "/tmp/nemacs-input-method" nil 'silent)
        (write-region "" nil "/tmp/nemacs-transient-input-method" nil 'silent)
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

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-commandp-runtime-accepted-policy ()
  "commandp should use command-loop runtime accepted policy in source-v1."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-commandp-runtime-policy"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'nemacs-unregistered-probe
                           (lambda () (setq files--bridge-status \"probe\")))
                     (setq files--bridge-command 'nemacs-unregistered-probe)
                     (nl-write-file
                      \"/tmp/nemacs-commandp-runtime-policy\"
                      (if (commandp) \"accepted\" \"rejected\")))")))
            (should (equal 0 (plist-get result :status)))
            (should (string-match-p "accepted"
                                    (nemacs-gui-file-bridge-runtime-test--slurp
                                     probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-minibuffer-mode-keymap-entry-first ()
  "Bridge-only source-v1 should parse mode-local minibuffer keys first."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-minibuffer-mode-keymap-policy"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'files--mode-minibuffer-keymap-source
                           (lambda ()
                             \"C-c x\\tmode-command\\tMode prompt: \\n\"))
                     (setq files--minibuffer-keymap-source
                           \"C-c x\\tglobal-command\\tGlobal prompt: \\n\")
                     (setq files--bridge-keys \"C-c x\")
                     (let ((entry
                            (emacs-minibuffer-gui-keymap-entry
                             (concat (files--mode-minibuffer-keymap-source)
                                     files--minibuffer-keymap-source)
                             files--bridge-keys)))
                       (nl-write-file
                        \"/tmp/nemacs-minibuffer-mode-keymap-policy\"
                        (concat (car entry) \"\\t\" (cdr entry)))))")))
            (should (equal 0 (plist-get result :status)))
            (should (string-match-p
                     (regexp-quote "mode-command\tMode prompt: ")
                     (nemacs-gui-file-bridge-runtime-test--slurp
                      probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-minibuffer-start-spec-from-keymaps ()
  "Bridge source-v1 should expose normalized minibuffer start specs."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-minibuffer-start-spec-from-keymaps"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (let ((spec
                            (emacs-minibuffer-gui-start-spec-from-keymaps
                             \"C-c x\\tmode-command\\tMode prompt: \\n\"
                             \"C-c x\\tglobal-command\\tGlobal prompt: \\nC-c y\\tglobal-y\\tGlobal y: \\n\"
                             \"C-c x\"
                             \"seed\")))
                       (nl-write-file
                        \"/tmp/nemacs-minibuffer-start-spec-from-keymaps\"
                        (concat (plist-get spec :purpose)
                                \"\\t\"
                                (plist-get spec :prompt)
                                \"\\t\"
                                (plist-get spec :initial-input)
                                \"\\t\"
                                (symbol-name (plist-get spec :source))))))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "mode-command\tMode prompt: \tseed\tmode"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-files-maybe-start-keymap-current-context ()
  "Bridge keymap minibuffer start should enter minibuffer current-context runtime."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-files-maybe-start-keymap-current-context"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-minibuffer-gui-maybe-start-from-keymaps
                           (lambda (&rest _args)
                             (error \"direct keymap helper used\")))
                     (fset 'emacs-minibuffer-gui-maybe-start-current-context
                           (lambda ()
                             (nl-write-file
                             \"/tmp/nemacs-files-maybe-start-keymap-current-context\"
                             (concat files--bridge-keys
                                     \"\\t\"
                                      files--bridge-arg))
                             t))
                     (setq files--bridge-keys \"C-x C-f\")
                     (setq files--bridge-arg \"/tmp/current-context.txt\")
                     (files--maybe-start-minibuffer-from-keymap))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "C-x C-f\t/tmp/current-context.txt"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-minibuffer-finish-delegates-to-runtime ()
  "Bridge minibuffer finish wrapper should leave commit policy to runtime."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-minibuffer-finish-runtime"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-minibuffer-gui-finish-read
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-minibuffer-finish-runtime\"
                              files--minibuffer-purpose)
                             :runtime-finished))
                     (setq files--minibuffer-purpose \"zap-to-char\")
                     (files--minibuffer-finish))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "zap-to-char"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-minibuffer-start-policy ()
  "Command-loop helper should delegate minibuffer start policy to runtime."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-loop-minibuffer-start-policy"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-minibuffer-gui-maybe-start-current-context
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-command-loop-minibuffer-start-policy\"
                              \"current-context\")
                             t))
                     (fset 'files--mode-minibuffer-keymap-source
                           (lambda ()
                             \"mode-source\"))
                     (setq files--minibuffer-keymap-source
                           \"global-source\")
                     (setq files--bridge-keys \"C-c x\")
                     (setq files--bridge-arg \"seed\")
                     (emacs-command-loop-gui-maybe-start-minibuffer))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "current-context"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
	        (when (file-exists-p probe-file)
	          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-files-minibuffer-handle-key-uses-command-loop ()
  "Bridge minibuffer key wrapper should enter command-loop runtime first."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-files-minibuffer-handle-key-command-loop"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-command-loop-gui-minibuffer-handle-key
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-files-minibuffer-handle-key-command-loop\"
                              (concat files--bridge-keys
                                      \"\\t\"
                                      files--minibuffer-purpose))
                             :handled))
                     (fset 'emacs-minibuffer-gui-handle-key-current-context
                           (lambda ()
                             (error \"direct minibuffer key helper used\")))
                     (setq files--bridge-keys \"a\")
                     (setq files--minibuffer-purpose \"switch-to-buffer\")
                     (files--minibuffer-handle-key))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "a\tswitch-to-buffer"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-files-minibuffer-handle-key-current-context ()
  "Bridge minibuffer key fallback should enter minibuffer current-context runtime."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-files-minibuffer-handle-key-current-context"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-minibuffer-gui-handle-key
                           (lambda (&rest _args)
                             (error \"direct minibuffer handle helper used\")))
                     (fset 'emacs-minibuffer-gui-handle-key-current-context
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-files-minibuffer-handle-key-current-context\"
                              (concat files--bridge-keys
                                      \"\\t\"
                                      files--minibuffer-purpose))
                             :handled))
                     (setq files--command-loop-minibuffer-handle-delegating t)
                     (setq files--bridge-keys \"a\")
                     (setq files--minibuffer-purpose \"switch-to-buffer\")
                     (files--minibuffer-handle-key))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "a\tswitch-to-buffer"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-files-maybe-start-minibuffer-uses-command-loop ()
  "Bridge minibuffer-start wrapper should enter command-loop runtime first."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-files-maybe-start-minibuffer-command-loop"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-command-loop-gui-maybe-start-minibuffer
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-files-maybe-start-minibuffer-command-loop\"
                              (concat files--bridge-keys
                                      \"\\t\"
                                      files--bridge-arg))
                             t))
                     (fset 'emacs-minibuffer-gui-maybe-start-current-context
                           (lambda ()
                             (error \"direct minibuffer helper used\")))
                     (setq files--bridge-keys \"C-x C-f\")
                     (setq files--bridge-arg \"/tmp/from-wrapper.txt\")
                     (files--maybe-start-minibuffer))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "C-x C-f\t/tmp/from-wrapper.txt"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-minibuffer-start-uses-current-context-fallback ()
  "Bridge-only command-loop minibuffer start should use runtime-named fallback."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-loop-minibuffer-current-fallback"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'files--maybe-start-minibuffer-from-keymap
                           (lambda ()
                             (error \"legacy minibuffer keymap fallback used\")))
                     (setq files--bridge-keys \"C-x C-f\")
                     (setq files--bridge-arg \"\")
                     (setq files--minibuffer-active nil)
                     (emacs-command-loop-gui-maybe-start-minibuffer)
                     (nl-write-file
                      \"/tmp/nemacs-command-loop-minibuffer-current-fallback\"
                      (concat (if files--minibuffer-active \"1\" \"0\")
                              \"\\t\"
                              files--minibuffer-purpose
                              \"\\t\"
                              files--minibuffer-prompt
                              \"\\t\"
                              files--bridge-status)))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "1\tfind-file\tFind file: \tminibuffer"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-minibuffer-start-backend-adapter ()
  "Bridge command-loop backend should keep minibuffer policy in runtime."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-loop-minibuffer-start-backend-adapter"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-minibuffer-gui-maybe-start-current-context
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-command-loop-minibuffer-start-backend-adapter\"
                              (concat files--bridge-keys
                                      \"\\t\"
                                      files--bridge-arg))
                             t))
                     (setq files--bridge-keys \"C-x C-f\")
                     (setq files--bridge-arg \"/tmp/a.txt\")
                     (files--command-loop-backend-maybe-start-minibuffer))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "C-x C-f\t/tmp/a.txt"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-lookup-key-runtime-adapter ()
  "Bridge key lookup should delegate source precedence to command-loop runtime."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-loop-lookup-key-runtime-adapter")
          (result-file "/tmp/nemacs-command-loop-lookup-key-runtime-result"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-image
                    reader image
                    "(progn
                       (fset 'emacs-command-loop-gui-lookup-key-sequence-from-sources
                             (lambda (key user-source mode-source global-source)
                               (nl-write-file
                                \"/tmp/nemacs-command-loop-lookup-key-runtime-adapter\"
                                (concat key
                                        \"\\t\"
                                        user-source
                                        \"\\t\"
                                        mode-source
                                        \"\\t\"
                                        global-source))
                               \"runtime-command\"))
                       (fset 'files--mode-keymap-source
                             (lambda ()
                               \"mode-source\"))
                       (setq files--keymap-source \"global-source\")
                       (nl-write-file (files--user-keymap-path) \"user-source\")
                       (setq files--bridge-keys \"C-c x\")
                       (nl-write-file
                        \"/tmp/nemacs-command-loop-lookup-key-runtime-result\"
                        (files--lookup-key-sequence)))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "runtime-command"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              result-file)))
              (should (equal "C-c x\tuser-source\tmode-source\tglobal-source"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))
        (when (file-exists-p result-file)
          (delete-file result-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-lookup-key-backend-adapter ()
  "Command-loop backend key lookup should only gather bridge sources."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-loop-lookup-key-backend-adapter")
          (result-file "/tmp/nemacs-command-loop-lookup-key-backend-result"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-image
                    reader image
                    "(progn
                       (fset 'emacs-command-loop-gui-lookup-key-sequence-from-sources
                             (lambda (key user-source mode-source global-source)
                               (nl-write-file
                                \"/tmp/nemacs-command-loop-lookup-key-backend-adapter\"
                                (concat key
                                        \"\\t\"
                                        user-source
                                        \"\\t\"
                                        mode-source
                                        \"\\t\"
                                        global-source))
                               \"backend-runtime-command\"))
                       (fset 'files--mode-keymap-source
                             (lambda ()
                               \"mode-source\"))
                       (setq files--keymap-source \"global-source\")
                       (nl-write-file (files--user-keymap-path) \"user-source\")
                       (setq files--bridge-keys \"C-c b\")
                       (nl-write-file
                        \"/tmp/nemacs-command-loop-lookup-key-backend-result\"
                        (files--command-loop-backend-lookup-key-sequence)))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "backend-runtime-command"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              result-file)))
              (should (equal "C-c b\tuser-source\tmode-source\tglobal-source"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))
        (when (file-exists-p result-file)
          (delete-file result-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-finish-command-policy ()
  "Standalone source-v1 should expose GUI command finish bookkeeping."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-loop-finish-command-policy"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (setq emacs-command-loop--this-command 'finish-probe)
                     (setq emacs-command-loop--real-this-command 'finish-probe)
                     (setq emacs-command-loop--last-command nil)
                     (setq emacs-command-loop--this-command-keys \"C-x C-f\")
                     (emacs-command-loop-gui-finish-command)
                     (nl-write-file
                      \"/tmp/nemacs-command-loop-finish-command-policy\"
                      (if (eq emacs-command-loop--last-command 'finish-probe)
                          (if emacs-command-loop--this-command
                              \"bad-this\"
                            (if emacs-command-loop--real-this-command
                                \"bad-real\"
                              (if (equal emacs-command-loop--this-command-keys
                                         \"C-x C-f\")
                                  \"ok\"
                                \"bad-keys\")))
                        \"bad-last\")))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "ok"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-call-interactively-context-policy ()
  "Standalone source-v1 should route `call-interactively' through the helper."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-call-interactively-context-policy"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'nemacs-call-interactively-context-probe
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-call-interactively-context-policy\"
                              (if (eq emacs-command-loop--this-command
                                      'nemacs-call-interactively-context-probe)
                                  \"called\"
                                \"bad-this\"))))
                     (setq files--bridge-command
                           'nemacs-call-interactively-context-probe)
                     (setq files--bridge-effective-command
                           \"nemacs-call-interactively-context-probe\")
                     (setq files--bridge-keys \"M-x\")
                     (setq files--bridge-arg \"\")
                     (setq files--bridge-status \"ok\")
                     (setq files--prefix-arg \"\")
                     (call-interactively)
                     (if (eq emacs-command-loop--last-command
                             'nemacs-call-interactively-context-probe)
                         nil
                       (nl-write-file
                        \"/tmp/nemacs-call-interactively-context-policy\"
                        \"bad-last\")))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "called"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-call-interactively-runtime-delegation ()
  "Standalone `call-interactively' should not fall back to bridge-local command dispatch."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-call-interactively-runtime-delegation"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-command-loop-gui-call-interactively-current-context
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-call-interactively-runtime-delegation\"
                              \"runtime\")
                             :runtime))
                     (fset 'find-file
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-call-interactively-runtime-delegation\"
                              \"direct\")
                             :direct))
                     (setq files--bridge-command 'find-file)
                     (setq files--bridge-effective-command \"find-file\")
                     (call-interactively))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "runtime"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-execute-runtime-delegation ()
  "Standalone `command-execute' should delegate policy to command-loop runtime."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-execute-runtime-delegation"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-command-loop-gui-command-execute-current-context
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-command-execute-runtime-delegation\"
                              \"runtime\")
                             :runtime))
                     (fset 'commandp
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-command-execute-runtime-delegation\"
                              \"direct-commandp\")
                             t))
                     (fset 'find-file
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-command-execute-runtime-delegation\"
                              \"direct-command\")
                             :direct))
                     (setq files--bridge-command 'find-file)
                     (setq files--bridge-effective-command \"find-file\")
                     (command-execute))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "runtime"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-execute-no-prefix-backend ()
  "Standalone command-execute should treat missing prefix backend as empty."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (make-temp-file "nemacs-command-loop-bridge-" nil ".nlri"))
          (probe-file "/tmp/nemacs-command-execute-no-prefix-backend"))
      (unwind-protect
          (progn
            (with-temp-file image
              (insert ";;; nelisp-runtime-image source-v1\n(progn\n")
              (when (file-readable-p nemacs-gui-file-bridge-runtime-test--prelude)
                (insert-file-contents
                 nemacs-gui-file-bridge-runtime-test--prelude)
                (goto-char (point-max)))
              (insert-file-contents
               (expand-file-name
                "src/emacs-command-loop.el"
                nemacs-gui-file-bridge-runtime-test--repo-root))
              (goto-char (point-max))
              (insert-file-contents nemacs-gui-file-bridge-runtime-test--source)
              (goto-char (point-max))
              (insert "\n)\n"))
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-image
                    reader image
                    "(progn
                     (fset 'nemacs-command-execute-no-prefix-commandp
                           (lambda (command) t))
                     (fset 'nemacs-command-execute-no-prefix-read-only-p
                           (lambda () nil))
                     (fset 'nemacs-command-execute-no-prefix-call-command
                           (lambda (command)
                             (nl-write-file
                              \"/tmp/nemacs-command-execute-no-prefix-backend\"
                              (symbol-name command))))
                     (fset 'nemacs-command-execute-no-prefix-with-prefix
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-command-execute-no-prefix-backend\"
                              \"bad-prefix\")))
                     (emacs-command-loop-gui-register-backend
                      :commandp
                      'nemacs-command-execute-no-prefix-commandp
                      :read-only-p
                      'nemacs-command-execute-no-prefix-read-only-p
                      :call-command
                      'nemacs-command-execute-no-prefix-call-command
                      :execute-with-prefix-arg
                      'nemacs-command-execute-no-prefix-with-prefix)
                     (emacs-command-loop-gui-set-context
                      :command 'forward-char
                      :effective-command \"forward-char\")
                     (emacs-command-loop-gui-command-execute))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "forward-char"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-execute-call-helper-policy ()
  "Bridge source-v1 command-execute call helper should own prefix dispatch."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-execute-call-helper-policy"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'nemacs-helper-direct-command
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-command-execute-call-helper-policy\"
                              (concat (rdf \"/tmp/nemacs-command-execute-call-helper-policy\")
                                      \"direct\\n\"))))
                     (fset 'files--execute-with-prefix-arg
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-command-execute-call-helper-policy\"
                              (concat (rdf \"/tmp/nemacs-command-execute-call-helper-policy\")
                                      \"prefix\\n\"))))
                     (setq files--bridge-command 'nemacs-helper-direct-command)
                     (setq files--bridge-effective-command
                           \"nemacs-helper-direct-command\")
                     (setq files--prefix-arg \"\")
                     (emacs-command-loop-gui-command-execute-call)
                     (setq files--prefix-arg \"4\")
                     (emacs-command-loop-gui-command-execute-call))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "direct\nprefix\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-read-only-command-policy-runtime ()
  "Bridge read-only command wrapper should delegate policy to command-loop."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-read-only-command-policy-runtime"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (fset 'emacs-command-loop-gui-read-only-command-p
                             (lambda (command)
                               (nl-write-file
                                \"/tmp/nemacs-read-only-command-policy-runtime\"
                                (if (symbolp command)
                                    (symbol-name command)
                                  command))
                               'runtime-policy))
                       (setq files--bridge-command 'insert-file)
                       (files--read-only-command-p))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "insert-file"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-run-request-adapter ()
  "Bridge run-request adapter should delegate request semantics to runtime."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-loop-run-request-adapter"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-command-loop-gui-run-request-current-context
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-command-loop-run-request-adapter\"
                              (concat files--bridge-keys
                                      \"\\t\"
                                      files--bridge-arg))
                             :ran))
                     (setq files--bridge-command nil)
                     (setq files--bridge-keys \"C-c r\")
                     (setq files--bridge-arg \"seed\")
                     (files--command-loop-run-request-current-context))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "C-c r\tseed"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-run-request-appends-key-once ()
  "Fallback run-request should leave post-key dispatch to the runtime helper."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-loop-run-request-kmacro-once"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-command-loop-gui-dispatch-key-request-current-context
                           (lambda ()
                             (emacs-command-loop-gui-after-key-dispatch)
                             :key))
                     (setq files--bridge-command nil)
                     (setq files--bridge-effective-command \"\")
                     (setq files--bridge-keys \"a\")
                     (setq files--bridge-arg \"\")
                     (setq files--kmacro-recording t)
                     (setq files--kmacro-replaying nil)
                     (setq files--kmacro-keys \"\")
                     (files--command-loop-run-request-current-context)
                     (nl-write-file
                      \"/tmp/nemacs-command-loop-run-request-kmacro-once\"
                      files--kmacro-keys))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "a\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-kmacro-replay-uses-command-loop ()
  "Bridge kmacro replay should delegate key-line parsing to command-loop runtime."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-kmacro-replay-command-loop")
          (state-file "/tmp/nemacs-kmacro-replay-state"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-command-loop-gui-replay-key-lines
                           (lambda (source dispatch)
                             (nl-write-file
                              \"/tmp/nemacs-kmacro-replay-command-loop\"
                              (concat \"source=\" source \"\\n\"))
                             (funcall dispatch \"C-f\")
                             (funcall dispatch \"a\")
                             2))
                     (fset 'files--dispatch-key-sequence
                           (lambda ()
                             (nl-write-file
                              \"/tmp/nemacs-kmacro-replay-command-loop\"
                              (concat
                               (rdf \"/tmp/nemacs-kmacro-replay-command-loop\")
                               \"dispatch=\" files--bridge-keys
                               \"\\n\"))))
                     (setq files--kmacro-keys \"C-f\\n\\na\\n\")
                     (setq files--bridge-keys \"old-key\")
                     (setq files--bridge-command 'old-command)
                     (setq files--bridge-effective-command \"old-effective\")
                     (setq files--bridge-arg \"old-arg\")
                     (setq files--kmacro-replaying nil)
                     (files--call-last-kbd-macro)
                     (nl-write-file
                      \"/tmp/nemacs-kmacro-replay-state\"
                      (concat files--bridge-keys
                              \"\\t\"
                              (symbol-name files--bridge-command)
                              \"\\t\"
                              files--bridge-effective-command
                              \"\\t\"
                              files--bridge-arg
                              \"\\t\"
                              (if files--kmacro-replaying \"1\" \"0\"))))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "source=C-f\n\na\n\ndispatch=C-f\ndispatch=a\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file)))
            (should (equal "old-key\told-command\told-effective\told-arg\t0"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            state-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))
        (when (file-exists-p state-file)
          (delete-file state-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-save-undo-adapter ()
  "Bridge undo snapshot adapter should delegate policy to command-loop runtime."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-loop-save-undo-adapter"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-command-loop-gui-save-undo-if-needed
                           (lambda (command)
                             (nl-write-file
                              \"/tmp/nemacs-command-loop-save-undo-adapter\"
                              (symbol-name command))
                             :saved))
                     (setq files--bridge-command 'kill-line)
                     (files--command-loop-save-undo-if-needed-current-context))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "kill-line"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-current-context-wrappers ()
  "Bridge wrappers should prefer command-loop current-context helpers."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (call-probe "/tmp/nemacs-call-current-context-wrapper")
          (execute-probe "/tmp/nemacs-execute-current-context-wrapper")
          (mx-probe "/tmp/nemacs-mx-current-context-wrapper")
          (dispatch-probe "/tmp/nemacs-dispatch-current-context-wrapper")
          (ingest-probe "/tmp/nemacs-ingest-request-context-wrapper")
          (finalize-probe "/tmp/nemacs-finalize-status-wrapper")
          (run-dispatch-probe
           "/tmp/nemacs-run-dispatch-current-context-wrapper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-image
                    reader image
                    "(progn
                       (fset 'emacs-command-loop-gui-call-interactively-current-context
                             (lambda ()
                               (nl-write-file
                                \"/tmp/nemacs-call-current-context-wrapper\"
                                (concat (symbol-name files--bridge-command)
                                        \"\\t\"
                                        files--bridge-effective-command))
                               :called))
                       (fset 'emacs-command-loop-gui-command-execute-current-context
                             (lambda ()
                              (nl-write-file
                               \"/tmp/nemacs-execute-current-context-wrapper\"
                               (concat (symbol-name files--bridge-command)
                                       \"\\t\"
                                       files--bridge-effective-command))
                               :executed))
                       (fset 'emacs-command-loop-gui-execute-extended-command-current-context
                             (lambda ()
                               (nl-write-file
                                \"/tmp/nemacs-mx-current-context-wrapper\"
                                (concat files--bridge-arg
                                        \"\\t\"
                                        files--bridge-minibuffer-arg))
                               :mx))
                       (fset 'emacs-command-loop-gui-ingest-request-context
                             (lambda (&rest _plist)
                               (nl-write-file
                                \"/tmp/nemacs-ingest-request-context-wrapper\"
                                (concat files--bridge-keys
                                        \"\\t\"
                                        files--bridge-arg
                                        \"\\t\"
                                        files--prefix-arg))
                               (if (equal files--bridge-keys \"\")
                                   nil
                                 (progn
                                   (setq files--bridge-command nil)
                                   (setq files--bridge-effective-command
                                         \"\")))
                               (setq files--bridge-status \"ok\")
                               :ingested))
                       (fset 'emacs-command-loop-gui-finalize-status-current-context
                             (lambda ()
                               (nl-write-file
                                \"/tmp/nemacs-finalize-status-wrapper\"
                                (concat files--bridge-effective-command
                                        \"\\t\"
                                        files--bridge-status))
                               'normal))
                       (fset 'emacs-command-loop-gui-write-post-command-state
                             (lambda (&optional command effective-command status)
                               (nl-write-file
                                \"/tmp/nemacs-finalize-status-wrapper\"
                                (concat files--bridge-effective-command
                                        \"\\t\"
                                        files--bridge-status))
                               (list :command-name
                                     (if effective-command
                                         effective-command
                                       \"\")
                                     :lane 'normal)))
                       (fset 'emacs-command-loop-gui-dispatch-current-context
                             (lambda ()
                               (if (equal files--bridge-keys \"C-c p\")
                                   (nl-write-file
                                    \"/tmp/nemacs-dispatch-current-context-wrapper\"
                                    (concat files--bridge-keys
                                            \"\\t\"
                                            files--bridge-arg))
                                 nil)
                               (nl-write-file
                                \"/tmp/nemacs-run-dispatch-current-context-wrapper\"
                                (concat files--bridge-keys
                                        \"\\t\"
                                       files--bridge-arg))
                               :dispatched))
                       (fset 'emacs-command-loop-gui-dispatch-key-request-current-context
                             (lambda ()
                               (if (equal files--bridge-keys \"C-c p\")
                                   (nl-write-file
                                    \"/tmp/nemacs-dispatch-current-context-wrapper\"
                                    (concat files--bridge-keys
                                            \"\\t\"
                                            files--bridge-arg))
                                 nil)
                               (nl-write-file
                                \"/tmp/nemacs-run-dispatch-current-context-wrapper\"
                                (concat files--bridge-keys
                                        \"\\t\"
                                        files--bridge-arg))
                               :key-request))
                       (fset 'emacs-command-loop-gui-run-request-current-context
                             (lambda ()
                               (nl-write-file
                                \"/tmp/nemacs-run-dispatch-current-context-wrapper\"
                                (concat files--bridge-keys
                                        \"\\t\"
                                        files--bridge-arg))
                               :ran))
                       (setq files--bridge-command 'current-context-probe)
                       (setq files--bridge-effective-command
                             \"current-context-probe\")
                       (setq files--bridge-keys \"C-c p\")
                       (setq files--bridge-arg \"seed\")
                       (call-interactively)
                       (command-execute)
                       (files--dispatch-key-sequence)
                       (setq files--bridge-command
                             'execute-extended-command)
                       (setq files--bridge-effective-command
                             \"execute-extended-command\")
                       (setq files--bridge-arg \"goto-line\")
                       (setq files--bridge-minibuffer-arg \"17\")
                       (execute-extended-command)
                       (nl-write-file \"/tmp/nemacs-cmd\" \"\")
                       (nl-write-file \"/tmp/nemacs-keys\" \"C-c r\")
                       (nl-write-file \"/tmp/nemacs-arg\" \"seed-run\")
                       (nl-write-file \"/tmp/nemacs-buf\" \"\")
                       (nl-write-file \"/tmp/nemacs-file\" \"\")
                       (nl-write-file \"/tmp/nemacs-buffer-name\" \"main\")
                       (nl-write-file \"/tmp/nemacs-read-only\" \"0\")
                       (nl-write-file \"/tmp/nemacs-point\" \"0\")
                       (nl-write-file \"/tmp/nemacs-mark\" \"0\")
                       (nemacs-gui-file-bridge-run))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "current-context-probe\tcurrent-context-probe"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              call-probe)))
              (should (equal "current-context-probe\tcurrent-context-probe"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              execute-probe)))
              (should (equal "goto-line\t17"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              mx-probe)))
              (should (equal "C-c p\tseed"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              dispatch-probe)))
              (should (equal "C-c r\tseed-run\t"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              ingest-probe)))
              (should (equal "\tok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              finalize-probe)))
              (should (equal "C-c r\tseed-run"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              run-dispatch-probe)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p call-probe)
          (delete-file call-probe))
        (when (file-exists-p execute-probe)
          (delete-file execute-probe))
        (when (file-exists-p mx-probe)
          (delete-file mx-probe))
        (when (file-exists-p dispatch-probe)
          (delete-file dispatch-probe))
        (when (file-exists-p ingest-probe)
          (delete-file ingest-probe))
        (when (file-exists-p finalize-probe)
          (delete-file finalize-probe))
        (when (file-exists-p run-dispatch-probe)
          (delete-file run-dispatch-probe))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-command-arg-wrappers ()
  "Bridge call/execute wrappers should accept explicit COMMAND args."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (call-probe "/tmp/nemacs-call-command-arg-wrapper")
          (execute-probe "/tmp/nemacs-execute-command-arg-wrapper"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (fset 'emacs-command-loop-gui-call-interactively
                           (lambda (command)
                             (nl-write-file
                              \"/tmp/nemacs-call-command-arg-wrapper\"
                              (concat (symbol-name command)
                                      \"\\t\"
                                      files--bridge-effective-command))
                             :called))
                     (fset 'emacs-command-loop-gui-command-execute
                           (lambda (command)
                             (nl-write-file
                              \"/tmp/nemacs-execute-command-arg-wrapper\"
                              (concat (symbol-name command)
                                      \"\\t\"
                                      files--bridge-effective-command))
                             :executed))
                     (setq files--bridge-command nil)
                     (setq files--bridge-effective-command \"\")
                     (call-interactively 'forward-char)
                     (command-execute 'backward-char))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "forward-char\tforward-char"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            call-probe)))
            (should (equal "backward-char\tbackward-char"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            execute-probe))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p call-probe)
          (delete-file call-probe))
        (when (file-exists-p execute-probe)
          (delete-file execute-probe))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-wrappers-ensure-backend ()
  "Bridge call/execute wrappers should install the command-loop backend first."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-loop-wrapper-ensure-backend"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (setq nemacs-command-loop-wrapper-ensure-log \"\")
                     (fset 'files--command-loop-install-backend
                           (lambda ()
                             (setq nemacs-command-loop-wrapper-ensure-log
                                   (concat
                                    nemacs-command-loop-wrapper-ensure-log
                                    \"install:\"
                                    files--bridge-effective-command
                                    \"\\n\"))))
                     (fset 'emacs-command-loop-gui-call-interactively-current-context
                           (lambda ()
                             (setq nemacs-command-loop-wrapper-ensure-log
                                   (concat
                                    nemacs-command-loop-wrapper-ensure-log
                                    \"call:\"
                                    files--bridge-effective-command
                                    \"\\n\"))
                             :called))
                     (fset 'emacs-command-loop-gui-command-execute-current-context
                           (lambda ()
                             (setq nemacs-command-loop-wrapper-ensure-log
                                   (concat
                                    nemacs-command-loop-wrapper-ensure-log
                                    \"execute:\"
                                    files--bridge-effective-command
                                    \"\\n\"))
                             :executed))
                     (setq files--bridge-command 'forward-char)
                     (setq files--bridge-effective-command \"forward-char\")
                     (call-interactively)
                     (setq files--bridge-command 'backward-char)
                     (setq files--bridge-effective-command \"backward-char\")
                     (command-execute)
                     (nl-write-file
                      \"/tmp/nemacs-command-loop-wrapper-ensure-backend\"
                      nemacs-command-loop-wrapper-ensure-log))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "install:forward-char\ncall:forward-char\ninstall:backward-char\nexecute:backward-char\n"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-writeback-command-name ()
  "Bridge runtime image should expose command-loop writeback normalization."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-writeback-command-name"))
      (unwind-protect
          (let ((result
                 (nemacs-gui-file-bridge-runtime-test--run-image
                  reader image
                  "(progn
                     (nl-write-file
                      \"/tmp/nemacs-writeback-command-name\"
                      (concat
                       (emacs-command-loop-gui-writeback-command-name
                        'project-query-replace-regexp \"minibuffer\")
                       \"\\t\"
                       (emacs-command-loop-gui-writeback-command-name
                        nil 'self-insert-command)
                       \"\\t\"
                       (emacs-command-loop-gui-writeback-command-name
                        'save-buffer nil)))))")))
            (should (equal 0 (plist-get result :status)))
            (should (equal "project-query-replace-regexp\tself-insert-command\tsave-buffer"
                           (nemacs-gui-file-bridge-runtime-test--slurp
                            probe-file))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-post-command-state ()
  "Bridge runtime image should flush post-command state through command-loop policy."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-post-command-state"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-image
                    reader image
                    "(progn
                       (setq files--bridge-command
                             'project-query-replace-regexp)
                       (setq files--bridge-effective-command
                             \"minibuffer\")
                       (setq files--bridge-status \"prefix-arg\")
                       (setq files--prefix-arg \"C-u\")
                       (setq files--kmacro-recording t)
                       (setq files--kmacro-keys \"C-x\\n\")
                       (setq files--buffer-string \"abcdef\\n\")
                       (setq files--point 3)
                       (setq files--mark 1)
                       (setq files--window-start 0)
                       (let ((state
                              (emacs-command-loop-gui-write-post-command-state
                               files--bridge-command
                               files--bridge-effective-command
                               files--bridge-status)))
                         (nl-write-file
                          \"/tmp/nemacs-post-command-state\"
                          (concat
                           (plist-get state :command-name)
                           \"\\t\"
                           (symbol-name (plist-get state :lane))))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "project-query-replace-regexp\tprefix-arg"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "C-u"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-prefix-arg")))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-kmacro-recording")))
              (should (equal "C-x\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-kmacro-keys")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-lane-writeback ()
  "Bridge runtime image should apply command-loop lane writeback specs."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-loop-lane-writeback"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-image
                    reader image
                    "(progn
                       (setq files--bridge-status \"read-only\")
                       (setq files--bridge-writeback-lane \"read-only\")
                       (setq files--buffer-string \"abc\")
                       (setq files--point 2)
                       (setq files--mark 1)
                       (setq files--window-start 0)
                       (files--command-loop-writeback-current-lane)
                       (let ((read-only-status (rdf \"/tmp/nemacs-status\"))
                             (read-only-buf (rdf \"/tmp/nemacs-buf\"))
                             (read-only-flag (rdf \"/tmp/nemacs-read-only\")))
                         (setq files--bridge-status \"prefix-arg\")
                         (setq files--bridge-writeback-lane \"prefix-arg\")
                         (setq files--prefix-arg \"C-u\")
                         (setq files--point 3)
                         (setq files--mark 2)
                         (files--command-loop-writeback-current-lane)
                         (nl-write-file
                          \"/tmp/nemacs-command-loop-lane-writeback\"
                          (concat
                           read-only-status
                           \"\\t\"
                           read-only-buf
                           \"\\t\"
                           read-only-flag
                           \"\\t\"
                           (rdf \"/tmp/nemacs-status\")
                           \"\\t\"
                           (rdf \"/tmp/nemacs-prefix-arg\")))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "read-only\tabc\t1\tprefix-arg\tC-u"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-command-loop-lane-writeback-uses-command-loop-flag ()
  "Command-loop lane writeback should not depend on fileio spec helpers."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-command-loop-lane-flag-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-image
                    reader image
                    "(progn
                       (fset 'files--fileio-writeback-spec-flag
                             (lambda (_spec _key)
                               (error \"fileio flag helper used\")))
                       (setq files--bridge-status \"prefix-arg\")
                       (setq files--bridge-writeback-lane \"prefix-arg\")
                       (setq files--prefix-arg \"C-u\")
                       (files--command-loop-writeback-current-lane)
                       (nl-write-file
                        \"/tmp/nemacs-command-loop-lane-flag-helper\"
                        (rdf \"/tmp/nemacs-prefix-arg\")))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "C-u"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-fileio-current-context-sync ()
  "Bridge fileio sync should prefer the runtime current-context helper."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-fileio-current-context-sync"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-image
                    reader image
                    "(progn
                       (fset 'emacs-fileio-gui-refresh-context-from-backend
                             (lambda ()
                               (nl-write-file
                                \"/tmp/nemacs-fileio-current-context-sync\"
                                (concat files--bridge-arg
                                        \"\\t\"
                                        files--bridge-status
                                        \"\\t\"
                                        files--buffer-name
                                        \"\\t\"
                                        (if files--buffer-read-only-p
                                            \"1\"
                                          \"0\")))
                               :synced))
                       (setq files--bridge-arg \"/tmp/context.txt\")
                       (setq files--bridge-status \"pending\")
                       (setq files--buffer-name \"notes\")
                       (setq files--buffer-read-only-p t)
                       (files--fileio-sync-context))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "/tmp/context.txt\tpending\tnotes\t1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-fileio-current-context-command-wrappers ()
  "Bridge file/buffer wrappers should prefer current-context commands."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-fileio-current-context-command-wrappers"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-image
                    reader image
                    "(progn
                       (setq files--fileio-current-context-wrapper-log \"\")
                       (fset 'files--fileio-wrapper-log
                             (lambda (entry)
                               (setq files--fileio-current-context-wrapper-log
                                     (concat files--fileio-current-context-wrapper-log
                                             entry
                                             \"\\n\"))))
                       (fset 'emacs-fileio-gui-current-context-command
                             (lambda (command &optional action read-only)
                               (let ((result nil))
                                 (if (eq command 'find-file)
                                     (progn
                                       (files--fileio-wrapper-log
                                        (concat \"find:\"
                                                action
                                                \":\"
                                                (if read-only \"ro\" \"rw\")))
                                       (setq result \"/tmp/current.txt\"))
                                   nil)
                                 (if (eq command 'find-alternate-file)
                                     (progn
                                       (files--fileio-wrapper-log \"alternate\")
                                       (setq result \"/tmp/alternate.txt\"))
                                   nil)
                                 (if (eq command 'project-find-file)
                                     (progn
                                       (files--fileio-wrapper-log
                                        \"project-find\")
                                       (setq result \"/tmp/project.txt\"))
                                   nil)
                                 (if (eq command 'project-or-external-find-file)
                                     (progn
                                       (files--fileio-wrapper-log
                                        \"project-or-external\")
                                       (setq result \"/tmp/project-or-external.txt\"))
                                   nil)
                                 (if (eq command 'save-buffer)
                                     (progn
                                       (files--fileio-wrapper-log \"save\")
                                       (setq result \"/tmp/current.txt\"))
                                   nil)
                                 (if (eq command 'save-some-buffers)
                                     (progn
                                       (files--fileio-wrapper-log \"save-some\")
                                       (setq result t))
                                   nil)
                                 (if (eq command 'write-file)
                                     (progn
                                       (files--fileio-wrapper-log \"write\")
                                       (setq result \"/tmp/write.txt\"))
                                   nil)
                                 (if (eq command 'insert-file)
                                     (progn
                                       (files--fileio-wrapper-log \"insert-file\")
                                       (setq result files--bridge-arg))
                                   nil)
                                 (if (eq command 'insert-buffer)
                                     (progn
                                       (files--fileio-wrapper-log \"insert-buffer\")
                                       (setq result files--bridge-arg))
                                   nil)
                                 (if (eq command 'revert-buffer)
                                     (progn
                                       (files--fileio-wrapper-log \"revert\")
                                       (setq result files--current-file-name))
                                   nil)
                                 (if (eq command 'switch-to-buffer)
                                     (progn
                                       (files--fileio-wrapper-log
                                        (concat \"switch:\" action))
                                       (setq result \"main\"))
                                   nil)
                                 (if (eq command 'display-buffer)
                                     (progn
                                       (files--fileio-wrapper-log
                                        (concat \"display:\" action))
                                       (setq result \"main\"))
                                   nil)
                                 (if (eq command 'rename-buffer)
                                     (progn
                                       (files--fileio-wrapper-log \"rename\")
                                       (setq result files--bridge-arg))
                                   nil)
                                 (if (eq command 'kill-buffer)
                                     (progn
                                       (files--fileio-wrapper-log \"kill\")
                                       (setq result \"main\"))
                                   nil)
                                 (if (eq command 'kill-buffer-and-window)
                                     (progn
                                       (files--fileio-wrapper-log \"kill-window\")
                                       (setq result \"main\"))
                                   nil)
                                 (if (eq command 'list-buffers)
                                     (progn
                                       (files--fileio-wrapper-log \"list\")
                                       (setq result \"*Buffer List*\"))
                                   nil)
                                 (if (eq command 'project-list-buffers)
                                     (progn
                                       (files--fileio-wrapper-log \"project-list\")
                                       (setq result \"*Buffer List*\"))
                                   nil)
                                 (if (eq command 'project-kill-buffers)
                                     (progn
                                       (files--fileio-wrapper-log \"project-kill\")
                                       (setq result \"main\"))
                                   nil)
                                 result)))
                       (find-file)
                       (find-alternate-file)
                       (project-find-file)
                       (project-or-external-find-file)
                       (save-buffer)
                       (save-some-buffers)
                       (write-file)
                       (insert-file)
                       (insert-buffer)
                       (revert-buffer)
                       (switch-to-buffer)
                       (display-buffer)
                       (rename-buffer)
                       (kill-buffer)
                       (kill-buffer-and-window)
                       (list-buffers)
                       (project-list-buffers)
                       (project-kill-buffers)
                       (nl-write-file
                        \"/tmp/nemacs-fileio-current-context-command-wrappers\"
                        files--fileio-current-context-wrapper-log))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "find:same:rw\nalternate\nproject-find\nproject-or-external\nsave\nsave-some\nwrite\ninsert-file\ninsert-buffer\nrevert\nswitch:same\ndisplay:other\nrename\nkill\nkill-window\nlist\nproject-list\nproject-kill\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-fileio-writeback-spec ()
  "Bridge file/buffer writeback should be driven by runtime specs."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-fileio-writeback-spec-result"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string \"buffer text\")
                       (setq files--current-file-name \"/tmp/current.txt\")
                       (setq files--buffer-name \"notes\")
                       (setq files--buffer-read-only-p t)
                       (setq files--window-layout \"single\")
                       (setq files--window-selected \"0\")
                       (setq files--point 5)
                       (setq files--mark 2)
                       (setq files--window-start 1)
                       (setq files--bridge-status \"ok\")
                       (files--fileio-writeback-current-context
                        \"switch-to-buffer-other-frame\")
                       (nl-write-file
                        \"/tmp/nemacs-fileio-writeback-spec-result\"
                        files--bridge-status))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "written"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "buffer text"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "/tmp/current.txt"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "notes"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "single"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "00005"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00002"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00001"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-fileio-writeback-uses-runtime-flag ()
  "File/buffer writeback should consume the fileio runtime flag helper."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-fileio-writeback-runtime-flag"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (fset 'files--fileio-writeback-spec-flag
                             (lambda (_spec _key)
                               (error \"legacy fileio flag helper used\")))
                       (setq files--buffer-string \"buffer text\")
                       (setq files--current-file-name \"/tmp/current.txt\")
                       (setq files--buffer-name \"notes\")
                       (setq files--buffer-read-only-p t)
                       (setq files--window-layout \"single\")
                       (setq files--window-selected \"0\")
                       (setq files--point 5)
                       (setq files--mark 2)
                       (setq files--window-start 1)
                       (setq files--bridge-status \"ok\")
                       (files--fileio-writeback-current-context
                        \"switch-to-buffer-other-frame\")
                       (nl-write-file
                        \"/tmp/nemacs-fileio-writeback-runtime-flag\"
                        files--bridge-status))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "written"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-fileio-writeback-uses-runtime-state-helper ()
  "File/buffer writeback should delegate to the runtime state helper."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-fileio-writeback-state-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (fset 'emacs-fileio-gui-writeback-state
                             (lambda (command status)
                               (nl-write-file
                                \"/tmp/nemacs-fileio-writeback-state-helper\"
                                (concat
                                 (if (symbolp command)
                                     (symbol-name command)
                                   command)
                                 \"\\t\"
                                 status))
                               t))
                       (fset 'emacs-fileio-gui-writeback-spec-flag
                             (lambda (_spec _key)
                               (error \"legacy fileio flag helper used\")))
                       (setq files--bridge-status \"ok\")
                       (files--fileio-writeback-current-context
                        \"save-buffer\"))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "save-buffer\tok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-dired-writeback-spec ()
  "Bridge Dired writeback should be driven by runtime specs."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-dired-writeback-spec-result"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string \"Directory /tmp\\n  a.txt\\n\")
                       (setq files--current-file-name \"\")
                       (setq files--buffer-name \"*Directory*\")
                       (setq files--window-layout \"single\")
                       (setq files--window-selected \"0\")
                       (setq files--point 4)
                       (setq files--mark 3)
                       (setq files--window-start 2)
                       (setq files--bridge-status \"ok\")
                       (files--dired-writeback-current-context
                        \"dired-other-tab\")
                       (setq files--buffer-string \"Directory /tmp\\n\")
                       (setq files--modeline-string \"Dired: /tmp\")
                       (setq files--point 7)
                       (setq files--mark 6)
                       (setq files--window-start 5)
                       (files--dired-writeback-current-context
                        \"dired-do-flagged-delete\")
                       (nl-write-file
                        \"/tmp/nemacs-dired-writeback-spec-result\"
                        files--bridge-status))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "written"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "Directory /tmp\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "*Directory*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "Dired: /tmp"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-modeline")))
              (should (equal "single"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "00007"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00006"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00005"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-info-writeback-spec ()
  "Bridge Info writeback should be driven by runtime specs."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-info-writeback-spec-result"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string \"Info body\\n\")
                       (setq files--current-file-name \"/tmp/manual.info\")
                       (setq files--buffer-name \"*info*\")
                       (setq files--buffer-read-only-p t)
                       (setq files--window-layout \"info-window\")
                       (setq files--window-selected \"1\")
                       (setq files--point 8)
                       (setq files--mark 4)
                       (setq files--window-start 2)
                       (setq files--bridge-status \"ok\")
                       (files--info-writeback-current-context
                        \"Info-next\")
                       (nl-write-file
                        \"/tmp/nemacs-info-writeback-spec-result\"
                        files--bridge-status))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "written"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "Info body\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "/tmp/manual.info"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "*info*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-read-only")))
              (should (equal "info-window"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (should (equal "00008"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00004"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00002"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-help-writeback-spec ()
  "Bridge Help writeback should be driven by runtime specs."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-help-writeback-spec-result"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string \"Help body\\n\")
                       (setq files--current-file-name \"\")
                       (setq files--buffer-name \"*Help*\")
                       (setq files--buffer-read-only-p t)
                       (setq files--window-layout \"help-window\")
                       (setq files--window-selected \"2\")
                       (setq files--point 9)
                       (setq files--mark 5)
                       (setq files--window-start 3)
                       (setq files--bridge-status \"ok\")
                       (files--help-writeback-current-context
                        \"describe-function\")
                       (setq files--buffer-string \"About body\\n\")
                       (setq files--point 11)
                       (setq files--mark 7)
                       (setq files--window-start 4)
                       (files--help-writeback-current-context
                        \"about-emacs\")
                       (nl-write-file
                        \"/tmp/nemacs-help-writeback-spec-result\"
                        files--bridge-status))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "written"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "About body\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal ""
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "*Help*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-read-only")))
              (should (equal "help-window"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "2"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (should (equal "00011"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00007"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00004"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-family-writeback-uses-family-flags ()
  "Dired/Info/Help writeback should not depend on fileio spec helpers."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-family-writeback-flag-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (fset 'files--fileio-writeback-spec-flag
                             (lambda (_spec _key)
                               (error \"fileio flag helper used\")))
                       (setq files--buffer-string \"Dired body\\n\")
                       (setq files--buffer-name \"*Directory*\")
                       (setq files--modeline-string \"Dired ok\")
                       (setq files--bridge-status \"ok\")
                       (files--dired-writeback-current-context
                        \"dired-do-flagged-delete\")
                       (let ((dired-status files--bridge-status))
                         (setq files--buffer-string \"Info body\\n\")
                         (setq files--buffer-name \"*info*\")
                         (setq files--buffer-read-only-p t)
                         (setq files--bridge-status \"ok\")
                         (files--info-writeback-current-context
                          \"Info-next\")
                         (let ((info-status files--bridge-status))
                           (setq files--buffer-string \"Help body\\n\")
                           (setq files--buffer-name \"*Help*\")
                           (setq files--buffer-read-only-p t)
                           (setq files--bridge-status \"ok\")
                           (files--help-writeback-current-context
                            \"about-emacs\")
                           (nl-write-file
                            \"/tmp/nemacs-family-writeback-flag-helper\"
                            (concat dired-status
                                    \"\\t\"
                                    info-status
                                    \"\\t\"
                                    files--bridge-status)))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "written\twritten\twritten"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-family-writeback-uses-runtime-state-helper ()
  "Dired/Info/Help writeback should delegate to family runtime helpers."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-family-writeback-state-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--family-writeback-helper-log \"\")
                       (fset 'append-family-log
                             (lambda (family command)
                               (setq files--family-writeback-helper-log
                                     (concat files--family-writeback-helper-log
                                             family
                                             \":\"
                                             (if (symbolp command)
                                                 (symbol-name command)
                                               command)
                                             \"\\n\"))
                               (setq files--bridge-status \"written\")
                               t))
                       (fset 'emacs-dired-min-gui-writeback-state
                             (lambda (command)
                               (append-family-log \"dired\" command)))
                       (fset 'emacs-info-gui-writeback-state
                             (lambda (command)
                               (append-family-log \"info\" command)))
                       (fset 'emacs-help-gui-writeback-state
                             (lambda (command)
                               (append-family-log \"help\" command)))
                       (fset 'emacs-dired-min-gui-writeback-spec-flag
                             (lambda (_spec _key)
                               (error \"legacy dired flag helper used\")))
                       (fset 'emacs-info-gui-writeback-spec-flag
                             (lambda (_spec _key)
                               (error \"legacy info flag helper used\")))
                       (fset 'emacs-help-gui-writeback-spec-flag
                             (lambda (_spec _key)
                               (error \"legacy help flag helper used\")))
                       (setq files--bridge-status \"ok\")
                       (files--dired-writeback-current-context
                        \"dired-do-flagged-delete\")
                       (setq files--bridge-status \"ok\")
                       (files--info-writeback-current-context \"Info-next\")
                       (setq files--bridge-status \"ok\")
                       (files--help-writeback-current-context \"about-emacs\")
                       (nl-write-file
                        \"/tmp/nemacs-family-writeback-state-helper\"
                        files--family-writeback-helper-log))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "dired:dired-do-flagged-delete\ninfo:Info-next\nhelp:about-emacs\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-family-writeback-helper ()
  "Bridge family writeback helper should clear handled commands only."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-family-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (fset 'files--fileio-writeback-current-context
                             (lambda (command)
                               (if (equal command \"switch-to-buffer\")
                                   t
                                 nil)))
                       (fset 'files--dired-writeback-current-context
                             (lambda (command)
                               (if (equal command \"dired-do-copy\")
                                   t
                                 nil)))
                       (fset 'files--info-writeback-current-context
                             (lambda (command)
                               (if (equal command \"Info-next\")
                                   t
                                 nil)))
                       (fset 'files--help-writeback-current-context
                             (lambda (command)
                               (if (equal command \"about-emacs\")
                                   t
                                 nil)))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-family-writeback-helper\"
                        (concat
                         (files--bridge-family-writeback-current-context
                          \"switch-to-buffer\")
                         \"\\t\"
                         (files--bridge-family-writeback-current-context
                          \"dired-do-copy\")
                         \"\\t\"
                         (files--bridge-family-writeback-current-context
                          \"Info-next\")
                         \"\\t\"
                         (files--bridge-family-writeback-current-context
                          \"about-emacs\")
                         \"\\t\"
                         (files--bridge-family-writeback-current-context
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "\t\t\t\tforward-char"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-prefix-writeback-helper ()
  "Bridge prefix writeback helper should write prefix state only."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-prefix-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string \"0123456789abcdef\")
                       (setq files--window-layout \"prefix-layout\")
                       (setq files--window-selected \"3\")
                       (setq files--point 12)
                       (setq files--bridge-status \"ok\")
                       (let ((same
                              (files--bridge-prefix-writeback-current-context
                               \"same-window-prefix\"))
                             (same-status files--bridge-status))
                         (setq files--bridge-status \"ok\")
                         (let ((other
                                (files--bridge-prefix-writeback-current-context
                                 \"other-window-prefix\"))
                               (other-status files--bridge-status))
                           (setq files--bridge-status \"ok\")
                           (let ((tab
                                  (files--bridge-prefix-writeback-current-context
                                   \"other-tab-prefix\"))
                                 (tab-status files--bridge-status))
                             (setq files--bridge-status \"ok\")
                             (let ((frame
                                    (files--bridge-prefix-writeback-current-context
                                     \"other-frame-prefix\"))
                                   (frame-status files--bridge-status))
                               (setq files--bridge-status \"ok\")
                               (let ((miss
                                      (files--bridge-prefix-writeback-current-context
                                       \"forward-char\"))
                                     (miss-status files--bridge-status))
                                 (nl-write-file
                                  \"/tmp/nemacs-bridge-prefix-writeback-helper\"
                                  (concat same
                                          \":\"
                                          same-status
                                          \"\\t\"
                                          other
                                          \":\"
                                          other-status
                                          \"\\t\"
                                          tab
                                          \":\"
                                          tab-status
                                          \"\\t\"
                                          frame
                                          \":\"
                                          frame-status
                                          \"\\t\"
                                          miss
                                          \":\"
                                          miss-status))))))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "same-window-prefix:written\tother-window-prefix:written\tother-tab-prefix:written\tother-frame-prefix:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "prefix-layout"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "3"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (should (equal "00012"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-project-writeback-helper ()
  "Bridge project writeback helper should write project command state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-project-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string
                             \"Project body\\n0123456789abcdef\")
                       (setq files--current-file-name
                             \"/tmp/nemacs-project-file.txt\")
                       (setq files--buffer-name \"ProjectBuffer\")
                       (setq files--buffer-read-only-p t)
                       (setq files--window-layout \"project-layout\")
                       (setq files--window-selected \"4\")
                       (setq files--point 13)
                       (setq files--mark 4)
                       (setq files--window-start 2)
                       (fset 'capture-project-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-project-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-project-writeback-helper\"
                        (concat
                         (capture-project-writeback \"project-find-file\")
                         \"\\t\"
                         (capture-project-writeback
                          \"project-or-external-find-file\")
                         \"\\t\"
                         (capture-project-writeback \"project-find-dir\")
                         \"\\t\"
                         (capture-project-writeback \"project-dired\")
                         \"\\t\"
                         (capture-project-writeback \"project-switch-project\")
                         \"\\t\"
                         (capture-project-writeback \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "project-find-file:written\tproject-or-external-find-file:written\tproject-find-dir:written\tproject-dired:written\tproject-switch-project:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "Project body\n0123456789abcdef"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "/tmp/nemacs-project-file.txt"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "ProjectBuffer"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-read-only")))
              (should (equal "project-layout"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "4"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (should (equal "00013"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00004"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00002"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-find-file-writeback-helper ()
  "Bridge find-file writeback helper should write file command state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-find-file-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string
                             \"Find body\\n0123456789abcdef\")
                       (setq files--current-file-name
                             \"/tmp/nemacs-find-file.txt\")
                       (setq files--buffer-read-only-p t)
                       (setq files--window-layout \"find-layout\")
                       (setq files--window-selected \"5\")
                       (setq files--point 11)
                       (fset 'capture-find-file-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-find-file-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-find-file-writeback-helper\"
                        (concat
                         (capture-find-file-writeback \"find-file\")
                         \"\\t\"
                         (capture-find-file-writeback
                          \"find-file-other-window\")
                         \"\\t\"
                         (capture-find-file-writeback
                          \"find-file-other-frame\")
                         \"\\t\"
                         (capture-find-file-writeback
                          \"find-file-other-tab\")
                         \"\\t\"
                         (capture-find-file-writeback
                          \"find-file-read-only\")
                         \"\\t\"
                         (capture-find-file-writeback
                          \"find-file-read-only-other-window\")
                         \"\\t\"
                         (capture-find-file-writeback
                          \"find-file-read-only-other-frame\")
                         \"\\t\"
                         (capture-find-file-writeback
                          \"find-file-read-only-other-tab\")
                         \"\\t\"
                         (capture-find-file-writeback
                          \"find-alternate-file\")
                         \"\\t\"
                         (capture-find-file-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "find-file:written\tfind-file-other-window:written\tfind-file-other-frame:written\tfind-file-other-tab:written\tfind-file-read-only:written\tfind-file-read-only-other-window:written\tfind-file-read-only-other-frame:written\tfind-file-read-only-other-tab:written\tfind-alternate-file:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "Find body\n0123456789abcdef"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "/tmp/nemacs-find-file.txt"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-read-only")))
              (should (equal "find-layout"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "5"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (should (equal "00011"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-read-only-writeback-helper ()
  "Bridge read-only writeback helper should write read-only state only."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-read-only-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (fset 'capture-read-only-writeback
                             (lambda (command flag)
                               (setq files--buffer-read-only-p flag)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-read-only-writeback-current-context
                                       command))
                                     (state
                                      (progn
                                        (setq files--transport-name
                                              \"nemacs-read-only\")
                                        (files--transport-read-current))))
                                 (concat returned
                                         \":\"
                                         files--bridge-status
                                         \":\"
                                         state))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-read-only-writeback-helper\"
                        (concat
                         (capture-read-only-writeback
                          \"toggle-read-only\" t)
                         \"\\t\"
                         (capture-read-only-writeback
                          \"read-only-mode\" nil)
                         \"\\t\"
                         (capture-read-only-writeback
                          \"forward-char\" t))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "toggle-read-only:written:1\tread-only-mode:written:0\tforward-char:ok:0"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-buffer-switch-writeback-helper ()
  "Bridge buffer switch writeback helper should write buffer/window state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-buffer-switch-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string
                             \"Switch body\\n0123456789abcdef\")
                       (setq files--current-file-name
                             \"/tmp/nemacs-switch-buffer.txt\")
                       (setq files--buffer-name \"SwitchBuffer\")
                       (setq files--window-layout \"switch-layout\")
                       (setq files--window-selected \"6\")
                       (setq files--point 12)
                       (setq files--mark 3)
                       (setq files--window-start 2)
                       (fset 'capture-buffer-switch-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-buffer-switch-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-buffer-switch-writeback-helper\"
                        (concat
                         (capture-buffer-switch-writeback
                          \"switch-to-buffer\")
                         \"\\t\"
                         (capture-buffer-switch-writeback
                          \"project-switch-to-buffer\")
                         \"\\t\"
                         (capture-buffer-switch-writeback
                          \"switch-to-buffer-other-window\")
                         \"\\t\"
                         (capture-buffer-switch-writeback
                          \"switch-to-buffer-other-frame\")
                         \"\\t\"
                         (capture-buffer-switch-writeback
                          \"switch-to-buffer-other-tab\")
                         \"\\t\"
                         (capture-buffer-switch-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "switch-to-buffer:written\tproject-switch-to-buffer:written\tswitch-to-buffer-other-window:written\tswitch-to-buffer-other-frame:written\tswitch-to-buffer-other-tab:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "Switch body\n0123456789abcdef"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "/tmp/nemacs-switch-buffer.txt"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "SwitchBuffer"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "switch-layout"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "6"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (should (equal "00012"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00003"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00002"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-display-buffer-writeback-helper ()
  "Bridge display-buffer writeback helper should write display state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-display-buffer-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string
                             \"Display body\\n0123456789abcdef\")
                       (setq files--current-file-name
                             \"/tmp/nemacs-display-buffer.txt\")
                       (setq files--buffer-name \"DisplayBuffer\")
                       (setq files--window-layout \"display-layout\")
                       (setq files--window-selected \"7\")
                       (setq files--point 13)
                       (setq files--mark 4)
                       (setq files--window-start 2)
                       (fset 'capture-display-buffer-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-display-buffer-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-display-buffer-writeback-helper\"
                        (concat
                         (capture-display-buffer-writeback
                          \"display-buffer\")
                         \"\\t\"
                         (capture-display-buffer-writeback
                          \"display-buffer-other-frame\")
                         \"\\t\"
                         (capture-display-buffer-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "display-buffer:written\tdisplay-buffer-other-frame:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "Display body\n0123456789abcdef"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "/tmp/nemacs-display-buffer.txt"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "DisplayBuffer"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "display-layout"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "7"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (should (equal "00013"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00004"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00002"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-register-writeback-helper ()
  "Bridge register writeback helper should write register state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-register-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string
                             \"Register body\\n0123456789abcdef\")
                       (setq files--current-file-name
                             \"/tmp/nemacs-register-buffer.txt\")
                       (setq files--buffer-name \"RegisterBuffer\")
                       (setq files--window-layout \"register-layout\")
                       (setq files--window-selected \"8\")
                       (setq files--window-split-delta 4)
                       (setq files--frame-selected-index 1)
                       (setq files--frame-count 3)
                       (setq files--frame-selected-name \"FrameB\")
                       (setq files--point 14)
                       (setq files--mark 5)
                       (setq files--window-start 3)
                       (fset 'capture-register-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-register-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-register-writeback-helper\"
                        (concat
                         (capture-register-writeback
                          \"point-to-register\")
                         \"\\t\"
                         (capture-register-writeback
                          \"frameset-to-register\")
                         \"\\t\"
                         (capture-register-writeback
                          \"window-configuration-to-register\")
                         \"\\t\"
                         (capture-register-writeback
                          \"jump-to-register\")
                         \"\\t\"
                         (capture-register-writeback
                          \"copy-to-register\")
                         \"\\t\"
                         (capture-register-writeback
                          \"insert-register\")
                         \"\\t\"
                         (capture-register-writeback
                          \"number-to-register\")
                         \"\\t\"
                         (capture-register-writeback
                          \"increment-register\")
                         \"\\t\"
                         (capture-register-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "point-to-register:written\tframeset-to-register:written\twindow-configuration-to-register:written\tjump-to-register:written\tcopy-to-register:written\tinsert-register:written\tnumber-to-register:written\tincrement-register:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "Register body\n0123456789abcdef"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "/tmp/nemacs-register-buffer.txt"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "RegisterBuffer"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "register-layout"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "8"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (should (equal "4"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-split-delta")))
              (should (equal "1\t3\tFrameB"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-frame-state")))
              (should (equal "00014"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00005"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00003"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-window-layout-writeback-helper ()
  "Bridge window layout writeback helper should write layout state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-window-layout-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--window-layout \"window-layout-state\")
                       (setq files--window-selected \"9\")
                       (fset 'capture-window-layout-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-window-layout-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-window-layout-writeback-helper\"
                        (concat
                         (capture-window-layout-writeback
                          \"delete-other-windows\")
                         \"\\t\"
                         (capture-window-layout-writeback
                          \"delete-window\")
                         \"\\t\"
                         (capture-window-layout-writeback
                          \"split-window-right\")
                         \"\\t\"
                         (capture-window-layout-writeback
                          \"split-window-below\")
                         \"\\t\"
                         (capture-window-layout-writeback
                          \"balance-windows\")
                         \"\\t\"
                         (capture-window-layout-writeback
                          \"shrink-window-if-larger-than-buffer\")
                         \"\\t\"
                         (capture-window-layout-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "delete-other-windows:written\tdelete-window:written\tsplit-window-right:written\tsplit-window-below:written\tbalance-windows:written\tshrink-window-if-larger-than-buffer:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "window-layout-state"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "9"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-window-split-writeback-helper ()
  "Bridge window split writeback helper should write split state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-window-split-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--window-layout \"window-split-layout\")
                       (setq files--window-selected \"10\")
                       (setq files--window-split-delta 6)
                       (fset 'capture-window-split-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-window-split-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-window-split-writeback-helper\"
                        (concat
                         (capture-window-split-writeback
                          \"fit-window-to-buffer\")
                         \"\\t\"
                         (capture-window-split-writeback
                          \"delete-windows-on\")
                         \"\\t\"
                         (capture-window-split-writeback
                          \"split-root-window-below\")
                         \"\\t\"
                         (capture-window-split-writeback
                          \"split-root-window-right\")
                         \"\\t\"
                         (capture-window-split-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "fit-window-to-buffer:written\tdelete-windows-on:written\tsplit-root-window-below:written\tsplit-root-window-right:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "window-split-layout"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "10"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (should (equal "6"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-split-delta")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-window-state-writeback-helper ()
  "Bridge window state writeback helper should write frame and dedicated state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-window-state-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--window-layout \"window-state-layout\")
                       (setq files--window-selected \"11\")
                       (setq files--window-split-delta 7)
                       (setq files--window-dedicated-p t)
                       (setq files--frame-selected-index 2)
                       (setq files--frame-count 4)
                       (setq files--frame-selected-name \"FrameC\")
                       (setq files--frame-undo-active t)
                       (setq files--frame-undo-index 1)
                       (setq files--frame-undo-name \"FrameB\")
                       (fset 'capture-window-state-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-window-state-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-window-state-writeback-helper\"
                        (concat
                         (capture-window-state-writeback
                          \"tear-off-window\")
                         \"\\t\"
                         (capture-window-state-writeback
                          \"toggle-window-dedicated\")
                         \"\\t\"
                         (capture-window-state-writeback
                          \"quit-window\")
                         \"\\t\"
                         (capture-window-state-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "tear-off-window:written\ttoggle-window-dedicated:written\tquit-window:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "window-state-layout"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "11"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (should (equal "7"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-split-delta")))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-dedicated")))
              (should (equal "2\t4\tFrameC"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-frame-state")))
              (should (equal "1\tFrameB"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-frame-undo-state")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-dired-writeback-helper ()
  "Bridge dired writeback helper should write dired buffer state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-dired-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string
                             \"  file-a\\nD file-b\\n\")
                       (setq files--buffer-name \"DiredBuffer\")
                       (setq files--modeline-string \"Dired modeline\")
                       (setq files--point 15)
                       (setq files--mark 2)
                       (setq files--window-start 1)
                       (fset 'capture-dired-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-dired-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-dired-writeback-helper\"
                        (concat
                         (capture-dired-writeback \"dired-mark\")
                         \"\\t\"
                         (capture-dired-writeback \"dired-unmark\")
                         \"\\t\"
                         (capture-dired-writeback
                          \"dired-flag-file-deletion\")
                         \"\\t\"
                         (capture-dired-writeback \"dired-do-rename\")
                         \"\\t\"
                         (capture-dired-writeback \"dired-do-copy\")
                         \"\\t\"
                         (capture-dired-writeback
                          \"dired-do-flagged-delete\")
                         \"\\t\"
                         (capture-dired-writeback \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "dired-mark:written\tdired-unmark:written\tdired-flag-file-deletion:written\tdired-do-rename:written\tdired-do-copy:written\tdired-do-flagged-delete:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "  file-a\nD file-b\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "DiredBuffer"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "Dired modeline"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-modeline")))
              (should (equal "00015"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00002"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00001"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-org-writeback-helper ()
  "Bridge org writeback helper should write org buffer state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-org-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string
                             \"* TODO item\\n| a | b |\\n\")
                       (setq files--current-file-name
                             \"/tmp/nemacs-org-buffer.org\")
                       (setq files--buffer-name \"OrgBuffer\")
                       (setq files--buffer-read-only-p t)
                       (setq files--point 11)
                       (setq files--mark 1)
                       (setq files--window-start 0)
                       (fset 'capture-org-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-org-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-org-writeback-helper\"
                        (concat
                         (capture-org-writeback \"org-todo\")
                         \"\\t\"
                         (capture-org-writeback \"org-capture\")
                         \"\\t\"
                         (capture-org-writeback
                          \"org-table-next-field\")
                         \"\\t\"
                         (capture-org-writeback \"org-cycle\")
                         \"\\t\"
                         (capture-org-writeback \"org-table-align\")
                         \"\\t\"
                         (capture-org-writeback
                          \"org-narrow-to-subtree\")
                         \"\\t\"
                         (capture-org-writeback \"org-agenda\")
                         \"\\t\"
                         (capture-org-writeback \"org-shifttab\")
                         \"\\t\"
                         (capture-org-writeback \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "org-todo:written\torg-capture:written\torg-table-next-field:written\torg-cycle:written\torg-table-align:written\torg-narrow-to-subtree:written\torg-agenda:written\torg-shifttab:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "* TODO item\n| a | b |\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "/tmp/nemacs-org-buffer.org"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "OrgBuffer"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-read-only")))
              (should (equal "00011"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00001"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00000"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-magit-vc-writeback-helper ()
  "Bridge magit/vc writeback helper should write repository buffer state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-magit-vc-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string
                             \"On branch main\\n M file.el\\n\")
                       (setq files--current-file-name
                             \"/tmp/nemacs-vc-file.el\")
                       (setq files--buffer-name \"MagitStatus\")
                       (setq files--buffer-read-only-p t)
                       (setq files--modeline-string \"Magit modeline\")
                       (setq files--point 19)
                       (setq files--mark 4)
                       (setq files--window-start 3)
                       (fset 'capture-magit-vc-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-magit-vc-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-magit-vc-writeback-helper\"
                        (concat
                         (capture-magit-vc-writeback \"magit-status\")
                         \"\\t\"
                         (capture-magit-vc-writeback
                          \"magit-stage-file\")
                         \"\\t\"
                         (capture-magit-vc-writeback
                          \"magit-unstage-file\")
                         \"\\t\"
                         (capture-magit-vc-writeback \"magit-diff\")
                         \"\\t\"
                         (capture-magit-vc-writeback \"magit-log\")
                         \"\\t\"
                         (capture-magit-vc-writeback \"vc-root-diff\")
                         \"\\t\"
                         (capture-magit-vc-writeback \"magit-commit\")
                         \"\\t\"
                         (capture-magit-vc-writeback \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "magit-status:written\tmagit-stage-file:written\tmagit-unstage-file:written\tmagit-diff:written\tmagit-log:written\tvc-root-diff:written\tmagit-commit:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "On branch main\n M file.el\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "/tmp/nemacs-vc-file.el"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "MagitStatus"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-read-only")))
              (should (equal "Magit modeline"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-modeline")))
              (should (equal "00019"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00004"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00003"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-customize-writeback-helper ()
  "Bridge customize writeback helper should write customize buffer state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-customize-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string
                             \"Customize: fill-column\\nValue: 70\\n\")
                       (setq files--current-file-name
                             \"/tmp/nemacs-custom.el\")
                       (setq files--buffer-name \"*Customize*\")
                       (setq files--buffer-read-only-p nil)
                       (setq files--modeline-string
                             \"Customize modeline\")
                       (setq files--point 24)
                       (setq files--mark 5)
                       (setq files--window-start 2)
                       (fset 'capture-customize-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-customize-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-customize-writeback-helper\"
                        (concat
                         (capture-customize-writeback
                          \"customize-variable\")
                         \"\\t\"
                         (capture-customize-writeback
                          \"customize-save-variable\")
                         \"\\t\"
                         (capture-customize-writeback \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "customize-variable:written\tcustomize-save-variable:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "Customize: fill-column\nValue: 70\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "/tmp/nemacs-custom.el"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "*Customize*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "0"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-read-only")))
              (should (equal "Customize modeline"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-modeline")))
              (should (equal "00024"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00005"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00002"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-side-window-resize-writeback-helper ()
  "Bridge side/window resize writeback helper should write window state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-side-window-resize-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--side-windows-visible-p t)
                       (setq files--window-layout
                             \"root(v main)(v side)\")
                       (setq files--window-selected \"side\")
                       (setq files--window-split-delta 9)
                       (fset 'capture-side-window-resize-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-side-window-resize-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-side-window-resize-writeback-helper\"
                        (concat
                         (capture-side-window-resize-writeback
                          \"window-toggle-side-windows\")
                         \"\\t\"
                         (capture-side-window-resize-writeback
                          \"enlarge-window\")
                         \"\\t\"
                         (capture-side-window-resize-writeback
                          \"shrink-window-horizontally\")
                         \"\\t\"
                         (capture-side-window-resize-writeback
                          \"enlarge-window-horizontally\")
                         \"\\t\"
                         (capture-side-window-resize-writeback
                          \"other-window\")
                         \"\\t\"
                         (capture-side-window-resize-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "window-toggle-side-windows:written\tenlarge-window:written\tshrink-window-horizontally:written\tenlarge-window-horizontally:written\tother-window:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-side-windows-visible")))
              (should (equal "root(v main)(v side)"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-layout")))
              (should (equal "side"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-selected")))
              (should (equal "9"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-split-delta")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-word-sexp-defun-motion-writeback-helper ()
  "Bridge word/sexp/defun motion writeback helper should write point and mark."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-word-sexp-defun-motion-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string
                             \"alpha beta gamma delta epsilon zeta eta theta iota\")
                       (setq files--point 42)
                       (setq files--mark 7)
                       (fset 'capture-word-sexp-defun-motion-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-word-sexp-defun-motion-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-word-sexp-defun-motion-writeback-helper\"
                        (concat
                         (capture-word-sexp-defun-motion-writeback
                          \"forward-word\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"backward-word\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"beginning-of-defun\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"forward-sexp\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"backward-sexp\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"end-of-defun\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"down-list\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"forward-list\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"backward-list\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"backward-up-list\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"forward-sentence\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"backward-sentence\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"mark-defun\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"mark-sexp\")
                         \"\\t\"
                         (capture-word-sexp-defun-motion-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "forward-word:written\tbackward-word:written\tbeginning-of-defun:written\tforward-sexp:written\tbackward-sexp:written\tend-of-defun:written\tdown-list:written\tforward-list:written\tbackward-list:written\tbackward-up-list:written\tforward-sentence:written\tbackward-sentence:written\tmark-defun:written\tmark-sexp:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "00042"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00007"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-kill-abbrev-writeback-helper ()
  "Bridge kill/abbrev writeback helper should write buffer, kill, point, and mark."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-kill-abbrev-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string
                             \"alpha beta gamma delta epsilon\")
                       (setq files--kill-ring-head \"beta\")
                       (setq files--point 18)
                       (setq files--mark 6)
                       (fset 'capture-kill-abbrev-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-kill-abbrev-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-kill-abbrev-writeback-helper\"
                        (concat
                         (capture-kill-abbrev-writeback \"kill-word\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback \"kill-sexp\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"backward-kill-word\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback \"zap-to-char\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback \"expand-abbrev\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"add-global-abbrev\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"add-mode-abbrev\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"inverse-add-global-abbrev\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"inverse-add-mode-abbrev\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"abbrev-prefix-mark\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"expand-jump-to-next-slot\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"expand-jump-to-previous-slot\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"dabbrev-expand\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"dabbrev-completion\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"complete-symbol\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"transpose-words\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"transpose-sexps\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"insert-parentheses\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"move-past-close-and-reindent\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"transpose-lines\")
                         \"\\t\"
                         (capture-kill-abbrev-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "kill-word:written\tkill-sexp:written\tbackward-kill-word:written\tzap-to-char:written\texpand-abbrev:written\tadd-global-abbrev:written\tadd-mode-abbrev:written\tinverse-add-global-abbrev:written\tinverse-add-mode-abbrev:written\tabbrev-prefix-mark:written\texpand-jump-to-next-slot:written\texpand-jump-to-previous-slot:written\tdabbrev-expand:written\tdabbrev-completion:written\tcomplete-symbol:written\ttranspose-words:written\ttranspose-sexps:written\tinsert-parentheses:written\tmove-past-close-and-reindent:written\ttranspose-lines:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "alpha beta gamma delta epsilon"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "beta"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-kill")))
              (should (equal "00018"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00006"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-mark-count-eval-writeback-helper ()
  "Bridge mark/count/eval writeback helper should write point, mark, and modeline."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-mark-count-eval-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string
                             \"alpha beta gamma delta epsilon\")
                       (setq files--modeline-string \"Count: 5\")
                       (setq files--point 16)
                       (setq files--mark 2)
                       (fset 'capture-mark-count-eval-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-mark-count-eval-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-mark-count-eval-writeback-helper\"
                        (concat
                         (capture-mark-count-eval-writeback \"mark-word\")
                         \"\\t\"
                         (capture-mark-count-eval-writeback
                          \"count-words-region\")
                         \"\\t\"
                         (capture-mark-count-eval-writeback
                          \"count-lines-page\")
                         \"\\t\"
                         (capture-mark-count-eval-writeback
                          \"eval-last-sexp\")
                         \"\\t\"
                         (capture-mark-count-eval-writeback
                          \"eval-expression\")
                         \"\\t\"
                         (capture-mark-count-eval-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "mark-word:written\tcount-words-region:written\tcount-lines-page:written\teval-last-sexp:written\teval-expression:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "Count: 5"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-modeline")))
              (should (equal "00016"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00002"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-paragraph-region-edit-writeback-helper ()
  "Bridge paragraph/region edit writeback helper should write matching state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-paragraph-region-edit-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string
                             \"alpha beta gamma\\n\\ndelta epsilon\")
                       (setq files--kill-ring-head \"sentence\")
                       (setq files--point 20)
                       (setq files--mark 3)
                       (fset 'capture-paragraph-region-edit-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-paragraph-region-edit-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-paragraph-region-edit-writeback-helper\"
                        (concat
                         (capture-paragraph-region-edit-writeback
                          \"forward-paragraph\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"backward-paragraph\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"set-fill-column\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"set-fill-prefix\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"comment-set-column\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"not-modified\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"mark-paragraph\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"kill-sentence\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"backward-kill-sentence\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"fill-paragraph\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"transpose-chars\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"delete-horizontal-space\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"cycle-spacing\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"just-one-space\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"delete-indentation\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"comment-line\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"comment-dwim\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"upcase-word\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"downcase-word\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"capitalize-word\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"upcase-region\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"downcase-region\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"capitalize-region\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"sort-lines\")
                         \"\\t\"
                         (capture-paragraph-region-edit-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "forward-paragraph:written\tbackward-paragraph:written\tset-fill-column:written\tset-fill-prefix:written\tcomment-set-column:written\tnot-modified:written\tmark-paragraph:written\tkill-sentence:written\tbackward-kill-sentence:written\tfill-paragraph:written\ttranspose-chars:written\tdelete-horizontal-space:written\tcycle-spacing:written\tjust-one-space:written\tdelete-indentation:written\tcomment-line:written\tcomment-dwim:written\tupcase-word:written\tdowncase-word:written\tcapitalize-word:written\tupcase-region:written\tdowncase-region:written\tcapitalize-region:written\tsort-lines:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "alpha beta gamma\n\ndelta epsilon"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "sentence"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-kill")))
              (should (equal "00020"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00003"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-delete-insert-writeback-helper ()
  "Bridge delete/insert writeback helper should write matching state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-delete-insert-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string \"alphaXbeta\")
                       (setq files--modeline-string \"Insert: A\")
                       (setq files--point 7)
                       (setq files--mark 2)
                       (fset 'capture-delete-insert-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-delete-insert-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-delete-insert-writeback-helper\"
                        (concat
                         (capture-delete-insert-writeback \"delete-char\")
                         \"\\t\"
                         (capture-delete-insert-writeback
                          \"backward-delete-char\")
                         \"\\t\"
                         (capture-delete-insert-writeback
                          \"delete-backward-char\")
                         \"\\t\"
                         (capture-delete-insert-writeback
                          \"self-insert-command\")
                         \"\\t\"
                         (capture-delete-insert-writeback \"insert-char\")
                         \"\\t\"
                         (capture-delete-insert-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "delete-char:written\tbackward-delete-char:written\tdelete-backward-char:written\tself-insert-command:written\tinsert-char:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "alphaXbeta"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "Insert: A"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-modeline")))
              (should (equal "00007"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00002"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-emoji-writeback-helper ()
  "Bridge emoji writeback helper should write matching state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-emoji-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string \"emoji buffer\")
                       (setq files--current-file-name \"/tmp/emoji.org\")
                       (setq files--buffer-name \"*Emoji*\")
                       (setq files--buffer-read-only-p t)
                       (setq files--modeline-string \"Emoji zoom 1\")
                       (setq files--point 8)
                       (setq files--mark 3)
                       (setq files--window-start 2)
                       (fset 'capture-emoji-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-emoji-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-emoji-writeback-helper\"
                        (concat
                         (capture-emoji-writeback \"emoji-insert\")
                         \"\\t\"
                         (capture-emoji-writeback \"emoji-list\")
                         \"\\t\"
                         (capture-emoji-writeback \"emoji-recent\")
                         \"\\t\"
                         (capture-emoji-writeback \"emoji-search\")
                         \"\\t\"
                         (capture-emoji-writeback \"emoji-describe\")
                         \"\\t\"
                         (capture-emoji-writeback
                          \"emoji-zoom-increase\")
                         \"\\t\"
                         (capture-emoji-writeback
                          \"emoji-zoom-decrease\")
                         \"\\t\"
                         (capture-emoji-writeback \"emoji-zoom-reset\")
                         \"\\t\"
                         (capture-emoji-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "emoji-insert:written\temoji-list:written\temoji-recent:written\temoji-search:written\temoji-describe:written\temoji-zoom-increase:written\temoji-zoom-decrease:written\temoji-zoom-reset:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "emoji buffer"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "/tmp/emoji.org"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "*Emoji*"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-read-only")))
              (should (equal "Emoji zoom 1"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-modeline")))
              (should (equal "00008"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00003"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00002"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-kmacro-writeback-helper ()
  "Bridge kmacro writeback helper should write matching state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-kmacro-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string \"kmacro buffer\")
                       (setq files--current-file-name \"/tmp/kmacro.el\")
                       (setq files--buffer-name \"kmacro.el\")
                       (setq files--buffer-read-only-p nil)
                       (setq files--modeline-string \"Kmacro active\")
                       (setq files--point 9)
                       (setq files--mark 4)
                       (setq files--window-start 3)
                       (fset 'capture-kmacro-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-kmacro-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-kmacro-writeback-helper\"
                        (concat
                         (capture-kmacro-writeback
                          \"kmacro-start-macro\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-end-macro\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-end-and-call-macro\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kbd-macro-query\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-insert-counter\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-set-counter\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-add-counter\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-edit-macro-repeat\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-view-macro-repeat\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-edit-macro\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-step-edit-macro\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"edit-kbd-macro\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-edit-lossage\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-end-or-call-macro-repeat\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-call-ring-2nd-repeat\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"apply-macro-to-region-lines\")
                         \"\\t\"
                         (capture-kmacro-writeback \"kmacro-keymap\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-delete-ring-head\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-set-format\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-cycle-ring-next\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-cycle-ring-previous\")
                         \"\\t\"
                         (capture-kmacro-writeback \"kmacro-swap-ring\")
                         \"\\t\"
                         (capture-kmacro-writeback \"kmacro-bind-to-key\")
                         \"\\t\"
                         (capture-kmacro-writeback \"kmacro-redisplay\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-name-last-macro\")
                         \"\\t\"
                         (capture-kmacro-writeback
                          \"kmacro-to-register\")
                         \"\\t\"
                         (capture-kmacro-writeback \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "kmacro-start-macro:written\tkmacro-end-macro:written\tkmacro-end-and-call-macro:written\tkbd-macro-query:written\tkmacro-insert-counter:written\tkmacro-set-counter:written\tkmacro-add-counter:written\tkmacro-edit-macro-repeat:written\tkmacro-view-macro-repeat:written\tkmacro-edit-macro:written\tkmacro-step-edit-macro:written\tedit-kbd-macro:written\tkmacro-edit-lossage:written\tkmacro-end-or-call-macro-repeat:written\tkmacro-call-ring-2nd-repeat:written\tapply-macro-to-region-lines:written\tkmacro-keymap:written\tkmacro-delete-ring-head:written\tkmacro-set-format:written\tkmacro-cycle-ring-next:written\tkmacro-cycle-ring-previous:written\tkmacro-swap-ring:written\tkmacro-bind-to-key:written\tkmacro-redisplay:written\tkmacro-name-last-macro:written\tkmacro-to-register:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "kmacro buffer"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "/tmp/kmacro.el"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-file")))
              (should (equal "kmacro.el"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buffer-name")))
              (should (equal "0"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-read-only")))
              (should (equal "Kmacro active"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-modeline")))
              (should (equal "00009"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00004"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))
              (should (equal "00003"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-window-start")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-indent-newline-writeback-helper ()
  "Bridge indent/newline writeback helper should write matching state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-indent-newline-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string \"alpha\\n  beta\\n\")
                       (setq files--point 10)
                       (setq files--mark 3)
                       (fset 'capture-indent-newline-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-indent-newline-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-indent-newline-writeback-helper\"
                        (concat
                         (capture-indent-newline-writeback
                          \"quoted-insert\")
                         \"\\t\"
                         (capture-indent-newline-writeback
                          \"indent-for-tab-command\")
                         \"\\t\"
                         (capture-indent-newline-writeback
                          \"tab-to-tab-stop\")
                         \"\\t\"
                         (capture-indent-newline-writeback
                          \"indent-region\")
                         \"\\t\"
                         (capture-indent-newline-writeback
                          \"indent-rigidly\")
                         \"\\t\"
                         (capture-indent-newline-writeback \"newline\")
                         \"\\t\"
                         (capture-indent-newline-writeback
                          \"electric-newline-and-maybe-indent\")
                         \"\\t\"
                         (capture-indent-newline-writeback
                          \"default-indent-new-line\")
                         \"\\t\"
                         (capture-indent-newline-writeback \"open-line\")
                         \"\\t\"
                         (capture-indent-newline-writeback \"split-line\")
                         \"\\t\"
                         (capture-indent-newline-writeback
                          \"delete-blank-lines\")
                         \"\\t\"
                         (capture-indent-newline-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "quoted-insert:written\tindent-for-tab-command:written\ttab-to-tab-stop:written\tindent-region:written\tindent-rigidly:written\tnewline:written\telectric-newline-and-maybe-indent:written\tdefault-indent-new-line:written\topen-line:written\tsplit-line:written\tdelete-blank-lines:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "alpha\n  beta\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "00010"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00003"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(ert-deftest nemacs-gui-file-bridge-runtime-test/standalone-bridge-kill-yank-writeback-helper ()
  "Bridge kill/yank writeback helper should write matching state."
  (nemacs-gui-file-bridge-runtime-test--skip-unless-reader
    (let ((reader (nemacs-gui-file-bridge-runtime-test--reader))
          (image (nemacs-gui-file-bridge-runtime-test--write-image))
          (probe-file "/tmp/nemacs-bridge-kill-yank-writeback-helper"))
      (unwind-protect
          (nemacs-gui-file-bridge-runtime-test--with-transport
            (let ((result
                   (nemacs-gui-file-bridge-runtime-test--run-ok
                    reader image
                    "(progn
                       (setq files--buffer-string \"alpha\\ngamma\")
                       (setq files--kill-ring-head \"beta\\n\")
                       (setq files--point 7)
                       (setq files--mark 2)
                       (fset 'capture-kill-yank-writeback
                             (lambda (command)
                               (setq files--bridge-status \"ok\")
                               (let ((returned
                                      (files--bridge-kill-yank-writeback-current-context
                                       command)))
                                 (concat returned
                                         \":\"
                                         files--bridge-status))))
                       (nl-write-file
                        \"/tmp/nemacs-bridge-kill-yank-writeback-helper\"
                        (concat
                         (capture-kill-yank-writeback \"kill-line\")
                         \"\\t\"
                         (capture-kill-yank-writeback
                          \"kill-whole-line\")
                         \"\\t\"
                         (capture-kill-yank-writeback \"yank\")
                         \"\\t\"
                         (capture-kill-yank-writeback \"yank-pop\")
                         \"\\t\"
                         (capture-kill-yank-writeback
                          \"forward-char\"))))")))
              (should (equal 0 (plist-get result :status)))
              (should (equal "kill-line:written\tkill-whole-line:written\tyank:written\tyank-pop:written\tforward-char:ok"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              probe-file)))
              (should (equal "alpha\ngamma"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-buf")))
              (should (equal "beta\n"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-kill")))
              (should (equal "00007"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-point")))
              (should (equal "00002"
                             (nemacs-gui-file-bridge-runtime-test--slurp
                              "/tmp/nemacs-mark")))))
        (when (file-exists-p image)
          (delete-file image))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(provide 'nemacs-gui-file-bridge-runtime-test)

;;; nemacs-gui-file-bridge-runtime-test.el ends here
