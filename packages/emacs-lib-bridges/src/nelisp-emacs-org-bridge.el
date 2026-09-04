;;; nelisp-emacs-org-bridge.el --- load the real vendor Org chain into a live session -*- lexical-binding: t; -*-

;;; Commentary:

;; Session-side loader for the vendor-first Org lane.
;;
;; The real Org source is normalized ahead of time by
;; `scripts/build-nelisp-emacs-org-bridge-bundle.el' and loaded here into an
;; already-bootstrapped NeLisp session or runtime image.  This is bridge
;; wiring only, not a local Org reimplementation.

;;; Code:

(defvar nelisp-emacs-org-bridge-repo-root nil
  "Repository root used to resolve the generated Org bundle path.")

(defvar nelisp-emacs-org-bridge-bundle-file nil
  "Explicit path to the generated Org bridge bundle.")

(defvar nelisp-emacs-org-bridge-loaded nil
  "Non-nil once the Org bridge bundle has been loaded in this session.")

(defvar nelisp-emacs-org-bridge-trace
  (and (fboundp 'getenv)
       (getenv "NEMACS_ORG_BRIDGE_TRACE"))
  "Non-nil means print bridge load progress markers.")

(defun nelisp-emacs-org-bridge--trace (format-string &rest args)
  "Print a bridge trace line when tracing is enabled."
  (when nelisp-emacs-org-bridge-trace
    (let ((line (apply #'format (concat "ORG-BRIDGE " format-string "\n")
                       args)))
      (if (fboundp 'nelisp--write-stdout-bytes)
          (nelisp--write-stdout-bytes line)
        (princ line)))))

(defun nelisp-emacs-org-bridge--normalize-path (path)
  "Return PATH resolved as far as the current substrate allows.
The Org bridge can be entered before standalone `file-truename' is installed.
Use it when available, otherwise fall back to `expand-file-name'."
  (if (fboundp 'file-truename)
      (file-truename path)
    (expand-file-name path)))

(defun nelisp-emacs-org-bridge--vendor-file (relative)
  "Return RELATIVE resolved under the repository root."
  (expand-file-name relative (nelisp-emacs-org-bridge--repo-root)))

(defun nelisp-emacs-org-bridge--source-file (relative)
  "Return RELATIVE under the repository root's source tree."
  (expand-file-name relative (nelisp-emacs-org-bridge--repo-root)))

(defun nelisp-emacs-org-bridge--ensure-var (symbol value)
  "Set SYMBOL to VALUE when it is missing or carries the NeLisp unbound marker."
  (when (or (not (boundp symbol))
            (eq (symbol-value symbol) 'nelisp--unbound-marker))
    (set symbol value)))

(defun nelisp-emacs-org-bridge--function-live-p (symbol)
  "Return non-nil when SYMBOL has a callable function cell."
  (and (fboundp symbol)
       (not (eq (symbol-function symbol) 'nelisp--unbound-marker))))

(defun nelisp-emacs-org-bridge--repo-root ()
  "Return the resolved repository root."
  (let ((explicit nelisp-emacs-org-bridge-repo-root)
        (origin (or (and (boundp 'load-file-name)
                         load-file-name)
                    (and (boundp 'buffer-file-name)
                         buffer-file-name)
                    (and (boundp 'default-directory)
                         default-directory)
                    nil)))
    (cond
     ((and (stringp explicit)
           (> (length explicit) 0))
      (nelisp-emacs-org-bridge--normalize-path explicit))
     (origin
      (nelisp-emacs-org-bridge--normalize-path
       (expand-file-name ".." (file-name-directory origin))))
     (t
      (error "nelisp-emacs-org-bridge repo root is unresolved")))))

(defun nelisp-emacs-org-bridge--bundle-file ()
  "Return the resolved Org bridge bundle path."
  (or nelisp-emacs-org-bridge-bundle-file
      (expand-file-name "build/nelisp-emacs-org-bridge-bundle.el"
                        (nelisp-emacs-org-bridge--repo-root))))

(defun nelisp-emacs-org-bridge--ensure-load-path ()
  "Ensure the vendor Org dependency paths are available."
  (let* ((root (nelisp-emacs-org-bridge--repo-root))
         (paths (list
                 (expand-file-name "src" root)
                 (expand-file-name "vendor/emacs-lisp" root)
                 (expand-file-name "vendor/emacs-lisp/emacs-lisp" root)
                 (expand-file-name "vendor/emacs-lisp/org" root)
                 (expand-file-name "vendor/emacs-lisp/calendar" root)
                 (expand-file-name "vendor/emacs-lisp/international" root)
                 (expand-file-name "vendor/emacs-lisp/textmodes" root)
                 (expand-file-name "vendor/emacs-lisp/url" root)
                 (expand-file-name "vendor/emacs-lisp/mail" root)
                 (expand-file-name "vendor/emacs-lisp/gnus" root)
                 (expand-file-name "vendor/emacs-lisp/net" root)
                 (expand-file-name "vendor/emacs-lisp/vc" root))))
    (dolist (path (reverse paths))
      (when (and (file-directory-p path)
                 (not (member path load-path)))
        (push path load-path)))))

(defun nelisp-emacs-org-bridge--ensure-core-substrate ()
  "Ensure shared mode/window substrate is loaded before vendor Org."
  ;; Source-v1 runtime-image replay does not reliably preserve `provide'
  ;; side-effects for every loaded source file, so use ordered absolute-path
  ;; loads here instead of `require'.  The important part is the live function
  ;; cell state, not the feature registry entry.
  (dolist (file '("src/emacs-mode.el"
                  "src/emacs-mode-builtins.el"
                  "src/emacs-stub.el"
                  "src/emacs-vars.el"
                  "src/emacs-time.el"
                  "src/emacs-eval.el"
                  "src/emacs-easy-mmode.el"
                  "src/lisp.el"
                  "src/emacs-list.el"
                  "src/emacs-subr-extras.el"
                  "src/emacs-backquote.el"
                  "src/emacs-pcase.el"
                  "src/emacs-cl-macros.el"
                  "src/seq.el"
                  "src/nelisp-regex.el"
                  "src/nelisp-text-buffer.el"
                  "src/nelisp-emacs-compat.el"
                  "src/emacs-search-builtins.el"
                  "src/emacs-char-table.el"
                  "src/nelisp-emacs-compat-fileio.el"
                  "src/files-runtime.el"
                  "src/files-standalone-buffer.el"
                  "src/emacs-string.el"
                  "src/emacs-file-name-handler.el"
                  "src/emacs-fileio-builtins.el"
                  "src/emacs-fileio.el"
                  "src/emacs-buffer.el"
                  "src/emacs-buffer-builtins.el"
                  "src/emacs-faces.el"
                  "src/emacs-faces-builtins.el"
                  "src/emacs-window.el"
                  "src/emacs-window-builtins.el"
                  "src/emacs-command-loop.el"
                  "src/emacs-command-loop-builtins.el"
                  "src/emacs-keymap.el"
                  "src/emacs-keymap-builtins.el"
                  "src/emacs-font-lock-builtins.el"
                  "src/emacs-minibuffer-builtins.el"
                  "src/emacs-textmodes-stub.el"
                  "src/simple.el"))
    (nelisp-emacs-org-bridge--trace "core-load %s" file)
    (load (nelisp-emacs-org-bridge--source-file file)
          nil 'no-message t t))
  (nelisp-emacs-org-bridge--trace "core-load vendor outline")
  (load (nelisp-emacs-org-bridge--vendor-file
         "vendor/emacs-lisp/outline.el")
        nil 'no-message t t)
  ;; The current Org bridge path does not replay `org-element.el' yet, but
  ;; vendor Org mode calls into the cache reset hook during startup.  Keep the
  ;; cache API callable so mode entry can proceed; this bridge does not rely on
  ;; persistent org-element cache semantics yet.
  (unless (boundp 'org-element-cache-persistent)
    (defvar org-element-cache-persistent nil))
  (unless (boundp 'org-element-use-cache)
    (defvar org-element-use-cache nil))
  (unless (fboundp 'org-element-cache-reset)
    (defun org-element-cache-reset (&optional _all _no-persistence)
      nil))
  ;; The normalized `org-faces.el` bundle currently retains the face objects
  ;; but drops the `org-tag-faces` defcustom; Org mode treats the default nil
  ;; value as "no per-tag face overrides", so define that base variable here.
  (nelisp-emacs-org-bridge--ensure-var 'org-tag-faces nil)
  (nelisp-emacs-org-bridge--ensure-var 'header-line-format nil)
  (nelisp-emacs-org-bridge--ensure-var 'org-capture-mode-hook nil)
  (nelisp-emacs-org-bridge--ensure-var 'debug-on-error nil)
  (nelisp-emacs-org-bridge--ensure-var 'kill-ring nil)
  (nelisp-emacs-org-bridge--ensure-var 'before-change-functions nil)
  (nelisp-emacs-org-bridge--ensure-var 'after-change-functions nil)
  (unless (nelisp-emacs-org-bridge--function-live-p 'substitute-command-keys)
    (defun substitute-command-keys (string) string))
  (unless (nelisp-emacs-org-bridge--function-live-p 'display-warning)
    (defun display-warning (_type _message &optional _level _buffer-name)
      nil))
  (unless (nelisp-emacs-org-bridge--function-live-p 'emacs-pid)
    (defun emacs-pid () 0))
  (unless (nelisp-emacs-org-bridge--function-live-p 'secure-hash)
    (defun secure-hash (_algorithm object &optional _start _end _binary)
      (format "%x" (sxhash-equal object))))
  t)

(defun nelisp-emacs-org-bridge-loaded-p ()
  "Return non-nil when the Org bridge has been loaded in this session."
  nelisp-emacs-org-bridge-loaded)

(defun nelisp-emacs-org-bridge--load-post-bundle-vendor-source ()
  "Load vendor Org sources whose full bodies are still needed at runtime.
The generated bundle is useful as the main ordered replay artifact, but some
heavily-interconnected Org files still need their complete upstream source
bodies in a live session."
  (dolist (file '("vendor/emacs-lisp/org/org-element-ast.el"
                  "vendor/emacs-lisp/emacs-lisp/avl-tree.el"
                  "vendor/emacs-lisp/calendar/calendar.el"
                  "vendor/emacs-lisp/calendar/cal-iso.el"
                  "vendor/emacs-lisp/calendar/diary-lib.el"
                  "vendor/emacs-lisp/org/org-compat.el"
                  "vendor/emacs-lisp/org/org-faces.el"
                  "vendor/emacs-lisp/org/org-persist.el"
                  "vendor/emacs-lisp/org/org-footnote.el"
                  "vendor/emacs-lisp/org/org-list.el"
                  "vendor/emacs-lisp/org/org-element.el"
                  "vendor/emacs-lisp/org/org-macro.el"
                  "vendor/emacs-lisp/org/org-capture.el"
                  "vendor/emacs-lisp/org/org-agenda.el"))
    (nelisp-emacs-org-bridge--trace "post-vendor-load %s" file)
    (load (nelisp-emacs-org-bridge--vendor-file file)
          nil 'no-message t t)))

(defun nelisp-emacs-org-bridge--disable-persist-cache ()
  "Disable Org persistent cache in the standalone bridge lane."
  (setq org-persist--index nil
        org-persist--index-hash nil
        org-element-use-cache t
        org-element-cache-persistent nil)
  (defun org-persist-read (&rest _args) nil)
  (defun org-persist-load (&rest _args) nil)
  (defun org-persist-register (&rest _args) nil)
  (defun org-persist-write (&rest _args) nil)
  (defun org-persist--read-elisp-file (&rest _args) nil)
  (defun org-persist--write-elisp-file (&rest _args) nil)
  t)

(defun nelisp-emacs-org-bridge--ensure-org-element-runtime-constants ()
  "Ensure Org element constants that can be lost during standalone replay."
  (nelisp-emacs-org-bridge--ensure-var
   'org-footnote-definition-re
   "^\\[fn:\\([-_[:word:]]+\\)\\]")
  ;; `org-element--current-element-re' is a load-bearing upstream defconst.
  ;; Rebuild it in the bridge lane: standalone replay can either lose the
  ;; defconst or preserve a string where `(group-n 11 "%%(")' has become an
  ;; invalid empty-group regexp.
  (setq org-element--current-element-re
        (rx-to-string
         `(or
           (group-n 1 (regexp ,org-element--latex-begin-environment-nogroup))
           (group-n 2 (regexp ,org-element-drawer-re-nogroup))
           (group-n 3 (regexp "[ \t]*:\\( \\|$\\)"))
           (group-n 7 (regexp ,org-element-dynamic-block-open-re-nogroup))
           (seq (group-n 4 (regexp "[ \t]*#\\+"))
                (or
                 (seq "BEGIN_" (group-n 5 (1+ (not space))))
                 (group-n 6 "CALL:")
                 (group-n 8 (1+ (not space)) ":")))
           (group-n 9 (regexp ,org-footnote-definition-re))
           (group-n 10 (regexp "[ \t]*-----+[ \t]*$"))
           ;; Standalone `rx-to-string' currently escapes literal "(" in this
           ;; string form as an empty group.  Use raw Emacs regexp syntax here:
           ;; bare "(" is literal, and `group-n' supplies the only group.
           (group-n 11 (regexp "%%(")))))
  t)

(defun nelisp-emacs-org-bridge--persist-current-mode-state (mode name)
  "Persist MODE/NAME for the current buffer in the standalone local store."
  (setq major-mode mode
        mode-name name)
  (when (and (fboundp 'current-buffer)
             (fboundp 'nelisp-ec-buffer-p)
             (fboundp 'emacs-buffer-set-buffer-local-value)
             (nelisp-ec-buffer-p (current-buffer)))
    (emacs-buffer-set-buffer-local-value 'mode-name (current-buffer) name)
    (emacs-buffer-set-buffer-local-value 'major-mode (current-buffer) mode)))

(defun nelisp-emacs-org-bridge--buffer-live-p (buffer)
  "Return non-nil when BUFFER is a live NeLisp buffer."
  (and (fboundp 'nelisp-ec-buffer-p)
       (ignore-errors (nelisp-ec-buffer-p buffer))
       (not (and (fboundp 'nelisp-ec-buffer-killed-p)
                 (ignore-errors (nelisp-ec-buffer-killed-p buffer))))))

(defun nelisp-emacs-org-bridge--ensure-selected-window-buffer ()
  "Ensure the selected standalone window displays a live current buffer."
  (when (and (nelisp-emacs-org-bridge--function-live-p 'selected-window)
             (nelisp-emacs-org-bridge--function-live-p 'set-window-buffer)
             (nelisp-emacs-org-bridge--function-live-p 'current-buffer))
    (let ((window (ignore-errors (selected-window)))
          (buffer (ignore-errors (current-buffer))))
      (when (and window (nelisp-emacs-org-bridge--buffer-live-p buffer))
        (ignore-errors (set-window-buffer window buffer))))))

(defun nelisp-emacs-org-bridge--coerce-buffer (buffer-or-name)
  "Return BUFFER-OR-NAME as a live buffer, creating named buffers as needed."
  (cond
   ((nelisp-emacs-org-bridge--buffer-live-p buffer-or-name)
    buffer-or-name)
   ((stringp buffer-or-name)
    (or (and (fboundp 'get-buffer) (get-buffer buffer-or-name))
        (and (fboundp 'get-buffer-create) (get-buffer-create buffer-or-name))))
   (t nil)))

(defun nelisp-emacs-org-bridge--wrap-switch-to-buffer-other-window ()
  "Keep Org capture's temporary buffer switch on a valid standalone window.
The generic window bridge may split a root window whose raw buffer slot is
still nil.  Vendor Org's capture path only needs the temporary capture buffer
to become current, so use the selected window directly in this bridge lane."
  (when (and (nelisp-emacs-org-bridge--function-live-p
              'switch-to-buffer-other-window)
             (not (get 'switch-to-buffer-other-window
                       'nelisp-emacs-org-bridge-wrapped)))
    (fset 'switch-to-buffer-other-window
          (lambda (buffer-or-name &optional _norecord)
            (let ((buffer (nelisp-emacs-org-bridge--coerce-buffer
                           buffer-or-name)))
              (unless (nelisp-emacs-org-bridge--buffer-live-p buffer)
                (signal 'wrong-type-argument
                        (list 'nelisp-ec-buffer-p buffer-or-name)))
              (when (and (fboundp 'selected-window)
                         (fboundp 'set-window-buffer))
                (let ((window (selected-window)))
                  (set-window-buffer window buffer)
                  (when (fboundp 'select-window)
                    (select-window window))))
              (when (fboundp 'nelisp-ec-set-buffer)
                (nelisp-ec-set-buffer buffer))
              buffer)))
    (put 'switch-to-buffer-other-window
         'nelisp-emacs-org-bridge-wrapped t)))

(defun nelisp-emacs-org-bridge--select-buffer (buffer-or-name)
  "Display BUFFER-OR-NAME in the selected window and make it current."
  (let ((buffer (nelisp-emacs-org-bridge--coerce-buffer buffer-or-name)))
    (unless (nelisp-emacs-org-bridge--buffer-live-p buffer)
      (signal 'wrong-type-argument
              (list 'nelisp-ec-buffer-p buffer-or-name)))
    (when (and (fboundp 'selected-window)
               (fboundp 'set-window-buffer))
      (let ((window (selected-window)))
        (set-window-buffer window buffer)
        (when (fboundp 'select-window)
          (select-window window))))
    (when (fboundp 'nelisp-ec-set-buffer)
      (nelisp-ec-set-buffer buffer))
    buffer))

(defun nelisp-emacs-org-bridge--wrap-buffer-display-functions ()
  "Use selected-window buffer display for Org's standalone workflow lane."
  (dolist (symbol '(switch-to-buffer-other-window
                    pop-to-buffer
                    pop-to-buffer-same-window))
    (when (and (nelisp-emacs-org-bridge--function-live-p symbol)
               (not (get symbol 'nelisp-emacs-org-bridge-wrapped)))
      (fset symbol
            (lambda (buffer-or-name &rest _ignored)
              (nelisp-emacs-org-bridge--select-buffer buffer-or-name)))
      (put symbol 'nelisp-emacs-org-bridge-wrapped t))))

(defun nelisp-emacs-org-bridge--plain-capture-template-p (template)
  "Return non-nil when TEMPLATE needs no Org capture placeholder expansion."
  (and (stringp template)
       (not (string-match-p "%" template))))

(defun nelisp-emacs-org-bridge--normalize-capture-template (template)
  "Return TEMPLATE with the trailing newline shape vendor capture expects."
  (cond
   ((not (stringp template)) "")
   ((or (= (length template) 0)
        (= (aref template (1- (length template))) ?\n))
    template)
   (t (concat template "\n"))))

(defun nelisp-emacs-org-bridge--wrap-org-capture-fill-template ()
  "Fast-path plain Org capture templates on the standalone bridge lane.
Vendor `org-capture-fill-template' expands placeholders in a temporary Org
buffer.  That path currently exercises incomplete standalone window/mode
state even for fixed templates that contain no placeholders.  Preserve the
vendor path for real expansions and return fixed templates directly."
  (when (and (nelisp-emacs-org-bridge--function-live-p
              'org-capture-fill-template)
             (not (get 'org-capture-fill-template
                       'nelisp-emacs-org-bridge-wrapped)))
    (let ((original (symbol-function 'org-capture-fill-template)))
      (fset 'org-capture-fill-template
            (lambda (&optional template initial annotation)
              (let ((effective-template
                     (or template
                         (and (fboundp 'org-capture-get)
                              (org-capture-get :template)))))
                (if (and (null initial)
                         (null annotation)
                         (nelisp-emacs-org-bridge--plain-capture-template-p
                          effective-template))
                    (nelisp-emacs-org-bridge--normalize-capture-template
                     effective-template)
                  (funcall original template initial annotation))))))
    (put 'org-capture-fill-template 'nelisp-emacs-org-bridge-wrapped t)))

(defun nelisp-emacs-org-bridge--capture-template-plist (entry)
  "Return ENTRY's Org capture property list."
  (nthcdr 5 entry))

(defun nelisp-emacs-org-bridge--plain-immediate-file-entry-p (entry)
  "Return non-nil when ENTRY can use the standalone file fast path."
  (let ((type (nth 2 entry))
        (target (nth 3 entry))
        (template (nth 4 entry))
        (plist (nelisp-emacs-org-bridge--capture-template-plist entry)))
    (and (memq type '(nil entry))
         (consp target)
         (eq (car target) 'file)
         (plist-get plist :immediate-finish)
         (nelisp-emacs-org-bridge--plain-capture-template-p template))))

(defun nelisp-emacs-org-bridge--capture-target-file (target)
  "Return TARGET's file path for the standalone capture fast path."
  (let ((file (nth 1 target)))
    (cond
     ((stringp file)
      (expand-file-name
       file
       (if (and (boundp 'org-directory) (stringp org-directory))
           org-directory
         default-directory)))
     ((and (symbolp file) (boundp file) (stringp (symbol-value file)))
      (expand-file-name (symbol-value file)))
     (t nil))))

(defun nelisp-emacs-org-bridge--append-plain-capture (entry)
  "Append ENTRY's fixed template to its target file and save it."
  (let* ((target (nth 3 entry))
         (file (nelisp-emacs-org-bridge--capture-target-file target))
         (template (nelisp-emacs-org-bridge--normalize-capture-template
                    (nth 4 entry))))
    (unless (and file (file-readable-p file))
      (error "Org capture target is not readable: %S" file))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert template)
      (save-buffer))
    nil))

(defun nelisp-emacs-org-bridge--wrap-org-capture ()
  "Fast-path plain immediate file captures on the standalone bridge lane."
  (when (and (nelisp-emacs-org-bridge--function-live-p 'org-capture)
             (not (get 'org-capture 'nelisp-emacs-org-bridge-wrapped)))
    (let ((original (symbol-function 'org-capture)))
      (fset 'org-capture
            (lambda (&optional goto keys)
              (let ((entry (and (null goto)
                                (stringp keys)
                                (boundp 'org-capture-templates)
                                (assoc keys org-capture-templates))))
                (if (and entry
                         (nelisp-emacs-org-bridge--plain-immediate-file-entry-p
                          entry))
                    (nelisp-emacs-org-bridge--append-plain-capture entry)
                  (funcall original goto keys))))))
    (put 'org-capture 'nelisp-emacs-org-bridge-wrapped t)))

(defun nelisp-emacs-org-bridge--wrap-org-agenda-mode ()
  "Ensure vendor `org-agenda-mode' survives standalone buffer switching."
  (when (and (nelisp-emacs-org-bridge--function-live-p 'org-agenda-mode)
             (not (get 'org-agenda-mode 'nelisp-emacs-org-bridge-wrapped)))
    (let ((original (symbol-function 'org-agenda-mode)))
      (fset 'org-agenda-mode
            (lambda (&rest args)
              (let ((value (apply original args)))
                (nelisp-emacs-org-bridge--persist-current-mode-state
                 'org-agenda-mode "Org-Agenda")
                value))))
    (put 'org-agenda-mode 'nelisp-emacs-org-bridge-wrapped t)))

(defun nelisp-emacs-org-bridge--agenda-buffer ()
  "Return the vendor Org agenda buffer, when it exists."
  (let ((name (and (boundp 'org-agenda-buffer-name)
                   org-agenda-buffer-name)))
    (and name (get-buffer name))))

(defun nelisp-emacs-org-bridge--agenda-files ()
  "Return the current agenda file list for bridge preflight."
  (condition-case nil
      (cond
       ((nelisp-emacs-org-bridge--function-live-p 'org-agenda-files)
        (org-agenda-files nil 'ifmode))
       ((boundp 'org-agenda-files) org-agenda-files)
       (t nil))
    (error nil)))

(defun nelisp-emacs-org-bridge--warm-org-element-cache-for-agenda-files ()
  "Prime in-memory `org-element' cache before vendor agenda scanning.
The bridge disables persistent cache, but vendor agenda still walks the
in-memory cache for scheduled entries.  Touching each headline with
`org-element-at-point' follows the same cache path vendor agenda later reads,
without changing vendor Org source."
  (when (nelisp-emacs-org-bridge--function-live-p 'org-element-at-point)
    (dolist (file (nelisp-emacs-org-bridge--agenda-files))
      (when (and (stringp file)
                 (file-readable-p file))
        (condition-case nil
            (with-current-buffer (find-file-noselect file)
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward "^\\*+\\(?:[ \t]\\|$\\)" nil t)
                  (beginning-of-line)
                  (org-element-at-point)
                  (forward-line 1))))
          (error nil))))))

(defun nelisp-emacs-org-bridge--wrap-org-agenda-list ()
  "Ensure `org-agenda-list' leaves the final buffer in agenda mode."
  (when (and (nelisp-emacs-org-bridge--function-live-p 'org-agenda-list)
             (not (get 'org-agenda-list 'nelisp-emacs-org-bridge-wrapped)))
    (let ((original (symbol-function 'org-agenda-list)))
      (fset 'org-agenda-list
            (lambda (&rest args)
              (nelisp-emacs-org-bridge--warm-org-element-cache-for-agenda-files)
              (let ((value (apply original args))
                    (buffer (nelisp-emacs-org-bridge--agenda-buffer)))
                (when buffer
                  (with-current-buffer buffer
                    (nelisp-emacs-org-bridge--persist-current-mode-state
                     'org-agenda-mode "Org-Agenda")))
                value))))
    (put 'org-agenda-list 'nelisp-emacs-org-bridge-wrapped t)))

(defun nelisp-emacs-org-bridge-load ()
  "Load the generated vendor Org bridge bundle into the current session."
  (unless nelisp-emacs-org-bridge-loaded
    (nelisp-emacs-org-bridge--trace "load begin")
    (nelisp-emacs-org-bridge--ensure-load-path)
    (nelisp-emacs-org-bridge--trace "load after-load-path")
    (nelisp-emacs-org-bridge--ensure-core-substrate)
    (nelisp-emacs-org-bridge--trace "load after-core")
    (let ((bundle (nelisp-emacs-org-bridge--bundle-file)))
      (unless (file-readable-p bundle)
        (error "org bridge bundle is not readable: %s" bundle))
      (nelisp-emacs-org-bridge--trace "load bundle %s" bundle)
      (load bundle nil 'no-message t t))
    (nelisp-emacs-org-bridge--trace "load after-bundle")
    (nelisp-emacs-org-bridge--load-post-bundle-vendor-source)
    (nelisp-emacs-org-bridge--trace "load after-post-vendor")
    ;; Vendor `files.el` bodies can reinstall upstream Lisp wrappers whose
    ;; standalone contracts depend on C primitives the reader does not ship.
    ;; Reapply the shared file-I/O bridge after the vendor replay so package
    ;; code keeps the substrate-owned `write-region' / temp-file path.
    (load (nelisp-emacs-org-bridge--source-file "src/emacs-fileio-builtins.el")
          nil 'no-message t t)
    (nelisp-emacs-org-bridge--trace "load after-fileio")
    (when (fboundp 'emacs-fileio-builtins-reinstall-standalone-overrides)
      (emacs-fileio-builtins-reinstall-standalone-overrides))
    (nelisp-emacs-org-bridge--trace "load after-fileio-reinstall")
    (nelisp-emacs-org-bridge--disable-persist-cache)
    (nelisp-emacs-org-bridge--ensure-org-element-runtime-constants)
    (nelisp-emacs-org-bridge--ensure-selected-window-buffer)
    (nelisp-emacs-org-bridge--wrap-switch-to-buffer-other-window)
    (nelisp-emacs-org-bridge--wrap-buffer-display-functions)
    (nelisp-emacs-org-bridge--wrap-org-capture-fill-template)
    (nelisp-emacs-org-bridge--wrap-org-capture)
    (nelisp-emacs-org-bridge--wrap-org-agenda-mode)
    (nelisp-emacs-org-bridge--wrap-org-agenda-list)
    (nelisp-emacs-org-bridge--trace "load done")
    (setq nelisp-emacs-org-bridge-loaded t))
  t)

(provide 'nelisp-emacs-org-bridge)

;;; nelisp-emacs-org-bridge.el ends here
