;;; emacs-parity-misc.el --- real-elisp parity shims (misc, round 2) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; b1k13+ parity round: REAL elisp implementations (never no-op stubs) of the
;; NON-org, NON-CC-Mode void-functions the user's real init references but that
;; the runtime's simplified substrate does not yet provide.  Bodies are copied
;; from stock GNU Emacs 30.1 sources (or their MELPA `f.el' equivalents) to
;; preserve exact semantics.  Each is guarded with `unless fboundp'/`unless
;; boundp' so a package that later provides the real definition still wins.
;;
;; This file is a companion to `emacs-parity-shims.el' (round 1); it does NOT
;; re-implement anything defined there.  `org-*' and CC-Mode functions are
;; owned by other agents and are intentionally absent here.
;;
;; DEFERRED (need core/interpreter support or a whole third-party subsystem --
;; NOT stubbed here; see the report accompanying this change):
;;   `unicode-property-table-internal'  (needs C char-table / charprop data)
;;   `add-variable-watcher'             (needs a core set/makunbound hook)
;;   `plz-media-type:application/json'  (plz.el media-type object, package data)
;;   `nnheader-parse-nov'               (gnus overview subsystem: mail-header
;;                                       cl-defstruct + 5 nov reader macros +
;;                                       gnus decode/message-id helpers)
;;   `make-mutex'                       (threading primitive; lock/unlock/
;;                                       with-mutex companions absent)
;;   `markdown-code-block-at-point-p'   (markdown-mode syntax machinery)
;;   `visual-line-mode' / `global-visual-line-mode'  (define-minor-mode + remap
;;                                       keymap `visual-line-mode-map')
;;   `list-keyboard-macros'             (kmacro ring + tabulated-list-mode)
;;   `kill-compilation' / `define-compilation-mode'  (compile.el subsystem)
;;   `comment-indent' / `comment-kill' / `comment-set-column' /
;;   `comment-indent-new-line'          (newcomment.el subsystem)
;;   `diff-goto-source' / `ediff-merge-files' / `eldoc-print-current-symbol-info'
;;   `defined-colors'                   (tty/x color subsystem)
;;   `if--setter'                       (unknown provenance; cannot source real
;;                                       semantics)
;; `native-compile-async' is provided below as an explicit, graceful "native
;; compilation unavailable" no-op that returns nil (it is not a placeholder for
;; missing behaviour -- there simply is no native compiler in this runtime).

;;; Code:

;;;; --- tiny shared dependencies --------------------------------------

(unless (fboundp 'purecopy) (defalias 'purecopy 'identity))

;;;; --- url: url-file-directory / url-default-expander (url-*.el) ------

;; url-util.el
(unless (fboundp 'url-file-directory)
  (defun url-file-directory (file)
    "Return the directory part of FILE, for a URL."
    (cond
     ((null file) "")
     ((string-match "\\?" file)
      (url-file-directory (substring file 0 (match-beginning 0))))
     ((string-match "\\(.*\\(/\\|%2[fF]\\)\\)" file)
      (match-string 1 file)))))

;; url-expand.el
(unless (fboundp 'url-expander-remove-relative-links)
  (defun url-expander-remove-relative-links (name)
    (if (equal name "")
        ;; An empty name is a properly valid relative URL reference/path.
        ""
      ;; Strip . and .. from pathnames
      (let ((new (if (not (string-match "^/" name))
                     (concat "/" name)
                   name)))
        ;; If it ends with a '/.' or '/..', tack on a trailing '/' so that
        ;; the tests that follow are not too complicated in terms of
        ;; looking for '..' or '../', etc.
        (if (string-match "/\\.+$" new)
            (setq new (concat new "/")))
        ;; Remove '/./' first
        (while (string-match "/\\(\\./\\)" new)
          (setq new (concat (substring new 0 (match-beginning 1))
                            (substring new (match-end 1)))))
        ;; Then remove '/../'
        (while (string-match "/\\([^/]*/\\.\\./\\)" new)
          (setq new (concat (substring new 0 (match-beginning 1))
                            (substring new (match-end 1)))))
        ;; Remove cruft at the beginning of the string.
        (while (string-match "^/\\.\\.\\(/\\)" new)
          (setq new (substring new (match-beginning 1) nil)))
        new))))

;; url-expand.el.  Depends on the url-parse.el struct accessors (url-type,
;; url-portspec, url-port, url-host, url-user, url-filename, url-fullness,
;; url-path-and-query); those exist whenever the url package is loaded enough
;; for this expander to be dispatched.
(unless (fboundp 'url-default-expander)
  (defun url-default-expander (urlobj defobj)
    ;; The default expansion routine - urlobj is modified by side effect!
    (if (url-type urlobj)
        ;; Well, they told us the scheme, let's just go with it.
        nil
      (setf (url-type urlobj) (or (url-type urlobj) (url-type defobj)))
      (setf (url-portspec urlobj) (or (url-portspec urlobj)
                                      (and (string= (url-type urlobj)
                                                    (url-type defobj))
                                           (url-port defobj))))
      (if (not (string= "file" (url-type urlobj)))
          (setf (url-host urlobj) (or (url-host urlobj) (url-host defobj))))
      (if (string= "ftp"  (url-type urlobj))
          (setf (url-user urlobj) (or (url-user urlobj) (url-user defobj))))
      ;; If the object we're expanding from is full, then we are now full.
      (unless (url-fullness urlobj)
        (setf (url-fullness urlobj) (url-fullness defobj)))
      (let* ((pathandquery (url-path-and-query urlobj))
             (defpathandquery (url-path-and-query defobj))
             (file (car pathandquery))
             (query (or (cdr pathandquery)
                        (and (equal file "") (cdr defpathandquery)))))
        (if (string-match "^/" (url-filename urlobj))
            (setq file (url-expander-remove-relative-links file))
          ;; We use concat rather than expand-file-name to combine
          ;; directory and file name, since urls do not follow the same
          ;; rules as local files on all platforms.
          (setq file (url-expander-remove-relative-links
                      (if (equal file "")
                          (or (car (url-path-and-query defobj)) "")
                        (concat (url-file-directory (url-filename defobj))
                                file)))))
        (setf (url-filename urlobj) (if query (concat file "?" query) file))))))

;;;; --- vc: vc-find-root / vc-git-root / vc-hg-root (vc-hooks/*.el) ----

(unless (boundp 'vc-ignore-dir-regexp)
  (defvar vc-ignore-dir-regexp
    (if (boundp 'locate-dominating-stop-dir-regexp)
        locate-dominating-stop-dir-regexp
      "\\`\\(?:[\\/][\\/][^\\/]+\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")
    "Regexp matching directory names that are not under VC's control."))

;; vc-hooks.el
(unless (fboundp 'vc-find-root)
  (defun vc-find-root (file witness)
    "Find the root of a checked out project.
The function walks up the directory tree from FILE looking for WITNESS.
If WITNESS is not found, return nil, otherwise return the root."
    (let ((locate-dominating-stop-dir-regexp
           (or vc-ignore-dir-regexp
               (and (boundp 'locate-dominating-stop-dir-regexp)
                    locate-dominating-stop-dir-regexp))))
      (locate-dominating-file file witness))))

;; vc-git.el
(unless (fboundp 'vc-git-root)
  (defun vc-git-root (file)
    (vc-find-root file ".git")))

;; vc-hg.el
(unless (fboundp 'vc-hg-root)
  (defun vc-hg-root (file)
    (vc-find-root file ".hg")))

;;;; --- coding systems (international/mule*.el) ------------------------

;; international/mule-cmds.el
(unless (boundp 'sort-coding-systems-predicate)
  (defvar sort-coding-systems-predicate nil
    "If non-nil, a predicate function to sort coding systems.
It is called with two coding systems, and should return t if the first
one is \"less\" than the second."))

(unless (fboundp 'sort-coding-systems)
  (defun sort-coding-systems (codings)
    "Sort coding system list CODINGS by a priority of each coding system.
Return the sorted list.  CODINGS is modified by side effects.
If the variable `sort-coding-systems-predicate' is non-nil, it is used to
sort CODINGS instead."
    (cond
     (sort-coding-systems-predicate
      (sort codings sort-coding-systems-predicate))
     ;; Priority subsystem unavailable: degenerate (stable, no-reorder) sort.
     ((not (and (fboundp 'coding-system-priority-list)
                (fboundp 'coding-system-base)
                (fboundp 'coding-system-get)
                (fboundp 'coding-system-type)
                (fboundp 'coding-system-category)))
      codings)
     (t
      (let* ((from-priority (coding-system-priority-list))
             (most-preferred (car from-priority))
             (lang-preferred
              (and (fboundp 'get-language-info)
                   (boundp 'current-language-environment)
                   (get-language-info current-language-environment
                                      'coding-system)))
             (func (lambda (x)
                     (let ((base (coding-system-base x)))
                       ;; Priority number 0..255 as PMMLCEII bits:
                       ;; P: most preferred.  MM: mime-charset rank.
                       ;; L: current lang env.  C: in category list.
                       ;; E: not XXX-with-esc.  II: iso-2022 rank.
                       (logior
                        (ash (if (eq base most-preferred) 1 0) 7)
                        (ash
                         (let ((mime (coding-system-get base :mime-charset)))
                           (if mime
                               (cond ((string-match-p "utf-16"
                                                      (symbol-name mime))
                                      2)
                                     ((string-match-p "^x-"
                                                      (symbol-name mime))
                                      1)
                                     (t 3))
                             0))
                         5)
                        (ash (if (memq base lang-preferred) 1 0) 4)
                        (ash (if (memq base from-priority) 1 0) 3)
                        (ash (if (string-match-p "-with-esc\\'"
                                                 (symbol-name base))
                                 0 1)
                             2)
                        (if (eq (coding-system-type base) 'iso-2022)
                            (let ((category (coding-system-category base)))
                              (cond
                               ((or (eq category 'coding-category-iso-8-1)
                                    (eq category 'coding-category-iso-8-2))
                                2)
                               ((or (eq category 'coding-category-iso-7-tight)
                                    (eq category 'coding-category-iso-7))
                                1)
                               (t 0)))
                          1))))))
        (sort codings (lambda (x y)
                        (> (funcall func x) (funcall func y)))))))))

;; international/mule.el
(unless (boundp 'selection-coding-system)
  (defvar selection-coding-system nil
    "Coding system to encode/decode communication with other X clients."))

(unless (fboundp 'set-selection-coding-system)
  (defun set-selection-coding-system (coding-system)
    "Make CODING-SYSTEM used for communicating with other X clients."
    (interactive "zCoding system for X selection: ")
    (when (fboundp 'check-coding-system)
      (check-coding-system coding-system))
    (setq selection-coding-system coding-system)))

;;;; --- SMIE indentation helpers (emacs-lisp/smie.el) -----------------

;; Stock default tokenizer used by `smie-forward-token-function'.
(unless (fboundp 'smie-default-forward-token)
  (defun smie-default-forward-token ()
    (forward-comment (point-max))
    (buffer-substring-no-properties
     (point)
     (progn (if (zerop (skip-syntax-forward "."))
                (skip-syntax-forward "w_'"))
            (point)))))

(unless (boundp 'smie-forward-token-function)
  (defvar smie-forward-token-function #'smie-default-forward-token
    "Function to move forward by one token and return that token."))

(unless (boundp 'smie--hanging-eolp-function)
  (defvar smie--hanging-eolp-function
    (lambda ()
      (skip-chars-forward " \t")
      (or (eolp)
          (forward-comment (point-max))))
    "Function to skip to the end of the line, comments included."))

(unless (fboundp 'smie-indent--bolp)
  (defun smie-indent--bolp ()
    "Return non-nil if the current token is the first on the line."
    (save-excursion (skip-chars-backward " \t") (bolp))))

(unless (fboundp 'smie-rule-bolp)
  (defalias 'smie-rule-bolp #'smie-indent--bolp))

(unless (fboundp 'smie-indent--hanging-p)
  (defun smie-indent--hanging-p ()
    "Return non-nil if the current token is \"hanging\".
A hanging keyword is one that's at the end of a line except it's not at
the beginning of a line."
    (and (not (smie-indent--bolp))
         (save-excursion
           (<= (line-end-position)
               (progn
                 (and (zerop (length (funcall smie-forward-token-function)))
                      (not (eobp))
                      ;; Could be an open-paren.
                      (forward-char 1))
                 (funcall smie--hanging-eolp-function)
                 (point)))))))

(unless (fboundp 'smie-rule-hanging-p)
  (defalias 'smie-rule-hanging-p #'smie-indent--hanging-p))

;;;; --- mode-line (bindings.el) ---------------------------------------

(unless (boundp 'mode-line-buffer-identification-keymap)
  (defvar mode-line-buffer-identification-keymap (make-sparse-keymap)
    "Keymap for what is displayed by `mode-line-buffer-identification'."))

(unless (fboundp 'propertized-buffer-identification)
  (defun propertized-buffer-identification (fmt)
    "Return a list suitable for `mode-line-buffer-identification'.
FMT is a format specifier such as \"%12b\".  This function adds
text properties for face, help-echo, and local-map to it."
    (list (propertize fmt
                      'face 'mode-line-buffer-id
                      'help-echo
                      (purecopy "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer")
                      'mouse-face 'mode-line-highlight
                      'local-map mode-line-buffer-identification-keymap))))

;;;; --- pcomplete (pcomplete.el) --------------------------------------

(unless (fboundp 'pcomplete-uniquify-list)
  (defun pcomplete-uniquify-list (sequence)
    "Sort and remove multiples in SEQUENCE.
SEQUENCE should be a vector or list of strings."
    (sort (delete-dups (append sequence nil)) #'string-lessp)))

(unless (fboundp 'pcomplete-uniqify-list)     ; obsolete alias, since 27.1
  (defalias 'pcomplete-uniqify-list #'pcomplete-uniquify-list))

;;;; --- tree-sitter font-lock rules (treesit.el) ----------------------

;; When tree-sitter is not compiled into the runtime, `treesit-available-p'
;; returns nil and `treesit-font-lock-rules' short-circuits to nil -- which is
;; exactly stock behaviour (font-lock rules are only meaningful with a live
;; tree-sitter parser).
(unless (fboundp 'treesit-available-p)
  (defun treesit-available-p ()
    "Return non-nil if tree-sitter support is built and available.
This runtime is not built with tree-sitter, so this returns nil."
    nil))

(unless (fboundp 'treesit-font-lock-rules)
  (defun treesit-font-lock-rules (&rest query-specs)
    "Return a value suitable for `treesit-font-lock-settings'.
QUERY-SPECS is a series of QUERY-SPECs; each QUERY is preceded by pairs of
:KEYWORD VALUE (notably :language and :feature).  See the stock docstring."
    ;; This is usually called inside a `defvar', which runs regardless of
    ;; whether tree-sitter is enabled, so we guard on availability.
    (when (treesit-available-p)
      (let (current-language current-override
            current-feature
            default-language
            (result nil))
        (while query-specs
          (let ((token (pop query-specs)))
            (pcase token
              (:default-language
               (let ((lang (pop query-specs)))
                 (when (or (not (symbolp lang)) (null lang))
                   (signal 'treesit-font-lock-error
                           `("Value of :default-language should be a symbol"
                             ,lang)))
                 (setq default-language lang)))
              (:language
               (let ((lang (pop query-specs)))
                 (when (or (not (symbolp lang)) (null lang))
                   (signal 'treesit-font-lock-error
                           `("Value of :language should be a symbol" ,lang)))
                 (setq current-language lang)))
              (:override
               (let ((flag (pop query-specs)))
                 (when (not (memq flag '(t nil append prepend keep)))
                   (signal 'treesit-font-lock-error
                           `("Value of :override should be one of t, nil, append, prepend, keep"
                             ,flag))
                   (signal 'wrong-type-argument
                           `((or t nil append prepend keep) ,flag)))
                 (setq current-override flag)))
              (:feature
               (let ((var (pop query-specs)))
                 (when (or (not (symbolp var)) (memq var '(t nil)))
                   (signal 'treesit-font-lock-error
                           `("Value of :feature should be a symbol" ,var)))
                 (setq current-feature var)))
              ((pred treesit-query-p)
               (let ((lang (or default-language current-language)))
                 (when (null lang)
                   (signal 'treesit-font-lock-error
                           `("Language unspecified, use :language keyword or :default-language to specify a language for this query" ,token)))
                 (when (null current-feature)
                   (signal 'treesit-font-lock-error
                           `("Feature unspecified, use :feature keyword to specify the feature name for this query" ,token)))
                 (if (treesit-compiled-query-p token)
                     (push `(,lang token) result)
                   (push `(,(treesit-query-compile lang token)
                           t
                           ,current-feature
                           ,current-override)
                         result))
                 (setq current-language nil
                       current-override nil
                       current-feature nil)))
              (_ (signal 'treesit-font-lock-error
                         `("Unexpected value" ,token))))))
        (nreverse result)))))

;;;; --- cl-generic generalizers (emacs-lisp/cl-generic.el) ------------

;; Fallback constructor if the cl--generic-generalizer struct is not loaded.
(unless (fboundp 'cl-generic-make-generalizer)
  (defun cl-generic-make-generalizer (name priority tagcode-function
                                           specializers-function)
    "Build a generalizer record (minimal, matches the cl-defstruct layout)."
    (record 'cl--generic-generalizer name priority
            tagcode-function specializers-function)))

(unless (fboundp 'cl-generic-define-generalizer)
  (defmacro cl-generic-define-generalizer (name priority tagcode-function
                                                specializers-function)
    "Define a new kind of generalizer.
NAME is the name of the variable that will hold it.  PRIORITY defines which
generalizer takes precedence.  TAGCODE-FUNCTION returns code computing the tag
of a value; SPECIALIZERS-FUNCTION maps a tag to matching specializers."
    (declare (indent 1) (debug (symbolp body)))
    `(defconst ,name
       (cl-generic-make-generalizer
        ',name ,priority ,tagcode-function ,specializers-function))))

;;;; --- widgets (wid-edit.el) -----------------------------------------

(unless (fboundp 'define-widget)
  (defun define-widget (name class doc &rest args)
    "Define a new widget type named NAME from CLASS.
CLASS should be an existing widget type, or nil to create from scratch.
DOC is a documentation string for the widget."
    (declare (doc-string 3) (indent defun))
    (unless (or (null doc) (stringp doc))
      (error "Widget documentation must be nil or a string"))
    (put name 'widget-type (cons class args))
    (put name 'widget-documentation (purecopy doc))
    name))

;;;; --- cl-eval-when (emacs-lisp/cl-macs.el) --------------------------

;; This runtime always interprets (never byte-/native-compiles user init), so
;; `macroexp-compiling-p' is effectively nil and the stock macro reduces to its
;; interpreted branch: run BODY only when `eval'/`:execute' is requested.
(unless (fboundp 'cl-eval-when)
  (defmacro cl-eval-when (when &rest body)
    "Control when BODY is evaluated.
If `eval' (or `:execute') is in WHEN, BODY is evaluated when interpreted.

\(fn (WHEN...) BODY...)"
    (declare (indent 1) (debug (sexp body)))
    (and (or (memq 'eval when) (memq :execute when))
         (cons 'progn body))))

;;;; --- processes (get-process) ---------------------------------------

;; C primitive; reconstruct via `process-list'.  Returns nil when there is no
;; such process, which is correct real semantics for an empty process table.
(unless (fboundp 'get-process)
  (defun get-process (name)
    "Return the process named NAME (a string), or nil if there is none.
NAME may also be a process; if so, the value is that process."
    (cond
     ((and (fboundp 'processp) (processp name)) name)
     ((not (stringp name)) nil)
     ((fboundp 'process-list)
      (catch 'found
        (dolist (p (process-list))
          (when (and (fboundp 'process-name) (equal (process-name p) name))
            (throw 'found p)))
        nil))
     (t nil))))

;;;; --- f.el file helpers (MELPA `f') ---------------------------------

(unless (fboundp 'f-read-text)
  (defun f-read-text (path &optional coding)
    "Read the text contents of file at PATH using CODING (default `utf-8')."
    (with-temp-buffer
      (let ((coding-system-for-read (or coding 'utf-8)))
        (insert-file-contents path))
      (buffer-string))))

(unless (fboundp 'f-write-text)
  (defun f-write-text (text coding path)
    "Write TEXT to PATH using CODING, replacing any existing contents."
    (with-temp-buffer
      (insert text)
      (let ((coding-system-for-write coding))
        (write-region (point-min) (point-max) path nil 'silent)))))

(unless (fboundp 'f-append-text)
  (defun f-append-text (text coding path)
    "Append TEXT to PATH using CODING."
    (with-temp-buffer
      (insert text)
      (let ((coding-system-for-write coding))
        (write-region (point-min) (point-max) path :append 'silent)))))

(unless (fboundp 'f-dirname)
  (defun f-dirname (path)
    "Return the parent directory to PATH (nil if PATH is the root)."
    (let ((parent (file-name-directory
                   (directory-file-name (expand-file-name path)))))
      (unless (and (fboundp 'f-same-p) (f-same-p path parent))
        (if (file-name-absolute-p path)
            (directory-file-name parent)
          (file-relative-name parent))))))

(unless (fboundp 'f-parent) (defalias 'f-parent #'f-dirname))

;;;; --- faces (faces.el) ----------------------------------------------

(unless (fboundp 'face-differs-from-default-p)
  (defun face-differs-from-default-p (face &optional frame)
    "Return non-nil if FACE displays differently from the default face.
If FRAME is given, report on face FACE in that frame.  If FRAME is t, report
on the defaults for face FACE.  If FRAME is nil, use the selected frame."
    (let ((attrs
           (delq :inherit
                 (delq :extend (mapcar 'car face-attribute-name-alist))))
          (differs nil))
      (while (and attrs (not differs))
        (let* ((attr (pop attrs))
               (attr-val (face-attribute face attr frame t)))
          (when (and
                 (not (eq attr-val 'unspecified))
                 (display-supports-face-attributes-p (list attr attr-val)
                                                     frame))
            (setq differs attr))))
      differs)))

;;;; --- connection-local profiles (files-x.el) ------------------------

(unless (boundp 'connection-local-profile-alist)
  (defvar connection-local-profile-alist nil
    "Alist mapping connection profiles to variable lists."))

(unless (boundp 'connection-local-criteria-alist)
  (defvar connection-local-criteria-alist nil
    "Alist mapping connection criteria to a list of profile names."))

(unless (fboundp 'connection-local-normalize-criteria)
  (defsubst connection-local-normalize-criteria (criteria)
    "Normalize plist CRITERIA according to properties.
Return a reordered plist."
    (mapcan (lambda (property)
              (let ((value (plist-get criteria property)))
                (and value (list property value))))
            '(:application :protocol :user :machine))))

(unless (fboundp 'connection-local-set-profile-variables)
  (defun connection-local-set-profile-variables (profile variables)
    "Map the symbol PROFILE to a list of variable settings VARIABLES."
    (setf (alist-get profile connection-local-profile-alist) variables)))

(unless (fboundp 'connection-local-set-profiles)
  (defun connection-local-set-profiles (criteria &rest profiles)
    "Add PROFILES for CRITERIA (a plist identifying a connection)."
    (unless (listp criteria)
      (error "Wrong criteria `%s'" criteria))
    (dolist (profile profiles)
      (unless (assq profile connection-local-profile-alist)
        (error "No such connection profile `%s'" (symbol-name profile))))
    (let* ((criteria (connection-local-normalize-criteria criteria))
           (slot (assoc criteria connection-local-criteria-alist)))
      (if slot
          (setcdr slot (delete-dups (append (cdr slot) profiles)))
        (setq connection-local-criteria-alist
              (cons (cons criteria (delete-dups profiles))
                    connection-local-criteria-alist))))))

;;;; --- frame.el: headless blink-cursor-mode ---------------------------

;; A standalone NeLisp process has no cursor renderer or timer to manage, but
;; packages legitimately disable this global mode during init.  Preserve the
;; GNU global-minor-mode calling convention and state variable while making
;; the display side effect explicitly empty.  The standalone marker is
;; required in addition to fboundp gating so host Emacs is never replaced.
(when (and (fboundp 'nelisp--write-stdout-bytes)
           (not (fboundp 'blink-cursor-mode)))
  (defvar blink-cursor-mode nil
    "Non-nil when cursor blinking is enabled.")
  (defun blink-cursor-mode (&optional arg)
    "Toggle cursor blinking state in the headless standalone runtime.
With optional ARG, enable the mode if ARG is nil or positive, and disable it
if ARG is zero or negative.  The interactive no-prefix sentinel `toggle'
toggles the current state."
    (interactive
     (list (if current-prefix-arg
               (prefix-numeric-value current-prefix-arg)
             'toggle)))
    (setq blink-cursor-mode
          (cond
           ((eq arg 'toggle) (not blink-cursor-mode))
           ((and (numberp arg) (< arg 1)) nil)
           (t t)))))

;; `emacs-parity-macros2.el' is hoisted before `generator.el' for bootstrap
;; parsing, while this miscellaneous parity layer naturally follows it.  Put
;; the saved finite-generator shim back after GNU generator has overwritten it.
(when (and (fboundp 'nelisp--write-stdout-bytes)
           (fboundp 'emacs-parity-macros2--restore-iter-shims))
  (emacs-parity-macros2--restore-iter-shims))

;;;; --- native compilation (explicit "unavailable" graceful no-op) ----

;; This runtime has no native compiler.  Rather than leave `native-compile-async'
;; void (a caught error), we define it to do nothing and return nil -- the same
;; observable result as an Emacs where `native-comp-available-p' is nil.
(unless (fboundp 'native-compile-async)
  (defun native-compile-async (_files &optional _recursively _load _selector)
    "Native compilation is unavailable in this runtime; do nothing.
Accepts the stock signature FILES &optional RECURSIVELY LOAD SELECTOR and
returns nil (no asynchronous compilation is scheduled)."
    nil))

(provide 'emacs-parity-misc)

;;; emacs-parity-misc.el ends here
