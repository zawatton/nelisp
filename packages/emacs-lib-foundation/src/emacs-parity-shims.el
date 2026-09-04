;;; emacs-parity-shims.el --- real-elisp parity shims for the audit replay -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; b1k13+ parity round: REAL elisp implementations (never no-op stubs) of the
;; functions/variables the user's real init references but that the runtime's
;; simplified substrate does not yet provide, using stock GNU Emacs semantics.
;; Each is guarded with `unless fboundp'/`unless boundp' so a package that later
;; provides the real definition still wins.  Loaded EARLY (before the user init)
;; from `nemacs-next-session.el'.
;;
;; Deferred (need core/interpreter support or a third-party package -- NOT
;; stubbed here): `add-variable-watcher' (needs a core set/makunbound hook),
;; `transient-font-lock-keywords' (transient.el data), general-charset
;; `make-char' (mule tables), `make-mail-user-agent' (not a real Emacs fn).

;;; Code:

;;;; --- small dependencies --------------------------------------------

(unless (fboundp 'macroexp-progn)
  (defun macroexp-progn (body)
    "Return an expression equivalent to `(progn ,@BODY)'."
    (if (cdr body) (cons 'progn body) (car body))))

(unless (fboundp 'gensym)
  (defvar gensym--counter 0)
  (defun gensym (&optional prefix)
    "Return a new uninterned symbol with PREFIX (default \"g\")."
    (make-symbol (format "%s%d" (or prefix "g")
                         (prog1 gensym--counter
                           (setq gensym--counter (1+ gensym--counter)))))))

(unless (fboundp 'purecopy) (defalias 'purecopy 'identity))

(unless (boundp 'fill-column)
  (defvar fill-column 70 "Column beyond which automatic line-wrapping happens."))

;;;; --- if-let*/when-let*/and-let*/while-let(*) (subr.el, Emacs 30) ----

(unless (fboundp 'internal--build-binding)
  (defun internal--build-binding (binding prev-var)
    "Check and build a single BINDING with PREV-VAR."
    (setq binding
          (cond
           ((symbolp binding) (list binding binding))
           ((null (cdr binding)) (list (make-symbol "s") (car binding)))
           ((eq '_ (car binding)) (list (make-symbol "s") (cadr binding)))
           (t binding)))
    (when (> (length binding) 2)
      (signal 'error (cons "`let' bindings can have only one value-form" binding)))
    (list (car binding) (list 'and prev-var (cadr binding)))))

(unless (fboundp 'internal--build-bindings)
  (defun internal--build-bindings (bindings)
    "Check and build conditional value forms for BINDINGS."
    (let ((prev-var t))
      (mapcar (lambda (binding)
                (let ((binding (internal--build-binding binding prev-var)))
                  (setq prev-var (car binding))
                  binding))
              bindings))))

(unless (fboundp 'if-let*)
  (defmacro if-let* (varlist then &rest else)
    "Bind per VARLIST (like `let*', stop on nil); eval THEN or ELSE."
    (declare (indent 2))
    (if varlist
        (list 'let* (setq varlist (internal--build-bindings varlist))
              (append (list 'if (car (car (last varlist))) then) else))
      (list 'let* () then))))

(unless (fboundp 'when-let*)
  (defmacro when-let* (varlist &rest body)
    "Bind per VARLIST (stop on nil); if all non-nil eval BODY."
    (declare (indent 1))
    (list 'if-let* varlist (macroexp-progn body))))

(unless (fboundp 'and-let*)
  (defmacro and-let* (varlist &rest body)
    "Like `when-let*'; with empty BODY returns the last binding's value."
    (declare (indent 1))
    (let (res)
      (if varlist
          (list 'let* (setq varlist (internal--build-bindings varlist))
                (append (list 'when (setq res (car (car (last varlist)))))
                        (or body (list res))))
        (append (list 'let* ()) (or body (list t)))))))

(unless (fboundp 'while-let)
  (defmacro while-let (spec &rest body)
    "Bind per SPEC (as in `if-let*'); while all non-nil, eval BODY and repeat."
    (declare (indent 1))
    (let ((done (gensym "done")))
      (list 'catch (list 'quote done)
            (list 'while t
                  (list 'if-let* spec (cons 'progn body)
                        (list 'throw (list 'quote done) nil)))))))

(unless (fboundp 'while-let*) (defalias 'while-let* 'while-let))

(unless (fboundp 'if-let)
  (defmacro if-let (spec then &rest else)
    "Like `if-let*' but accepts a single (SYMBOL VALUE) spec specially."
    (declare (indent 2))
    (when (and (<= (length spec) 2) (not (listp (car spec))))
      (setq spec (list spec)))
    (list 'if-let* spec then (macroexp-progn else))))

(unless (fboundp 'when-let)
  (defmacro when-let (spec &rest body)
    "Like `when-let*' with the legacy single-binding SPEC of `if-let'."
    (declare (indent 1))
    (list 'if-let spec (macroexp-progn body))))

;;;; --- static-if (subr.el, Emacs 30) ---------------------------------

(unless (fboundp 'static-if)
  (defmacro static-if (condition then-form &rest else-forms)
    "Expansion-time conditional: eval CONDITION now; keep THEN or ELSE."
    (declare (indent 2))
    (if (eval condition (and (boundp 'lexical-binding) lexical-binding))
        then-form
      (cons 'progn else-forms))))

;;;; --- docstring line filler (subr.el) -------------------------------

(unless (fboundp 'internal--fill-string-single-line)
  (defun internal--fill-string-single-line (str)
    "Fill STR to `fill-column' (bootstrap filler, no fill.el)."
    (if (< (length str) fill-column)
        str
      (let* ((limit (min fill-column (length str)))
             (fst (substring str 0 limit))
             (lst (substring str limit)))
        (cond ((string-match "\\( \\)$" fst)
               (setq fst (replace-match "\n" nil nil fst 1)))
              ((string-match "^ \\(.*\\)" lst)
               (setq fst (concat fst "\n"))
               (setq lst (match-string 1 lst)))
              ((string-match ".*\\( \\(.+\\)\\)$" fst)
               (setq lst (concat (match-string 2 fst) lst))
               (setq fst (replace-match "\n" nil nil fst 1))))
        (concat fst (internal--fill-string-single-line lst))))))

(unless (fboundp 'internal--format-docstring-line)
  (defun internal--format-docstring-line (string &rest objects)
    "Format one docstring line from STRING and OBJECTS."
    (when (string-match "\n" string)
      (error "Unable to fill string containing newline: %S" string))
    (internal--fill-string-single-line (apply #'format string objects))))

;;;; --- coding-system-p (mule semantics) ------------------------------

(defvar nemacs-parity--known-coding-systems
  '(no-conversion undecided prefer-utf-8 raw-text
    ascii us-ascii utf-8 utf-8-unix utf-8-dos utf-8-mac utf-8-emacs
    utf-16 utf-16le utf-16be utf-7 latin-1 iso-8859-1 iso-latin-1
    iso-2022-jp euc-jp shift_jis japanese-shift-jis cp932 sjis
    binary emacs-mule escape-quoted)
  "Fallback registry when full mule coding-system tables are unavailable.")

(unless (fboundp 'coding-system-p)
  (defun coding-system-p (object)
    "Return non-nil if OBJECT names a coding system (nil = no-conversion)."
    (and (or (null object) (symbolp object))
         (or (null object)
             (and (fboundp 'coding-system-list) (memq object (coding-system-list)))
             (and (get object 'coding-system) t)
             (and (memq object nemacs-parity--known-coding-systems) t))
         t)))

;;;; --- faces / custom ------------------------------------------------

(unless (fboundp 'define-obsolete-face-alias)
  (defun define-obsolete-face-alias (obsolete-face current-face when)
    "Make OBSOLETE-FACE an alias for CURRENT-FACE, marked obsolete since WHEN."
    (put obsolete-face 'face-alias current-face)
    (put obsolete-face 'obsolete-face (or when t))
    obsolete-face))

(unless (fboundp 'custom-add-to-group)
  (defun custom-add-to-group (group option widget)
    "To existing GROUP add a new OPTION of type WIDGET (no dup)."
    (let ((members (get group 'custom-group))
          (entry (list option widget)))
      (unless (member entry members)
        (put group 'custom-group (nconc members (list entry)))))))

(unless (fboundp 'custom-add-choice)
  (defun custom-add-choice (variable choice)
    "Add CHOICE to the `choice' custom type of VARIABLE."
    (let ((choices (get variable 'custom-type)))
      (when (eq (car-safe choices) 'choice)
        (put variable 'custom-type (append choices (list choice)))))))

(unless (fboundp 'set-face-inverse-video)
  (defun set-face-inverse-video (face inverse-video-p &optional frame)
    "Specify whether FACE is in inverse video."
    (set-face-attribute face frame :inverse-video inverse-video-p)))

;;;; --- keymaps -------------------------------------------------------

(unless (fboundp 'make-composed-keymap)
  (defun make-composed-keymap (maps &optional parent)
    "New keymap composed of MAPS (list or single keymap), inheriting PARENT."
    (append (cons 'keymap (if (keymapp maps) (list maps) maps)) parent)))

(unless (fboundp 'bindings--define-key)
  (defun bindings--define-key (map key item)
    "Define KEY in MAP from menu ITEM."
    (define-key map key
      (cond
       ((not (consp item)) item)
       ((keymapp item) item)
       ((stringp (car item))
        (if (keymapp (cdr item)) (cons (purecopy (car item)) (cdr item))
          (purecopy item)))
       ((eq 'menu-item (car item)) (purecopy item))
       (t item)))))

;;;; --- derived modes / completion / navigation -----------------------

(unless (fboundp 'derived-mode--flush)
  (defun derived-mode--flush (mode)
    "Flush the cached parent list of MODE and its followers."
    (put mode 'derived-mode--all-parents nil)
    (let ((followers (get mode 'derived-mode--followers)))
      (when followers
        (put mode 'derived-mode--followers nil)
        (mapc #'derived-mode--flush followers)))))

(unless (fboundp 'derived-mode-add-parents)
  (defun derived-mode-add-parents (mode extra-parents)
    "Add EXTRA-PARENTS (list of symbols) to the parents of MODE."
    (put mode 'derived-mode-extra-parents extra-parents)
    (derived-mode--flush mode)))

(unless (fboundp 'completion-table-case-fold)
  (defun completion-table-case-fold (table &optional dont-fold)
    "Return TABLE made case-insensitive (case-sensitive if DONT-FOLD)."
    (lambda (string pred action)
      (let ((completion-ignore-case (not dont-fold)))
        (complete-with-action action table string pred)))))

(unless (fboundp 'easy-mmode-define-navigation)
  (defmacro easy-mmode-define-navigation (base re &optional name endfun _narrowfun
                                               &rest body)
    "Define BASE-next and BASE-prev to navigate to places matching RE."
    (declare (indent 5))
    (let* ((base-name (symbol-name base))
           (prev-sym (intern (concat base-name "-prev")))
           (next-sym (intern (concat base-name "-next"))))
      (unless name (setq name base-name))
      (list 'progn
            (append
             (list 'defun next-sym '(&optional count)
                   (format "Go to the next COUNT'th %s." name)
                   '(interactive "p")
                   '(unless count (setq count 1))
                   (list 'if '(< count 0) (list prev-sym '(- count))
                         (list 'progn
                               (list 'if (list 'looking-at re) '(setq count (1+ count)))
                               (list 'if (list 'not (list 're-search-forward re nil t 'count))
                                     (list 'if (list 'looking-at re)
                                           (list 'goto-char (if endfun (list 'funcall (list 'function endfun)) '(point-max)))
                                           (list 'user-error "No next %s" name))
                                     '(goto-char (match-beginning 0))))))
             body)
            (append
             (list 'defun prev-sym '(&optional count)
                   (format "Go to the previous COUNT'th %s." name)
                   '(interactive "p")
                   '(unless count (setq count 1))
                   (list 'if '(< count 0) (list next-sym '(- count))
                         (list 'unless (list 're-search-backward re nil t 'count)
                               (list 'user-error "No previous %s" name))))
             body)))))

;;;; --- windows / mail / skeleton / misc macros -----------------------

(unless (fboundp 'with-selected-window)
  (defmacro with-selected-window (window &rest body)
    "Execute BODY with WINDOW selected, restoring selection afterward."
    (declare (indent 1) (debug t))
    (let ((wsym (make-symbol "save-win")))
      (list 'let (list (list wsym '(selected-window)))
            (list 'save-current-buffer
                  (list 'unwind-protect
                        (append (list 'progn (list 'select-window window ''norecord)) body)
                        (list 'when (list 'window-live-p wsym)
                              (list 'select-window wsym ''norecord))))))))

(unless (fboundp 'define-mail-user-agent)
  (defun define-mail-user-agent (symbol composefunc sendfunc
                                        &optional abortfunc hookvar)
    "Register SYMBOL as a mail-sending package for `mail-user-agent'."
    (put symbol 'composefunc composefunc)
    (put symbol 'sendfunc sendfunc)
    (put symbol 'abortfunc (or abortfunc #'kill-buffer))
    (put symbol 'hookvar (or hookvar 'mail-send-hook))))

(defvar skeleton-debug nil "Non-nil enables `define-skeleton' debug storage.")
(unless (fboundp 'define-skeleton)
  (defmacro define-skeleton (command documentation &rest skeleton)
    "Define user-configurable COMMAND that enters a statement skeleton."
    (declare (doc-string 2) (indent defun))
    (list 'progn
          (list 'put (list 'quote command) ''no-self-insert t)
          (append
           (list 'defun command '(&optional str arg)
                 (concat documentation "\n\nThis is a skeleton command (see `skeleton-insert').")
                 '(interactive "*P\nP"))
           (list (list 'when (list 'fboundp ''skeleton-proxy-new)
                       (list 'skeleton-proxy-new (list 'quote skeleton) 'str 'arg)))))))

;;;; --- make-char (ascii/unicode/8-bit/latin-1; errors otherwise) -----

(unless (fboundp 'make-char)
  (defun make-char (charset &optional c1 _c2)
    "Return a character of CHARSET at position code C1 (reduced shim)."
    (cond
     ((null charset) (or c1 0))
     ((eq charset 'ascii) (logand (or c1 0) #x7f))
     ((memq charset '(unicode ucs iso-10646-1)) (or c1 0))
     ((memq charset '(eight-bit eight-bit-graphic eight-bit-control))
      (+ #x3fff00 (logand (or c1 0) #xff)))
     ((memq charset '(latin-iso8859-1 iso-8859-1 iso-latin-1))
      (+ #x80 (logand (or c1 0) #x7f)))
     (t (error "make-char: unsupported charset %S in shim" charset)))))

;;;; --- org-release ---------------------------------------------------

(unless (fboundp 'org-release)
  (defun org-release () "The release version of Org." "9.6"))

;;;; --- VARIABLES -----------------------------------------------------

;; keymaps (empty sparse maps = safe shape for unloaded third-party pkgs)
(unless (boundp 'json-mode-map) (defvar json-mode-map (make-sparse-keymap)))
(unless (boundp 'evil-list-view-mode-map) (defvar evil-list-view-mode-map (make-sparse-keymap)))
(unless (boundp 'widget-field-keymap) (defvar widget-field-keymap (make-sparse-keymap)))
(unless (boundp 'widget-text-keymap) (defvar widget-text-keymap (make-sparse-keymap)))
(unless (boundp 'widget-link-keymap) (defvar widget-link-keymap (make-sparse-keymap)))
(unless (boundp 'vc-menu-map) (defvar vc-menu-map (make-sparse-keymap)))
(unless (boundp 'text-mode-abbrev-table)
  (defvar text-mode-abbrev-table
    (if (fboundp 'make-abbrev-table) (make-abbrev-table) nil)))

;; scalars / regexps (stock Emacs 30.1 values)
(unless (boundp 'meta-prefix-char) (defvar meta-prefix-char 27))
(unless (boundp 'require-final-newline) (defvar require-final-newline nil))
(unless (boundp 'remote-shell-program) (defvar remote-shell-program "ssh"))
(unless (boundp 'read-extended-command-predicate) (defvar read-extended-command-predicate nil))
(unless (boundp 'mm-auto-save-coding-system) (defvar mm-auto-save-coding-system 'utf-8-emacs))

;; org timestamp regexps (rx pre-expanded; no rx/org dependency)
(defconst nemacs-parity--org-ts-internal
  "[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?")
(unless (boundp 'org-ts-regexp-inactive)
  (defvar org-ts-regexp-inactive
    (concat "\\[\\(" nemacs-parity--org-ts-internal "\\)\\]")))
(unless (boundp 'org-ts-regexp-both)
  (defvar org-ts-regexp-both
    (concat "[[<]\\(" nemacs-parity--org-ts-internal "\\)[]>]")))

;; faces
(unless (boundp 'grep-hit-face)
  (defvar grep-hit-face
    (if (boundp 'compilation-info-face) compilation-info-face 'match)))

;;;; --- parity round 2 additions --------------------------------------

;; display var (buffer-local; nil default is correct)
(unless (boundp 'header-line-format) (defvar header-line-format nil))

;; files.el constant (stock value)
(unless (boundp 'locate-dominating-stop-dir-regexp)
  (defvar locate-dominating-stop-dir-regexp
    "\\`\\(?:[\\/][\\/][^\\/]+\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'"
    "Regexp of directory names at which `locate-dominating-file' stops."))

;; f.el path-equality predicate (real semantics)
(unless (fboundp 'f-same-p)
  (defun f-same-p (path-a path-b)
    "Return t if PATH-A and PATH-B point to the same file."
    (equal (file-truename (directory-file-name (expand-file-name path-a)))
           (file-truename (directory-file-name (expand-file-name path-b))))))
(unless (fboundp 'f-same?) (defalias 'f-same? 'f-same-p))

(provide 'emacs-parity-shims)

;;; emacs-parity-shims.el ends here
