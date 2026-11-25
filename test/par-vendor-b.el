;;; par-vendor-b.el --- GNU vendor Batch B behavior and provenance -*- lexical-binding: t; -*-

;; Exercise the image's active definitions without reloading them in NeLisp.
(unless (fboundp 'nemacs-runtime-dump-reinit)
  (dolist (feature '(charscript iso-transl case-table button easy-mmode help
                     hl-line newcomment skeleton so-long font-core keymap
                     wid-edit cus-face))
    (require feature)))

(defun par-vendor-b-owner-p (file symbol &optional kind)
  (let ((owner (symbol-file symbol (or kind 'defun))))
    (and (stringp owner)
         (member (file-name-nondirectory owner)
                 (list (concat file ".el") (concat file ".elc")))
         (or (not (fboundp 'nemacs-runtime-dump-reinit))
             (string-match-p "/vendor/emacs-lisp/" owner))
         (or (eq kind 'defvar)
             (and (fboundp symbol)
                  (not (autoloadp (symbol-function symbol)))))
         t)))

(defun par-vendor-b-feature-owner-p (file feature)
  ;; Generated charscript.el populates a C variable and defines no symbol.
  (let ((history load-history) found)
    (while (and history (not found))
      (let ((entry (car history)))
        (when (and (stringp (car entry))
                   (member (file-name-nondirectory (car entry))
                           (list (concat file ".el") (concat file ".elc")))
                   (or (not (fboundp 'nemacs-runtime-dump-reinit))
                       (string-match-p "/vendor/emacs-lisp/" (car entry)))
                   (member (cons 'provide feature) (cdr entry)))
          (setq found t)))
      (setq history (cdr history)))
    found))

(defmacro par-vendor-b-row (name owner &rest body)
  (declare (indent 2))
  `(let ((provenance ,owner))
     (prin1 (list ',name provenance
                  (condition-case err
                      (progn ,@body)
                    (error (list 'error (car err)
                                 (and (symbolp (cadr err)) (cadr err)))))))
     (terpri)))

(par-vendor-b-row charscript (par-vendor-b-feature-owner-p "charscript" 'charscript)
  (list (mapcar (lambda (char) (aref char-script-table char))
                '(#x41 #x17f #x3042 #x1f642 #x10fff))
        (length (char-table-extra-slot char-script-table 0))
        (aref (char-category-set #x2042) ?5)))

(par-vendor-b-row iso-transl (par-vendor-b-owner-p "iso-transl" 'iso-transl-define-keys)
  (let ((iso-transl-ctl-x-8-map (copy-keymap iso-transl-ctl-x-8-map))
        (key-translation-map (copy-keymap key-translation-map))
        (isearch-mode-map (copy-keymap isearch-mode-map)))
    (let ((standard (lookup-key iso-transl-ctl-x-8-map "~n")))
      (iso-transl-set-language "German")
      (list standard
            (lookup-key iso-transl-ctl-x-8-map "A")
            (lookup-key key-translation-map (vector (+ (ash 1 22) ?~) ?n))
            (lookup-key key-translation-map [dead-tilde ?n])))))

(par-vendor-b-row case-table (par-vendor-b-owner-p "case-table" 'copy-case-table)
  (let* ((table (make-char-table 'case-table))
         (saved (standard-case-table)))
    (set-char-table-range table ?A ?a)
    (set-char-table-range table ?a ?a)
    (let* ((up (case-table-get-table table 'up))
           (copy (copy-case-table table))
           (copy-up (char-table-extra-slot copy 0)))
      (list (eq saved (standard-case-table)) (char-table-p copy)
            (aref up ?a) (aref copy ?A) (aref copy-up ?a) (eq up copy-up)
            (char-table-extra-slot copy 1)
            (aref (case-table-get-table copy 'canon) ?A)))))

(par-vendor-b-row button (par-vendor-b-owner-p "button" 'button-start)
  (with-temp-buffer
    (insert-text-button "open" 'help-echo "Test button")
    (let ((button (button-at (point-min))))
      (list (button-start button) (button-end button)
            (button-label button) (button-get button 'help-echo)))))

(par-vendor-b-row easy-mmode (par-vendor-b-owner-p "easy-mmode" 'define-minor-mode)
  (eval '(define-minor-mode par-vendor-b-mode "Parity test mode." :lighter " PB") t)
  (with-temp-buffer
    (par-vendor-b-mode 1)
    (prog1 (list par-vendor-b-mode (local-variable-p 'par-vendor-b-mode)
                 (assq 'par-vendor-b-mode minor-mode-alist))
      (par-vendor-b-mode -1))))

(par-vendor-b-row help (par-vendor-b-owner-p "help" 'help-function-arglist)
  (list (help-function-arglist (lambda (first &optional second) (list first second)))
        (help-function-arglist '(lambda (x &rest xs) (cons x xs)))))

(par-vendor-b-row hl-line (par-vendor-b-owner-p "hl-line" 'global-hl-line-mode)
  (with-temp-buffer
    (insert "one\ntwo\n")
    (goto-char (point-min))
    (hl-line-mode 1)
    (hl-line-highlight)
    (prog1 (list hl-line-mode (overlay-start hl-line-overlay)
                 (overlay-end hl-line-overlay))
      (hl-line-mode -1))))

(par-vendor-b-row newcomment (par-vendor-b-owner-p "newcomment" 'comment-normalize-vars)
  (with-temp-buffer
    (setq-local comment-start "#")
    (setq-local comment-end "")
    (setq-local comment-padding 1)
    (comment-normalize-vars)
    (list comment-start comment-end (default-value 'comment-column))))

(par-vendor-b-row skeleton (par-vendor-b-owner-p "skeleton" 'skeleton-insert)
  (with-temp-buffer
    (skeleton-insert '(nil "left" _ "right"))
    (list (buffer-string) (point))))

(par-vendor-b-row so-long (par-vendor-b-owner-p "so-long" 'global-so-long-mode)
  (let ((mode-line-misc-info nil))
    (global-so-long-mode 1)
    (unwind-protect
        (list global-so-long-mode
              (and (advice-member-p #'so-long--set-auto-mode 'set-auto-mode) t))
      (global-so-long-mode -1))))

(par-vendor-b-row font-core (par-vendor-b-owner-p "font-core" 'font-lock-default-function)
  (with-temp-buffer
    (font-lock-mode -1)
    (list font-lock-mode (default-value 'font-lock-global-modes))))

(par-vendor-b-row keymap (par-vendor-b-owner-p "keymap" 'defvar-keymap)
  (let ((map (define-keymap "C-c x" #'ignore "M-a" #'forward-char)))
    (list (keymap-lookup map "C-c x") (keymap-lookup map "M-a")
          (key-valid-p "C-c x") (key-valid-p "C-c  x"))))

(par-vendor-b-row wid-edit (par-vendor-b-owner-p "wid-edit" 'widget-convert)
  (let ((widget (widget-convert '(string :tag "Name" :value "sample"))))
    (list (widget-type widget) (widget-get widget :tag) (widget-value widget))))

(par-vendor-b-row cus-face (par-vendor-b-owner-p "cus-face" 'custom-declare-face)
  (let ((face 'par-vendor-b-face))
    (custom-declare-face face '((t :weight bold)) "Parity test face.")
    (list (and (facep face) t) (face-attribute face :weight nil)
          (plist-get (custom-face-attributes-get face nil) :weight))))

(defun par-vendor-b-library-owner-p (feature file symbol)
  ;; Lazy libraries should resolve through the image's usual require path.
  (let ((inhibit-message t)
        (warning-minimum-level :error))
    (condition-case nil
        (and (require feature nil t)
             (par-vendor-b-owner-p file symbol))
      (error nil))))

(par-vendor-b-row cus-edit (par-vendor-b-library-owner-p 'cus-edit "cus-edit" 'custom-variable-type)
  (list (custom-variable-type 'fill-column)
        (keymapp custom-field-keymap)
        (keymapp custom-dirlocals-field-map)))

(par-vendor-b-row seq (par-vendor-b-owner-p "seq" 'seq-length)
  (let ((vector (vector 0 0))
        (list (list 1 2)))
    (setf (seq-elt vector 1) 9)
    (setf (seq-elt list 0) 9)
    (seq-let (a (b c) &rest rest) [1 (2 3) 4 5]
      (list vector list (seq-drop "abcd" 2) a b c rest))))

(par-vendor-b-row map (par-vendor-b-library-owner-p 'map "map" 'map-elt)
  (let ((table (map-into '((a . 1) (b . 2)) 'hash-table)))
    (map-let (a b) '((a . 1) (b . 2))
      (list a b (map-elt table 'b) (map-elt [1 2] -1 :missing)
            (map-elt "abc" 1) (map-values [3 2 1])))))

(par-vendor-b-row term (par-vendor-b-library-owner-p 'term "term" 'term-mode)
  (with-temp-buffer
    (term-mode)
    (list major-mode (term-in-line-mode) (ring-p term-input-ring))))

(par-vendor-b-row grep (par-vendor-b-library-owner-p 'grep "grep" 'grep-exit-message)
  (with-temp-buffer
    (let ((grep-num-matches-found 2))
      (insert "matches here")
      (list grep-hit-face (grep-exit-message 'exit 0 "")))))

(par-vendor-b-row add-log (par-vendor-b-library-owner-p 'add-log "add-log" 'add-log-iso8601-time-string)
  (add-log-iso8601-time-string (seconds-to-time 0) t))

(par-vendor-b-row log-edit (par-vendor-b-library-owner-p 'log-edit "log-edit" 'log-edit-extract-headers)
  (log-edit-extract-headers '(("Author" . "--author"))
                            "Summary: Example\nAuthor: Ada\n\nBody\n"))

(par-vendor-b-row ediff-init (par-vendor-b-library-owner-p 'ediff-init "ediff-init" 'ediff-move-overlay)
  (with-temp-buffer
    (insert "abc")
    (let ((overlay (make-overlay 1 1)))
      (ediff-move-overlay overlay 2 3)
      (list (overlay-start overlay) (overlay-end overlay)))))

(par-vendor-b-row transient (par-vendor-b-library-owner-p 'transient "transient" 'transient-bind-q-to-quit)
  (let ((transient-base-map (copy-keymap transient-base-map))
        (transient-sticky-map (copy-keymap transient-sticky-map))
        (transient-substitute-key-function nil))
    (transient-bind-q-to-quit)
    (list (lookup-key transient-base-map "q")
          (lookup-key transient-sticky-map "q"))))

(par-vendor-b-row cc-bytecomp
    (par-vendor-b-library-owner-p 'cc-bytecomp "cc-bytecomp" 'cc-bytecomp-boundp)
  (eval '(cc-bytecomp-boundp 'noninteractive) t))

(par-vendor-b-row cc-defs
    (par-vendor-b-library-owner-p 'cc-defs "cc-defs" 'c-major-mode-is)
  (let ((c-buffer-is-cc-mode 'c-mode))
    (list (eval '(c-major-mode-is 'c-mode) t)
          (stringp c-alpha) (stringp c-alnum))))

(par-vendor-b-row cc-vars
    (par-vendor-b-library-owner-p 'cc-vars "cc-vars" 'c-valid-offset)
  (mapcar (lambda (value) (and (c-valid-offset value) t))
          '(4 + [3] [3 4] (first + 4))))

(par-vendor-b-row cc-menus
    (par-vendor-b-library-owner-p 'cc-menus "cc-menus" 'cc-imenu-java-build-type-args-regex)
  (list (stringp (cc-imenu-java-build-type-args-regex 1))
        (consp cc-imenu-c++-generic-expression)))

(par-vendor-b-row cc-styles
    (par-vendor-b-library-owner-p 'cc-styles "cc-styles" 'c-add-style)
  (let ((c-style-alist (copy-tree c-style-alist)))
    (c-add-style "par-vendor-b" '((c-basic-offset . 7)))
    (list (and (assoc "gnu" c-style-alist) t)
          (cdr (assoc "par-vendor-b" c-style-alist)))))

(par-vendor-b-row warning-buffer t
  (let ((name " *par-vendor-b warnings*"))
    (unwind-protect
        (progn
          (display-warning 'par-vendor-b "Logged in the requested buffer." :warning name)
          (with-current-buffer name
            (goto-char (point-min))
            (list (and (search-forward "Logged in the requested buffer." nil t) t)
                  (eq buffer-undo-list t))))
      (when (get-buffer name) (kill-buffer name)))))

(par-vendor-b-row smie (par-vendor-b-library-owner-p 'smie "smie" 'smie-default-forward-token)
  (with-temp-buffer
    (insert "sample + next")
    (goto-char (point-min))
    (list (smie-default-forward-token) (smie-default-forward-token))))

(par-vendor-b-row viper-init (par-vendor-b-library-owner-p 'viper-init "viper-init" 'viper-window-display-p)
  (let ((window-system nil)) (viper-window-display-p)))

(par-vendor-b-row epa-hook (par-vendor-b-library-owner-p 'epa-hook "epa-hook" 'epa-file-name-p)
  (let ((auto-encryption-mode t))
    (mapcar (lambda (name) (and (epa-file-name-p name) t))
            '("report.gpg" "report.gpg.~1~" "report.txt"))))

(par-vendor-b-row epa-file (par-vendor-b-library-owner-p 'epa-file "epa-file" 'epa-file-enable)
  (let ((file-name-handler-alist nil) (auto-mode-alist nil)
        (hook (default-value 'find-file-hook)) (inhibit-message t))
    (unwind-protect
        (progn
          (set-default 'find-file-hook nil)
          (epa-file-enable)
          (list (car file-name-handler-alist) (default-value 'find-file-hook)
                (eq (caddar auto-mode-alist) 'epa-file)))
      (set-default 'find-file-hook hook))))

(defvar org-modules)
(defvar org-export-backends)
(let ((org-modules nil) (org-export-backends '(ascii html))
      (inhibit-message t) (warning-minimum-level :error))
  (par-vendor-b-row org (par-vendor-b-library-owner-p 'org "org" 'org-get-heading)
    (with-temp-buffer
      (insert "* TODO Headline :tag:\n")
      (org-mode)
      (goto-char (point-min))
      (list (org-get-heading t t t t) (org-get-todo-state))))

  (par-vendor-b-row ol (par-vendor-b-library-owner-p 'ol "ol" 'org-link-types)
    (and (member "file" (org-link-types)) t))

  (par-vendor-b-row ol-bibtex (par-vendor-b-library-owner-p 'ol-bibtex "ol-bibtex" 'org-bibtex-headline-format-default)
    (org-bibtex-headline-format-default '((:title . "Example"))))

  (par-vendor-b-row org-agenda (par-vendor-b-library-owner-p 'org-agenda "org-agenda" 'org-agenda-time-of-day-to-ampm)
    (let ((org-agenda-time-leading-zero t)) (org-agenda-time-of-day-to-ampm "15:09")))

  (par-vendor-b-row org-colview (par-vendor-b-library-owner-p 'org-colview "org-colview" 'org-columns-add-ellipses)
    (let ((org-columns-ellipses "...")) (org-columns-add-ellipses "abcdefghi" 6)))

  (par-vendor-b-row org-compat (par-vendor-b-library-owner-p 'org-compat "org-compat" 'org-sublist)
    (org-sublist '(a b c d) 1 3))

  (par-vendor-b-row org-element-ast (par-vendor-b-library-owner-p 'org-element-ast "org-element-ast" 'org-element-adopt)
    (let ((node (org-element-create 'paragraph nil)))
      (org-element-adopt node "text")
      (mapcar #'substring-no-properties (org-element-contents node))))

  (par-vendor-b-row org-element (par-vendor-b-library-owner-p 'org-element "org-element" 'org-element-at-point)
    (with-temp-buffer
      (insert "* Headline\n")
      (org-mode)
      (goto-char (point-min))
      (org-element-type (org-element-at-point))))

  (par-vendor-b-row org-fold-core (par-vendor-b-library-owner-p 'org-fold-core "org-fold-core" 'org-fold-core-folded-p)
    (with-temp-buffer
      (insert "Visible")
      (org-mode)
      (org-fold-core-folded-p (point-min))))

  (par-vendor-b-row org-fold (par-vendor-b-library-owner-p 'org-fold "org-fold" 'org-fold-show-subtree)
    (with-temp-buffer
      (insert "* Headline\nBody\n")
      (org-mode)
      (goto-char (point-min))
      (org-fold-hide-subtree)
      (let ((hidden (and (org-fold-core-folded-p (line-end-position)) t)))
        (org-fold-show-subtree)
        (list hidden (and (org-fold-core-folded-p (line-end-position)) t)))))

  (par-vendor-b-row org-inlinetask (par-vendor-b-library-owner-p 'org-inlinetask "org-inlinetask" 'org-inlinetask-outline-regexp)
    (let ((org-odd-levels-only nil) (org-inlinetask-min-level 4))
      (string-match-p (org-inlinetask-outline-regexp) "**** Task")))

  (par-vendor-b-row org-list (par-vendor-b-library-owner-p 'org-list "org-list" 'org-at-item-p)
    (with-temp-buffer
      (insert "- item\n")
      (goto-char (point-min))
      (and (org-at-item-p) t)))

  (par-vendor-b-row org-pcomplete (par-vendor-b-library-owner-p 'org-pcomplete "org-pcomplete" 'org-pcomplete-case-double)
    (org-pcomplete-case-double '("Org")))

  (par-vendor-b-row org-table (par-vendor-b-library-owner-p 'org-table "org-table" 'org-at-table-p)
    (with-temp-buffer
      (insert "| 1 | 2 |\n")
      (goto-char (point-min))
      (and (org-at-table-p) t)))

  (par-vendor-b-row ox-publish (par-vendor-b-library-owner-p 'ox-publish "ox-publish" 'org-publish-property)
    (org-publish-property :section '("example" :section "docs") "default")))

(par-vendor-b-row frame (par-vendor-b-owner-p "frame" 'frame-remove-geometry-params)
  (let ((frame-initial-geometry-arguments nil))
    (frame-remove-geometry-params '((name . "example") (left . 4) (width . 20)))))

(par-vendor-b-row bindings (par-vendor-b-owner-p "bindings" 'propertized-buffer-identification)
  (let ((text (car (propertized-buffer-identification "%b"))))
    (list (substring-no-properties text)
          (keymapp (get-text-property 0 'local-map text)))))

(par-vendor-b-row files (par-vendor-b-owner-p "files" 'file-relative-name)
  (list (file-relative-name "/tmp/example/report.org" "/tmp/example/")
        (file-name-sans-extension "x.tar.gz")
        (indirect-variable 'revert-buffer-in-progress-p)))
