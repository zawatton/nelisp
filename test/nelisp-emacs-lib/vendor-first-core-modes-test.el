;;; vendor-first-core-modes-test.el --- Vendor-first core mode smoke -*- lexical-binding: t; -*-

;;; Code:

(require 'nemacs-test-paths
         (expand-file-name "nemacs-test-paths"
                           (file-name-directory (or load-file-name buffer-file-name))))

(require 'ert)

(defconst vendor-first-core-modes-test--repo-root
  nemacs-test-paths-repo-root
  "Repository root for vendor-first core mode tests.")

(defconst vendor-first-core-modes-test--vendor-roots
  '("vendor/emacs-lisp" "vendor/emacs-lisp-api" "vendor/staged-emacs-lisp")
  "Vendored Emacs Lisp roots, in resolution order.")

(defconst vendor-first-core-modes-test--vendor-subdirs
  '("emacs-lisp" "textmodes" "vc" "calendar" "mail" "international"
    "url" "nxml" "gnus" "net")
  "Subdirectories of each vendor root that may hold libraries.")

(defun vendor-first-core-modes-test--vendor-load-path ()
  "Return load paths with vendored Emacs Lisp before package sources.
Only directories that exist are returned."
  (let (dirs)
    (dolist (root vendor-first-core-modes-test--vendor-roots)
      (let ((root-dir (expand-file-name
                       root vendor-first-core-modes-test--repo-root)))
        (push root-dir dirs)
        (dolist (sub vendor-first-core-modes-test--vendor-subdirs)
          (push (expand-file-name sub root-dir) dirs))))
    (append (delq nil (mapcar (lambda (dir) (and (file-directory-p dir) dir))
                              (nreverse dirs)))
            (nemacs-test-paths-source-dirs))))

(defun vendor-first-core-modes-test--resolve-source (relative)
  "Return the absolute path of vendored source RELATIVE.
\"src/NAME\" resolves under packages/*/src/.  \"vendor/emacs-lisp/REL\"
is tried under vendor/emacs-lisp, vendor/emacs-lisp-api and
vendor/staged-emacs-lisp, then as packages/*/src/BASENAME.  The host
Emacs lisp directory is never consulted; signal an error when no
vendored copy exists."
  (cond
   ((string-prefix-p "src/" relative)
    (nemacs-test-paths-source (substring relative 4)))
   ((string-prefix-p "vendor/emacs-lisp/" relative)
    (let ((rel (substring relative (length "vendor/emacs-lisp/"))))
      (or (catch 'found
            (dolist (root vendor-first-core-modes-test--vendor-roots)
              (let ((candidate (expand-file-name
                                rel (expand-file-name
                                     root
                                     vendor-first-core-modes-test--repo-root))))
                (when (file-exists-p candidate)
                  (throw 'found candidate))))
            (dolist (dir (nemacs-test-paths-source-dirs))
              (let ((candidate (expand-file-name
                                (file-name-nondirectory rel) dir)))
                (when (file-exists-p candidate)
                  (throw 'found candidate)))))
          (error "vendor-first: no vendored copy of %s" relative))))
   (t
    (let ((file (expand-file-name relative
                                  vendor-first-core-modes-test--repo-root)))
      (unless (file-exists-p file)
        (error "vendor-first: no vendored copy of %s" relative))
      file))))

(defun vendor-first-core-modes-test--load-source (relative)
  "Load vendored source file RELATIVE and return its absolute path."
  (let ((file (vendor-first-core-modes-test--resolve-source relative))
        (load-path (append (vendor-first-core-modes-test--vendor-load-path)
                           load-path)))
    (load file nil nil t)
    file))

(ert-deftest vendor-first-core-modes-test/text-dired-vc-load-vendor-source ()
  "Core text, Dired and VC modes load from vendored Emacs Lisp first."
  (let ((loaded-files
         (mapcar #'vendor-first-core-modes-test--load-source
	                 '("vendor/emacs-lisp-api/textmodes/text-mode.el"
	                   "vendor/emacs-lisp/dired.el"
	                   "vendor/emacs-lisp/vc/vc.el"
	                   "vendor/emacs-lisp/vc/vc-git.el"
	                   "vendor/emacs-lisp/emacs-lisp/eieio.el"))))
    (dolist (file loaded-files)
      (should (file-exists-p file)))
    (dolist (feature '(text-mode dired vc vc-git eieio))
      (should (featurep feature)))
    (dolist (symbol '(text-mode dired dired-mode dired-jump
	                                vc-dir vc-diff vc-print-log
	                                vc-git-registered vc-git-state
	                                defclass make-instance cl--find-class))
      (should (fboundp symbol)))))

(ert-deftest vendor-first-core-modes-test/lightweight-support-libraries-load-vendor-source ()
  "Small upstream libraries stay as vendor-first substrate gates."
  (let ((loaded-files
         (mapcar #'vendor-first-core-modes-test--load-source
                 '("vendor/emacs-lisp/button.el"
                   "vendor/emacs-lisp/thingatpt.el"
                   "src/case-table.el"
                   "vendor/emacs-lisp/help-macro.el"
                   "vendor/emacs-lisp/emacs-lisp/subr-x.el"
                   "vendor/emacs-lisp/cdl.el"
                   "vendor/emacs-lisp/emacs-lisp/regi.el"
                   "vendor/emacs-lisp/emacs-lisp/generator.el"
                   "vendor/emacs-lisp/emacs-lisp/avl-tree.el"
                   "vendor/emacs-lisp/hex-util.el"
                   "vendor/emacs-lisp/color.el"
                   "vendor/emacs-lisp/electric.el"
                   "vendor/emacs-lisp/emacs-lisp/lisp.el"
                   "vendor/emacs-lisp/emacs-lisp/map-ynp.el"
                   "vendor/emacs-lisp/international/charprop.el"
                   "vendor/emacs-lisp/international/charscript.el"
                   "vendor/emacs-lisp/international/emoji-labels.el"
                   "vendor/emacs-lisp/international/idna-mapping.el"
                   "vendor/emacs-lisp/international/iso-transl.el"
                   "src/emacs-translation-table.el"
                   "vendor/emacs-lisp/ansi-osc.el"
                   "vendor/emacs-lisp/url/url-vars.el"
                   "vendor/emacs-lisp/url/url-privacy.el"
                   "vendor/emacs-lisp/url/url-domsuf.el"
                   "vendor/emacs-lisp/url/url-file.el"
                   "vendor/emacs-lisp/url/url-auth.el"
                   "vendor/emacs-lisp/url/url-gw.el"
                   "vendor/emacs-lisp/emacs-lisp/inline.el"
                   "vendor/emacs-lisp/emacs-lisp/easymenu.el"
                   "vendor/emacs-lisp/emacs-lisp/let-alist.el"
                   "vendor/emacs-lisp/emacs-lisp/text-property-search.el"
                   "vendor/emacs-lisp/emacs-lisp/thunk.el"
                   "vendor/emacs-lisp/emacs-lisp/rmc.el"
                   "vendor/emacs-lisp/emacs-lisp/cursor-sensor.el"
                   "vendor/emacs-lisp/calendar/parse-time.el"
                   "vendor/emacs-lisp/calendar/iso8601.el"
                   "vendor/emacs-lisp/mail/mail-prsvr.el"
                   "vendor/emacs-lisp/gnus/mm-util.el"
                   "vendor/emacs-lisp/mail/rfc2047.el"
                   "vendor/emacs-lisp/mail/rfc2231.el"
                   "vendor/emacs-lisp/mail/rfc6068.el"
                   "vendor/emacs-lisp/url/url-parse.el"
                   "vendor/emacs-lisp/url/url-methods.el"
                   "vendor/emacs-lisp/url/url-proxy.el"
                   "vendor/emacs-lisp/url/url-history.el"
                   "vendor/emacs-lisp/url/url-util.el"
                   "vendor/emacs-lisp/url/url-cookie.el"
                   "vendor/emacs-lisp/net/puny.el"
                   "vendor/emacs-lisp/mail/ietf-drums.el"
                   "vendor/emacs-lisp/mail/rfc2045.el"
                   "vendor/emacs-lisp/mail/mail-parse.el"
                   "vendor/emacs-lisp/emacs-lisp/generate-lisp-file.el"
                   "vendor/emacs-lisp/url/url-expand.el"
                   "vendor/emacs-lisp/net/mailcap.el"
                   "vendor/emacs-lisp/url/url.el"
                   "vendor/emacs-lisp/mail/mailheader.el"
                   "vendor/emacs-lisp/emacs-lisp/benchmark.el"
                   "vendor/emacs-lisp/password-cache.el"
                   "vendor/emacs-lisp/widget.el"
                   "vendor/emacs-lisp/emacs-lisp/seq.el"
                   "vendor/emacs-lisp/emacs-lisp/map.el"
                   "vendor/emacs-lisp/emacs-lisp/ring.el"
                   "vendor/emacs-lisp/emacs-lisp/easy-mmode.el"
                   "vendor/emacs-lisp/emacs-lisp/derived.el"
                   "vendor/emacs-lisp/emacs-lisp/range.el"
                   "vendor/emacs-lisp/emacs-lisp/tabulated-list.el"
                   "vendor/emacs-lisp/font-core.el"
                   "vendor/emacs-lisp/hl-line.el"
                   "vendor/emacs-lisp/newcomment.el"
                   "vendor/emacs-lisp/replace.el"
                   "vendor/emacs-lisp/savehist.el"
                   "vendor/emacs-lisp/calendar/time-date.el"
                   "vendor/emacs-lisp/ansi-color.el"
                   "vendor/emacs-lisp/bookmark.el"
                   "vendor/emacs-lisp/files-x.el"
                   "vendor/emacs-lisp/rfn-eshadow.el"
                   "vendor/emacs-lisp/format-spec.el"
                   "vendor/emacs-lisp/keymap.el"
                   "vendor/emacs-lisp/char-fold.el"
                   "vendor/emacs-lisp/imenu.el"
                   "vendor/emacs-lisp/help-fns.el"
                   "vendor/emacs-lisp/isearch.el"
                   "vendor/emacs-lisp/rect.el"
                   "vendor/emacs-lisp/menu-bar.el"
                   "vendor/emacs-lisp/ehelp.el"
                   "vendor/emacs-lisp/man.el"
                   "vendor/emacs-lisp/icomplete.el"
                   "vendor/emacs-lisp/ibuffer.el"
                   "vendor/emacs-lisp/comint.el"
                   "vendor/emacs-lisp/shell.el"))))
    (dolist (file loaded-files)
      (should (file-exists-p file)))
    (dolist (feature '(button thingatpt case-table help-macro subr-x cdl regi
                              generator avl-tree hex-util color composite electric
                              charprop charscript emoji-labels idna-mapping iso-transl
                              emacs-translation-table ansi-osc url-vars url-privacy
                              url-domsuf url-file url-auth url-gw page inline easymenu
                              let-alist radix-tree text-property-search thunk rmc
                              obarray cursor-sensor parse-time iso8601 mail-prsvr
                              mm-util rfc2047 rfc2231 rfc6068 url-parse url-methods
                              url-proxy url-history url-util url-cookie puny ietf-drums
                              rfc2045 mail-parse generate-lisp-file url-expand mailcap
                              url mailheader dynamic-setting benchmark password-cache
                              widget syntax range seq map ring easy-mmode derived
                              time-date tabulated-list font-core hl-line newcomment
                              replace savehist ansi-color bookmark abbrev env files-x
                              uniquify rfn-eshadow format-spec keymap char-fold imenu
                              help-fns isearch rect mouse menu-bar ehelp man ibuf-macs
                              icomplete ibuffer comint shell))
      (should (featurep feature)))
    (dolist (symbol '(insert-button thing-at-point set-case-table
                              make-help-screen syntax-ppss string-trim cdl-get-file
                              regi-interpret iter-next avl-tree-enter encode-hex-string
                              array-mode color-rgb-to-hex compose-string
                              make-display-table display-line-numbers-mode
                              delimit-columns-region describe-text-properties
                              electric-indent-mode electric-pair-mode command-history
                              forward-sexp mark-defun map-y-or-n-p read-answer
                              iso-transl-define-keys iso-transl-set-language
                              textsec-suspicious-p kinsoku
                              latexenc-inputenc-to-coding-system make-translation-table
                              ansi-osc-filter-region glyphless-display-mode
                              word-wrap-whitespace-mode url-mime-charset-string
                              url-device-type url-domsuf-cookie-allowed-p
                              url-file-build-filename url-file-host-is-local-p
                              url-get-authentication url-auth-registered url-open-stream
                              url-gateway-nslookup-host url-irc url-ircs
                              url-file-handler forward-page reposition-window
                              define-inline easy-menu-create-menu let-alist
                              radix-tree-lookup text-property-search-forward thunk-force
                              fileloop-initialize read-multiple-choice obarray-get
                              cursor-sensor-mode kill-ring-deindent-mode
                              display-fill-column-indicator-mode parse-time-string
                              iso8601-parse mm-charset-to-coding-system
                              rfc2047-decode-string rfc2231-parse-string
                              rfc6068-parse-mailto-url url-generic-parse-url
                              url-scheme-get-property url-find-proxy-for-url url-data
                              url-history-update-url url-hexify-string
                              url-build-query-string url-domain url-cookie-store
                              url-cookie-retrieve url-cookie-generate-header-lines
                              url-mailto puny-encode-domain ietf-drums-strip
                              rfc2045-encode-string mail-header-parse
                              generate-lisp-file-trailer url-expand-file-name
                              mailcap-parse-mailcaps url-retrieve rfc2104-hash
                              mail-header-parse fill-flowed uudecode-decode-region
                              hack-read-symbol-shorthands
                              dynamic-setting-handle-config-changed-event benchmark-call
                              password-read-from-cache scroll-lock-mode list-threads
                              untabify rot13-string underline-region define-widget
                              seq-filter map-elt make-ring define-minor-mode
                              define-derived-mode forward-paragraph fill-region
                              tabulated-list-mode widget-create
                              font-lock-default-fontify-region global-hl-line-mode
                              comment-dwim sort-lines query-replace view-mode
                              recentf-mode savehist-mode save-place-mode range-normalize
                              time-to-seconds ansi-color-apply bookmark-set
                              bookmark-jump define-abbrev dabbrev-expand
                              completion-in-region-mode emacs-version
                              substitute-env-vars add-file-local-variable
                              uniquify-buffer-base-name minibuffer-electric-default-mode
                              delete-selection-mode file-name-shadow-mode format-spec
                              keymap-set char-fold-to-regexp imenu describe-function
                              isearch-forward skeleton-insert rectangle-mark-mode
                              kmacro-start-macro double-mode mouse-set-point
                              menu-bar-mode help-at-pt-string find-cmd
                              electric-help-mode man info-lookup-symbol
                              electric-buffer-list dirtrack-mode icomplete-mode ibuffer
                              comint-mode comint-run comint-send-input shell))
      (should (fboundp symbol)))
    (dolist (symbol '(ansi-osc-control-seq-regexp
                                     mail-parse-charset))
      (should (boundp symbol)))
    (should (equal (get-char-code-property ?a 'uppercase) ?A))
    (should (equal (get-char-code-property ?A 'lowercase) ?a))
    (should (equal (get-char-code-property ?a 'titlecase) ?A))
    (should (eq (get-char-code-property ?A 'bidi-class) 'L))
    (should (eq (get-char-code-property ?A 'general-category) 'Lu))
    (should (equal (get-char-code-property ?A 'decomposition) (list ?A)))
    (should (equal (get-char-code-property ?0 'decimal-digit-value) 0))
    (should (equal (get-char-code-property ?0 'digit-value) 0))
    (should (equal (get-char-code-property ?0 'numeric-value) 0))
    (should (equal (get-char-code-property ?A 'name) "LATIN CAPITAL LETTER A"))
    (should (equal (get-char-code-property ?0 'name) "DIGIT ZERO"))
    (should (equal (rfc6068-parse-mailto-url "mailto:a@example.test?subject=Hi")
                   '(("To" . "a@example.test") ("Subject" . "Hi"))))
    (should (equal (get-char-code-property ?ß 'special-uppercase) "SS"))))

(ert-deftest vendor-first-core-modes-test/eieio-basic-subclass-after-vendor-load ()
  "Vendor EIEIO should support the small subclass shape Magit later needs."
  (let ((load-path (append (vendor-first-core-modes-test--vendor-load-path)
                           load-path)))
    (vendor-first-core-modes-test--load-source
     "vendor/emacs-lisp/emacs-lisp/eieio.el")
    (defclass vendor-first-core-modes-test-parent nil
      ((name :initform nil)
       (value :initform nil :initarg :value)))
    (defclass vendor-first-core-modes-test-child
      (vendor-first-core-modes-test-parent)
      ((keymap :initform nil)))
    (defclass vendor-first-core-modes-test-override-child
      (vendor-first-core-modes-test-child)
      ((keymap :initform 'vendor-first-core-modes-test-map)))
    (defvar vendor-first-core-modes-test-section-type-alist nil)
    (defclass vendor-first-core-modes-test-abstract-child
      (vendor-first-core-modes-test-parent)
      ((keymap :initform 'vendor-first-core-modes-test-abstract-map))
      :abstract t)
    (defclass vendor-first-core-modes-test-file-child
      (vendor-first-core-modes-test-abstract-child)
      ((keymap :initform 'vendor-first-core-modes-test-file-map)
       (source :initform nil :initarg :source)
       (header :initform nil :initarg :header)
       (binary :initform nil :initarg :binary)
       (heading-highlight-face :initform 'vendor-first-core-modes-test-highlight)
       (heading-selection-face :initform 'vendor-first-core-modes-test-selection)))
    (defclass vendor-first-core-modes-test-module-child
      (vendor-first-core-modes-test-file-child)
      ((keymap :initform 'vendor-first-core-modes-test-module-map)
       (range :initform nil :initarg :range)))
    (defclass vendor-first-core-modes-test-hunk-child
      (vendor-first-core-modes-test-abstract-child)
      ((keymap :initform 'vendor-first-core-modes-test-hunk-map)
       (painted :initform nil)
       (fontified :initform nil)
       (refined :initform nil)
       (combined :initform nil :initarg :combined)
       (from-range :initform nil :initarg :from-range)
       (from-ranges :initform nil)
       (to-range :initform nil :initarg :to-range)
       (about :initform nil :initarg :about)
       (heading-highlight-face :initform 'vendor-first-core-modes-test-hunk-highlight)
       (heading-selection-face :initform 'vendor-first-core-modes-test-hunk-selection)))
    (defun vendor-first-core-modes-test--meta-hunk-p (section)
      (not (cdr (oref section value))))
    (defun vendor-first-core-modes-test--completion-table (collection)
      (lambda (string pred action)
        (if (eq action 'metadata)
            '(metadata (display-sort-function . identity))
          (complete-with-action action collection string pred))))
    (defclass vendor-first-core-modes-test-log-child
      (vendor-first-core-modes-test-parent)
      ((keymap :initform 'vendor-first-core-modes-test-log-map))
      :abstract t)
    (defclass vendor-first-core-modes-test-unpulled-child
      (vendor-first-core-modes-test-log-child)
      nil)
    (defclass vendor-first-core-modes-test-unpushed-child
      (vendor-first-core-modes-test-log-child)
      nil)
    (defclass vendor-first-core-modes-test-unmerged-child
      (vendor-first-core-modes-test-log-child)
      nil)
    (setf (alist-get 'abstract vendor-first-core-modes-test-section-type-alist)
          'vendor-first-core-modes-test-abstract-child)
    (setf (alist-get 'hunk vendor-first-core-modes-test-section-type-alist)
          'vendor-first-core-modes-test-hunk-child)
    (setf (alist-get 'unpulled vendor-first-core-modes-test-section-type-alist)
          'vendor-first-core-modes-test-unpulled-child)
    (setf (alist-get 'unmerged vendor-first-core-modes-test-section-type-alist)
          'vendor-first-core-modes-test-unmerged-child)
    (defclass vendor-first-core-modes-test-transient-like-child
      (vendor-first-core-modes-test-parent)
      ((reader :initform #'vendor-first-core-modes-test--vendor-load-path)
       (always-read :initform t)
       (set-value :initarg :set-value :initform #'set)))
    (should (cl--find-class 'vendor-first-core-modes-test-parent))
    (should (cl--find-class 'vendor-first-core-modes-test-child))
    (should (cl--find-class 'vendor-first-core-modes-test-override-child))
    (should (cl--find-class 'vendor-first-core-modes-test-abstract-child))
    (should (eq (alist-get 'abstract
                           vendor-first-core-modes-test-section-type-alist)
                'vendor-first-core-modes-test-abstract-child))
    (should (cl--find-class 'vendor-first-core-modes-test-file-child))
    (should (cl--find-class 'vendor-first-core-modes-test-module-child))
    (should (cl--find-class 'vendor-first-core-modes-test-hunk-child))
    (should (cl--find-class 'vendor-first-core-modes-test-unmerged-child))
    (should (eq (alist-get 'hunk vendor-first-core-modes-test-section-type-alist)
                'vendor-first-core-modes-test-hunk-child))
    (should (eq (alist-get 'unmerged
                           vendor-first-core-modes-test-section-type-alist)
                'vendor-first-core-modes-test-unmerged-child))
    (should (equal (funcall (vendor-first-core-modes-test--completion-table
                             nil)
                            "" nil 'metadata)
                   '(metadata (display-sort-function . identity))))
    (should (cl--find-class 'vendor-first-core-modes-test-transient-like-child))
    (should (object-of-class-p
             (make-instance 'vendor-first-core-modes-test-child)
             'vendor-first-core-modes-test-parent))
    (should (object-of-class-p
             (make-instance 'vendor-first-core-modes-test-override-child)
             'vendor-first-core-modes-test-child))
    (let ((object (make-instance
                   'vendor-first-core-modes-test-transient-like-child
                   :set-value #'ignore)))
      (should (object-of-class-p
               object 'vendor-first-core-modes-test-parent))
      (should (eq (slot-value object 'reader)
                  #'vendor-first-core-modes-test--vendor-load-path))
      (should (eq (slot-value object 'always-read) t))
      (should (eq (slot-value object 'set-value) #'ignore)))
    (let ((object (make-instance
                   'vendor-first-core-modes-test-module-child
                   :source 'source
                   :range 'range)))
      (should (object-of-class-p
               object 'vendor-first-core-modes-test-file-child))
      (should (eq (slot-value object 'source) 'source))
      (should (eq (slot-value object 'range) 'range)))
    (let ((object (make-instance
                   'vendor-first-core-modes-test-hunk-child
                   :combined t
                   :from-range 'from
                   :to-range 'to
                   :about 'about
                   :value '(meta))))
      (should (object-of-class-p
               object 'vendor-first-core-modes-test-abstract-child))
      (should (eq (slot-value object 'combined) t))
      (should (eq (slot-value object 'from-range) 'from))
      (should (eq (slot-value object 'to-range) 'to))
      (should (eq (slot-value object 'about) 'about))
      (should (vendor-first-core-modes-test--meta-hunk-p object)))))

(ert-deftest vendor-first-core-modes-test/text-mode-buffer-behavior ()
  "Exercise vendored text-mode code through real buffer primitives."
  (let ((load-path (append (vendor-first-core-modes-test--vendor-load-path)
                           load-path)))
    (vendor-first-core-modes-test--load-source
     "vendor/emacs-lisp-api/textmodes/text-mode.el")
    (with-temp-buffer
      (insert "Root\nbody\n")
      (goto-char (point-min))
      (text-mode)
      (should (eq major-mode 'text-mode))
      (should (derived-mode-p 'text-mode))
      (should (keymapp text-mode-map))
      (should (eq (current-local-map) text-mode-map)))))

(provide 'vendor-first-core-modes-test)

;;; vendor-first-core-modes-test.el ends here
