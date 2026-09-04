;;; emacs-stub-bulk.el --- Auto-generated bulk stubs (macro-aware) -*- lexical-binding: t; -*-
;; Doc 51 Phase 3-A''-3 batch 6.  Macros emitted as defmacro stubs returning nil;
;; functions emitted as defun stubs returning nil; vars as defvar nil.
;;; Code:

;; auto-mode-alist moved to emacs-mode-builtins (Track H).

;; Pure value-passing functions must NEVER fall into the nil no-op
;; bulk below — a nil-returning `identity' silently breaks every
;; `(mapconcat 'identity ...)' pipeline (s.el's s-join returned
;; "nil-nil-..." on the standalone reader, found 2026-06-11).
(unless (fboundp 'float)
  (defun float (x)
    "Numeric coercion (never a nil no-op: same hazard as `identity')."
    (+ x 0.0)))
(unless (fboundp 'floor)
  (defun floor (x &optional d)
    ;; integer pair: exact flooring division without floats (the
    ;; reader's float division/`integerp' are unreliable - recorded
    ;; caveat); float input falls back to truncate-and-adjust
    (if (and d (integerp x) (integerp d))
        (let ((q (/ x d)))
          (if (and (/= (* q d) x) (< (* x d) 0))
              (1- q)
            q))
      (progn
        (when d (setq x (/ (float x) d)))
        (if (integerp x)
            x
          (let ((tr (truncate x)))
            (if (> (float tr) x) (1- tr) tr)))))))
(unless (fboundp 'ceiling)
  (defun ceiling (x &optional d)
    (when d (setq x (/ (float x) d)))
    (if (integerp x)
        x
      (let ((tr (truncate x)))
        (if (< (float tr) x) (1+ tr) tr)))))
(unless (fboundp 'round)
  (defun round (x &optional d)
    (when d (setq x (/ (float x) d)))
    (if (integerp x)
        x
      (truncate (if (< x 0) (- x 0.5) (+ x 0.5))))))
(unless (fboundp 'identity)
  (defun identity (arg)
    "Return ARG unchanged."
    arg))
(unless (fboundp 'int-to-string)
  (defun int-to-string (n)
    "Alias of `number-to-string' (same nil no-op hazard as `identity')."
    (number-to-string n)))

(unless (fboundp 'backward-char)
  (defun backward-char (&optional n)
    "Doc 51 Track B (2026-05-04) MVP `backward-char'.
Forwarder to `forward-char' with negated count."
    (when (fboundp 'forward-char)
      (forward-char (- (or n 1))))))

;; buffer-undo-list moved to emacs-undo-builtins (Track E.2).

;; call-interactively moved to emacs-command-loop-builtins (Phase B.3).
;; call-process / call-process-region moved to emacs-process-builtins (Track I).

;; current-prefix-arg moved to emacs-command-loop-builtins (Phase B.3).

(unless (fboundp 'decode-coding-string)
  ;; NeLisp strings are UTF-8 internally, so UTF-8 encode/decode is
  ;; identity. 他の coding system は未対応
  ;; (= encoded-coding-system 引数は無視)。
  (defun decode-coding-string (string _coding-system &optional _nocopy &rest _)
    (if (stringp string) string "")))

;; define-derived-mode moved to emacs-mode-builtins (Track H).

;; digit-argument moved to emacs-command-loop-builtins (Phase B.5).

(unless (fboundp 'encode-coding-string)
  (defun encode-coding-string (string _coding-system &optional _nocopy &rest _)
    (if (stringp string) string "")))

(unless (fboundp 'with-eval-after-load) (defmacro with-eval-after-load (_file &rest _body) nil))

;; execute-extended-command moved to emacs-command-loop-builtins (Phase B.5).

;; exit-recursive-edit moved to emacs-command-loop-builtins (Phase B.6).

;; face-background moved to emacs-faces-builtins (Track F).

;; face-foreground moved to emacs-faces-builtins (Track F).

;; fill-column / fill-region moved to emacs-textmodes-stub.el (Phase
;; 4 'C', 2026-05-06): the previous no-op `fill-region' broke MELPA
;; packages routing word-wrap through `with-temp-buffer' + `fill-
;; region' (= s.el's `s-word-wrap' canonical example).  The new
;; module provides a real greedy word-wrap polyfill.

;; force-mode-line-update moved to emacs-redisplay-builtins (Track G).

;; funcall-interactively moved to emacs-command-loop-builtins (Phase B.3).

;; fundamental-mode moved to emacs-mode-builtins (Track H).

;; inhibit-quit moved to emacs-command-loop-builtins (Phase B.1).

;; keyboard-quit moved to emacs-command-loop-builtins (Phase B.6).

;; kill-all-local-variables moved to emacs-mode-builtins (Track H).

;; last-command-event / last-input-event / last-nonmenu-event moved to
;; emacs-command-loop-builtins (Phase B.1).

;; major-mode moved to emacs-mode-builtins (Track H).

;; make-process moved to emacs-process-builtins (Track I).

;; mode-name moved to emacs-mode-builtins (Track H).

(unless (fboundp 'multibyte-string-p)
  ;; NeLisp の Sexp::Str は内部 UTF-8 — 上位 ASCII bit を含む文字が
  ;; あれば multibyte 扱い。pure-ASCII なら nil を返す。
  (defun multibyte-string-p (string)
    (when (stringp string)
      (let ((i 0) (n (length string)) found)
        (while (and (not found) (< i n))
          (when (>= (aref string i) 128) (setq found t))
          (setq i (1+ i)))
        found))))

;; negative-argument moved to emacs-command-loop-builtins (Phase B.5).

;; post-command-hook moved to emacs-command-loop-builtins (Phase B.4).

;; pre-command-hook moved to emacs-command-loop-builtins (Phase B.4).
;; prefix-arg moved to emacs-command-loop-builtins (Phase B.3).

;; primitive-undo moved to emacs-undo-builtins (Track E.2).

;; process-buffer moved to emacs-process-builtins (Track I).

;; processp moved to emacs-process-builtins (Track I).

;; process-send-string moved to emacs-process-builtins (Track I).

;; process-status moved to emacs-process-builtins (Track I).

;; quit-flag moved to emacs-command-loop-builtins (Phase B.1).

;; read-char / read-command / read-event moved to
;; emacs-command-loop-builtins (Phase B.1).

;; read-key-sequence{,-vector} moved to emacs-command-loop-builtins (Phase B.2).

;; real-this-command moved to emacs-command-loop-builtins (Phase B.1).

;; redisplay moved to emacs-redisplay-builtins (Track G).

;; set-face-background moved to emacs-faces-builtins (Track F).

;; set-face-foreground moved to emacs-faces-builtins (Track F).

;; shell-command-switch moved to emacs-process-builtins (Track I).

;; this-command / this-command-keys family / throw-on-input moved to
;; emacs-command-loop-builtins (Phase B.1).

;; top-level moved to emacs-command-loop-builtins (Phase B.4).

;; undo moved to emacs-undo-builtins (Track E.2).

;; undo-boundary moved to emacs-undo-builtins (Track E.2).

;; unread-command-events moved to emacs-command-loop-builtins (Phase B.1).

;; `window-system' moved to emacs-stub.el's display capability map
;; (Phase 1.E 2026-05-05) — it now consults `emacs-display-system'
;; instead of always returning nil.

;; Phase 11.D batch — trivial stubs collapsed into 3 dolist forms.
;; Reason: nelisp standalone interpreter charges ~47ms per top-level
;; form for the original `(unless (fboundp X) (defun X (&rest _) nil))'
;; idiom; batching 698 defun + 25 defmacro + 122 defvar stubs
;; through a single dolist body drops emacs-stub-bulk load time from
;; ~40s to ~6s (= 7x speedup) and unblocks bootstrap iteration.
;; Under host Emacs the symbols' fboundp / boundp gates remain
;; identical so this is a lossless refactor.

(let ((--stub-defuns--
       '(abbreviate-file-name abbrev-mode abort-recursive-edit abs add-text-properties advice-add advice-member-p advice-remove
    all-completions append apply aref arrayp aset ash assoc
    assq atom auto-fill-mode autoload autoload-do-load auto-save-mode backtrace backtrace-frame--internal
    backward-delete-char backward-sexp backward-word beep beginning-of-line bobp bolp bool-vector
    bool-vector-p boundp bounds-of-thing-at-point buffer-file-name buffer-list buffer-live-p buffer-local-value buffer-modified-p
    buffer-name bufferp buffer-size buffer-string buffer-substring buffer-substring-no-properties byte-code byte-compile byte-compile-disable-warning byte-compile-enable-warning byte-compile-warning-enabled-p byte-compile-warn-obsolete byte-run--set-speed cancel-timer capitalize-word
    car car-less-than-car car-safe cdr cdr-safe char-after
    char-before char-syntax char-table-p char-table-range char-to-string chmod cl--assertion-failed cl--class-allparents
    cl--class-docstring cl--class-index-table cl--class-name cl--class-parents cl--class-slots cl-generic-combine-methods cl--generic-dispatches cl--generic-generalizer-name
    cl--generic-generalizer-p cl--generic-generalizer-priority cl-generic-generalizers cl--generic-generalizer-specializers-function cl--generic-generalizer-tagcode-function cl--generic-make cl-generic-make-generalizer cl--generic-make-method
    cl--generic-method-call-con cl--generic-method-function cl--generic-method-qualifiers cl--generic-method-specializers cl--generic-method-table cl--generic-name cl--generic-options cl-method-qualifiers
    cl-no-applicable-method cl-no-next-method cl-no-primary-method cl-old-struct-compat-mode cl-prin1-to-string cl--slot-descriptor-initform cl--slot-descriptor-name cl--slot-descriptor-props
    cl--slot-descriptor-type cl--struct-class-named cl--struct-class-p cl--struct-class-print cl--struct-class-slots cl--struct-class-type cl-struct-define cl--struct-get-class
    cl--struct-name-p cl-type-of c-mode combine-after-change-execute commandp command-remapping compare-strings compare-window-configurations
    comp-el-to-eln-rel-filename compile completing-read compose-mail concat cons consp copy-keymap
    copy-marker copy-sequence ctl-x-4-prefix ctl-x-5-prefix current-buffer current-case-table current-column current-global-map
    current-input-mode current-local-map current-message current-window-configuration cursor-intangible-mode cursor-sensor-mode debug defalias
    default-boundp default-file-modes default-value define-button-type define-key defining-kbd-macro defvaralias delete
    delete-backward-char delete-char delete-minibuffer-contents delete-overlay delete-region delq describe-bindings describe-function
    describe-key describe-symbol describe-variable ding directory-file-name discard-input display-buffer display-graphic-p
    display-popup-menus-p display-warning documentation documentation-property downcase downcase-region downcase-word drop
    elt emacs-pid emacs-version end-of-line enlarge-window eobp eq eql
    equal erase-buffer error-message-string eval eval-after-load eval-buffer event-convert-list exec-path
    execute-extended-command-for-buffer exit-minibuffer exp expand-file-name face-background-pixmap face-font face-stipple face-underline-p
    fboundp featurep fetch-bytecode field-beginning field-end file-exists-p file-modes file-name-extension file-name-nondirectory file-name-sans-extension file-newer-than-file-p fillarray find-function-search-for-symbol
    find-lisp-object-file-name flatten-list floatp float-time fmakunbound format
    format-message format-spec forward-char forward-line forward-sexp forward-word frame-char-height
    frame-char-width frame-height frame-live-p framep frame-parameter frame-parameters frame-selected-window frame-toggle-on-screen-keyboard
    frame-visible-p frame-width fset funcall funcall-with-delayed-message function-documentation functionp function-put
    garbage-collect generate-new-buffer-name get get-advertised-calling-convention get-buffer get-buffer-create get-buffer-process get-char-property
    getenv gethash get-load-suffixes get-register get-text-property gnus goto-char grep
    hack-local-variables handler-bind-1 hash-table-p help help-add-fundoc-usage help-buffer help--docstring-quote help-form-show
    help-function-arglist help-insert-xref-button help-mode help-setup-xref help-split-fundoc hs-minor-mode iconify-frame
    indent-to indent-to-column indirect-function info input-pending-p insert insert-buffer-substring integerp
    intern internal-event-symbol-parse-modifiers internal--labeled-narrow-to-region internal--labeled-widen internal--track-mouse intern-soft invocation-directory
    invocation-name isearch-mode key-binding keyboard-coding-system key-description keymap-global-lookup keymap-global-set keymap-global-unset
    keymap-local-lookup keymap-local-set keymap-local-unset keymapp keymap-parent keymap-prompt keymap-set-after keymap-substitute
    key-parse key-translate keywordp kill-buffer kill-emacs kill-local-variable kmacro-end-macro length
    libxml-parse-html-region libxml-parse-xml-region line-beginning-position line-end-position lisp-indent-line list listp load
    loadhist-unload-element load-library load-with-code-conversion local-variable-if-set-p local-variable-p locate-file locate-file-internal locate-user-emacs-file
    log logand logb logior lognot logxor looking-at lookup-key
    macroexpand macroexpand-all macroexp-compiling-p macroexp-const-p macroexp-copyable-p macroexp--fgrep macroexp--funcall-if-compiled macroexp-progn
    macroexp--warn-and-return macroexp-warn-and-return mail make-char-table make-directory make-directory-autoloads make-frame-invisible make-frame-visible
    make-hash-table make-keymap make-list make-local-variable make-obsolete make-obsolete-variable make-overlay make-sparse-keymap
    make-string make-symbol make-text-button make-variable-buffer-local make-vector mapatoms mapbacktrace mapc
    mapcar mapconcat maphash map-keymap mark markerp marker-position
    mark-marker match-beginning match-data match-data--translate match-end max member memq
    message min minibuffer-message minibufferp minibuffer-prompt minibuffer-prompt-end minibuffer-recenter-top-bottom
    minibuffer-scroll-down-command minibuffer-scroll-other-window minibuffer-scroll-other-window-down minibuffer-scroll-up-command minibuffer-window mkdir mod modify-frame-parameters
    mouse-position move-marker move-overlay move-to-column mutex-lock mutex-unlock narrow-to-region native-comp-available-p
    native-comp-function-p native-comp-unit-file natnump nconc newline next-frame next-property-change next-single-property-change
    next-window nlistp normal-mode nreverse nth nthcdr null number-at-point
    numberp number-to-string object-intervals occur oclosure-type other-frame overlay-buffer overlay-end
    overlay-get overlay-lists overlay-properties overlay-put overlay-recenter overlays-in overlay-start overwrite-mode
    pcase--make-docstring play-sound-internal plist-get plist-member plist-put point point-at-bol point-at-eol
    point-marker point-max point-min pos-bol pos-eol posn-at-point prefix-numeric-value prin1
    prin1-to-string princ print process-attributes process-file process-filter process-plist process-query-on-exit-flag
    process-send-region process-sentinel progress-reporter-make propertize purecopy put puthash
    put-text-property raise-frame random rassq read read-from-minibuffer read-from-string read-kbd-macro
    read-library-name read-string recenter recenter-top-bottom record recordp redirect-frame-focus regexp-opt
    regexp-quote remember remove-list-of-text-properties rename-buffer repeat replace-match re-search-backward re-search-forward
    restore-buffer-modified-p reverse rplaca rplacd run-hooks run-hook-with-args run-hook-with-args-until-success
    run-hook-wrapped run-window-configuration-change-hook run-with-idle-timer safe-length save-current-buffer save-excursion save-restriction scroll-bar-scale
    scroll-down scroll-down-command scroll-left scroll-other-window scroll-other-window-down scroll-right scroll-up scroll-up-command
    search-backward-regexp search-forward search-forward-regexp secure-hash selected-frame selected-window select-frame select-window
    self-insert-command send-region send-string seq-concatenate seq-find seq-some seq-subseq seq-uniq
    set-advertised-calling-convention set-buffer set-buffer-modified-p setcar set-case-table setcdr set-char-table-parent set-char-table-range
    set-default set-default-file-modes setenv set-face-font set-face-stipple set-face-underline set-file-modes set-frame-height
    set-frame-parameter set-frame-selected-window set-frame-width set-input-mode set-keymap-parent set-mark set-marker set-match-data
    set-mouse-position setplist set-process-buffer set-process-filter set-process-plist set-process-sentinel set-register set-standard-case-table
    set-syntax-table set-temporary-overlay-map set-terminal-parameter set-text-conversion-style set-text-properties set-visited-file-modtime set-visited-file-name set-window-buffer
    set-window-configuration set-window-dedicated-p set-window-display-table set-window-hscroll set-window-parameter set-window-point set-window-start shell
    signal single-key-description skip-chars-backward skip-chars-forward skip-syntax-backward skip-syntax-forward sleep-for sort
    special-variable-p standard-case-table standard-syntax-table start-file-process store-match-data string string-as-multibyte string-as-unibyte
    string-equal string-lessp string-make-multibyte string-make-unibyte string-match stringp string-search string-split
    string-to-multibyte string-to-number string-to-unibyte string-width subr-arity subr-native-comp-unit substitute-quotes substring substring-no-properties suspend-emacs switch-to-buffer sxhash sxhash-equal symbol-function
    symbol-name symbolp symbol-plist symbol-value syntax-ppss-flush-cache syntax-propertize syntax-table take
    temp-buffer-resize-mode temporary-file-directory terminal-parameter terpri text-properties-at time-convert time-less-p truncate
    tty-top-frame type-of undo-amalgamate-change-group undo-auto-amalgamate undo-more unhandled-file-name-directory unintern upcase
    upcase-region upcase-word update-directory-autoloads use-global-map use-local-map user-login-name user-original-login-name variable-at-point
    vconcat vector vectorp view-mode visited-file-modtime walk-windows warn wholenump
    widen window-buffer window-combination-limit window-configuration-equal-p window-dedicated-p window-display-table window-end window-font-height
    window-font-width window-frame window-height window-hscroll window-live-p windowp window-parameter window-point
    window-start window-width with-no-warnings write-region x-popup-dialog xterm-mouse-mode yank yes-or-no-p
    y-or-n-p-with-timeout zlib-available-p)))
  (dolist (--s-- --stub-defuns--)
    (unless (fboundp --s--)
      (fset --s-- (lambda (&rest _) nil))
      (put --s-- 'emacs-stub-bulk t))))

(let ((--stub-defmacros--
       '(add-function bound-and-true-p cl--define-built-in-type define-inline define-minor-mode define-obsolete-function-alias define-obsolete-variable-alias defsubst
    eval-and-compile eval-when-compile gv-letplace macroexp-let2 minibuffer-with-setup-hook oclosure-define oclosure-lambda pcase
    pcase-defmacro pcase-dolist pcase-exhaustive pcase-let setf with-connection-local-variables with-help-window with-suppressed-warnings
    with-temp-buffer-window)))
  (dolist (--s-- --stub-defmacros--)
    (unless (fboundp --s--)
      ;; Install through a constructed `defmacro' form, NOT a raw
      ;; `(fset SYM (cons 'macro (lambda ...)))'.  The NeLisp standalone
      ;; evaluator registers macros through the `defmacro' path; a macro
      ;; whose function cell is a hand-built `(macro . CLOSURE)' cons is
      ;; `fboundp' and `macrop' but *invoking* it aborts the enclosing
      ;; top-level form flagless (bare rc=1, no error stash -- verified
      ;; in isolation: `(fset 'x (cons 'macro (lambda (&rest _) nil)))'
      ;; then `(x)' aborts, while the equivalent `defmacro' works).  Any
      ;; vendor call site reaching one of these stubs (e.g. Magit/
      ;; transient's `pcase-exhaustive' uses) previously killed its whole
      ;; form silently instead of expanding to nil as intended.  Host
      ;; Emacs accepts both shapes, so the constructed `defmacro' is
      ;; strictly more portable.
      (eval (list 'defmacro --s-- '(&rest _) nil) t)
      (put --s-- 'emacs-stub-bulk t))))

(let ((--stub-defvars--
       '(after-change-functions after-load-alist before-change-functions buffer-invisibility-spec buffer-list-update-hook buffer-read-only case-fold-search cl--generic-derived-generalizer
    cl--generic-eql-generalizer cl--generic-head-generalizer cl--generic-oclosure-generalizer cl--generic-t-generalizer cl--generic-typeof-generalizer command-debug-status command-error-function comp-enable-subr-trampolines
    current-load-list cursor-in-echo-area data-directory debugger debug-ignored-errors debug-on-error default-directory defun-declarations-alist
    delayed-after-hook-functions delayed-mode-hooks delayed-warnings-list echo-keystrokes emacs-basic-display emacs-startup-hook enable-recursive-minibuffers executing-kbd-macro features
    find-tag-default-function find-word-boundary-function-table font-lock-defaults function-key-map help-char help-fns-describe-function-functions help-form history-delete-duplicates
    history-length horizontal-scroll-bar inhibit-changing-match-data inhibit-field-text-motion inhibit-modification-hooks inhibit-nul-byte-detection inhibit-null-byte-detection inhibit-point-motion-hooks
    inhibit-read-only input-decode-map input-method-function jka-compr-load-suffixes keyboard-translate-table kill-buffer-hook kill-buffer-query-functions lexical-binding
    line-spacing load-dangerous-libraries load-file-name load-file-rep-suffixes load-history load-path load-suffixes macro-declarations-alist
    macroexpand-all-environment macroexp--dynvars magic-fallback-mode-alist mail-user-agent major-mode--suspended max-lisp-eval-depth menu-prompting
    messages-buffer-max-lines minibuffer-auto-raise minibuffer-default-prompt-format minibuffer-local-map minibuffer-scroll-window minor-mode-alist minor-mode-map-alist mode-line-mode-menu
    most-negative-fixnum most-positive-fixnum native-comp-deferred-compilation native-comp-enable-subr-trampolines native-comp-jit-compilation needed noninteractive obarray
    operating-system-release output overriding-local-map overriding-terminal-local-map parse-sexp-lookup-properties pending-undo-list post-self-insert-hook print-escape-newlines
    print-gensym print-quoted print-unreadable-function purify-flag query-replace-map redisplay-dont-pause s set-variable-value-history
    shell-file-name standard-output sym symbols-with-pos-enabled syntax-propertize-function system-type temp-buffer-show-function text-conversion-style
    translation-table-for-input undo-in-progress undo-limit undo-outer-limit undo-strong-limit use-dialog-box values vertical-scroll-bar
    x-gtk-use-window-move yank-transform-functions)))
  (dolist (--s-- --stub-defvars--)
    (unless (boundp --s--)
      ;; Constructed `defvar', not `set': every name in this list is a
      ;; real dynamic (special) variable in Emacs, and vendor code
      ;; let-binds many of them expecting other functions to observe the
      ;; binding dynamically.  A bare `set' only populates the global
      ;; value cell without marking the symbol special, so under
      ;; lexical-binding a later `(let ((inhibit-read-only t)) ...)'
      ;; creates an invisible LEXICAL binding and the read-only check in
      ;; `emacs-buffer--barf-if-read-only' still sees the global nil --
      ;; exactly the `default-process-coding-system' defect class the
      ;; magit bridge documented, recurring here for every name below.
      (eval (list 'defvar --s-- nil) t))))

(unless (fboundp 'define-abbrev-table)
  (defun define-abbrev-table (symbol definitions &optional _docstring &rest _props)
    "Standalone load-time stub for Emacs abbrev tables."
    (set symbol definitions)
    symbol))

(unless (fboundp 'make-syntax-table)
  (defun make-syntax-table (&optional _table)
    "Standalone load-time stub for syntax tables."
    (make-vector 256 nil)))

(unless (fboundp 'syntax-table-p)
  (defun syntax-table-p (object)
    "Standalone load-time stub for syntax table predicate.
Recognizes every syntax-table representation the substrate can
produce: the `emacs-syntax-table.el' char-table (subtype
`syntax-table') when `emacs-char-table-p' is available, the
`emacs-stub.el' cons fallback `(syntax-table . PARENT)', and the
plain 256-slot vectors produced by the `make-syntax-table' stub
above."
    (or (and (fboundp 'emacs-char-table-p)
             (emacs-char-table-p object)
             (eq (emacs-char-table-subtype object) 'syntax-table))
        (eq (car-safe object) 'syntax-table)
        (and (vectorp object) (= (length object) 256)))))

(unless (fboundp 'modify-syntax-entry)
  (defun modify-syntax-entry (_char _syntax &optional _table)
    "Standalone load-time stub for syntax table mutation."
    nil))

(unless (fboundp 'let-when-compile)
  (defmacro let-when-compile (bindings &rest body)
    "Standalone load-time stub for compile-time lexical binding."
    (cons 'let (cons bindings body))))

(unless (fboundp 'defvar-keymap)
  (defmacro defvar-keymap (name &rest _args)
    "Standalone load-time stub for keymap variables."
    (list 'defvar name (list 'make-sparse-keymap))))

(provide 'emacs-stub-bulk)
;;; emacs-stub-bulk.el ends here
