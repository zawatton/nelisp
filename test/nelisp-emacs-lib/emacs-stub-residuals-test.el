;;; emacs-stub-residuals-test.el --- ERT for Phase 11.C'' residual stubs  -*- lexical-binding: t; -*-

;;; Commentary:

;; Phase F (2026-05-03) — Doc 51 / nelisp-emacs.
;;
;; Phase 11.C'' deliberately kept sentinel compatibility stubs in
;; `emacs-stub.el' for names whose corresponding prefixed substrate did
;; not exist yet.  The former display probes now use a small capability
;; map keyed by `emacs-display-system'.
;;
;; `define-key-after' has since moved out of residual-stub status via
;; `emacs-keymap-define-key-after' and `emacs-keymap-builtins.el'; the
;; `emacs-stub.el' fallback remains only for minimal load-order
;; compatibility.
;; `window-live-p' and `frame-selected-window' likewise moved to
;; `emacs-window-builtins.el' once the prefixed window model grew real
;; live/deleted predicates and selected-window access.
;;
;; These tests pin the documented sentinel return values so any
;; future replacement (= bridge to a real prefixed impl) cannot
;; silently regress the API surface that callers depend on.
;;
;; Under host Emacs the host's C builtins win, so the kept stubs in
;; `emacs-stub.el' never fire.  We assert two things:
;;
;;   (a) `featurep' / `fboundp' parity (= the stubs load without error
;;       and the unprefixed names are bound, regardless of whether the
;;       binding is host or stub).
;;   (b) Polyfill-body shape parity using literal copies of the stub
;;       bodies — these run regardless of host-Emacs presence and pin
;;       what standalone NeLisp will see when the stub fires.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-stub)

(defconst emacs-stub-residuals-test--builtin-bridge-libraries
  '("emacs-buffer-builtins"
    "emacs-fileio-builtins"
    "emacs-edit-builtins"
    "emacs-keymap-builtins"
    "emacs-frame-builtins"
    "emacs-window-builtins"
    "emacs-line-builtins"
    "emacs-minibuffer-builtins"
    "emacs-search-builtins"
    "emacs-command-loop-builtins"
    "emacs-process-builtins"
    "emacs-undo-builtins"
    "emacs-mode-builtins"
    "emacs-faces-builtins"
    "emacs-font-lock-builtins"
    "emacs-redisplay-builtins")
  "Builtin bridge libraries that must install over standalone stubs.")

(defun emacs-stub-residuals-test--source-file (library)
  "Return source .el path for LIBRARY."
  (let ((file (locate-library library)))
    (when (and file (string-match-p "\\.elc\\'" file))
      (setq file (concat (substring file 0 (- (length file) 1)))))
    file))

(defun emacs-stub-residuals-test--with-reloaded-custom-fallbacks (thunk)
  "Run THUNK after reloading `emacs-stub' with Custom host subrs unbound.
Restore the original host definitions afterwards."
  (let* ((source (emacs-stub-residuals-test--source-file "emacs-stub"))
         (symbols '(custom-add-to-group
                    custom-add-version
                    custom-add-package-version
                    custom-add-link
                    custom-add-dependencies
                    custom-handle-keyword
                    custom-handle-all-keywords
                    custom-current-group))
         (saved (mapcar (lambda (sym)
                          (cons sym (and (fboundp sym)
                                         (symbol-function sym))))
                        symbols)))
    (unwind-protect
        (progn
          (dolist (sym symbols)
            (fmakunbound sym))
          (load-file source)
          (funcall thunk))
      (dolist (entry saved)
        (let ((sym (car entry))
              (fn (cdr entry)))
          (if fn
              (fset sym fn)
            (fmakunbound sym)))))))

(defun emacs-stub-residuals-test--with-reloaded-custom-face-metadata (thunk)
  "Run THUNK after reloading `emacs-stub' with face metadata unbound.
Restore the original host bindings afterwards."
  (let* ((source (emacs-stub-residuals-test--source-file "emacs-stub"))
         (symbol 'custom-face-attributes)
         (was-bound (boundp symbol))
         (saved-value (and was-bound (symbol-value symbol)))
         (saved-get (and (fboundp 'custom-face-attributes-get)
                         (symbol-function 'custom-face-attributes-get))))
    (unwind-protect
        (progn
          (makunbound symbol)
          (fmakunbound 'custom-face-attributes-get)
          (load-file source)
          (funcall thunk))
      (if was-bound
          (set symbol saved-value)
        (makunbound symbol))
      (if saved-get
          (fset 'custom-face-attributes-get saved-get)
        (fmakunbound 'custom-face-attributes-get)))))

(defun emacs-stub-residuals-test--read-first-form-matching (file predicate)
  "Return the first top-level form in FILE matching PREDICATE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (catch 'found
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (funcall predicate form)
                (throw 'found form))))
        (end-of-file nil)))))

(defun emacs-stub-residuals-test--error-message (thunk)
  "Return the error message from THUNK, or nil when THUNK succeeds."
  (condition-case err
      (progn (funcall thunk) nil)
    (error (cadr err))))

;;;; A. Load cleanly + fboundp parity

(ert-deftest emacs-stub-residuals-test/feature-and-fboundp ()
  (should (featurep 'emacs-stub))
  (dolist (sym '(function-get define-key-after
                  display-graphic-p display-color-p display-multi-frame-p
                  window-system
                  emacs-display-window-system emacs-display-graphic-p
                  emacs-display-color-p emacs-display-multi-frame-p
                  window-live-p frame-selected-window
                  custom-add-option custom-add-frequent-value
                  custom-variable-p defgroup defcustom
                  custom-current-group
                  file-size-human-readable
                  set-advertised-calling-convention
                  get-advertised-calling-convention
                  convert-standard-filename string-to-list
                  regexp-quote regexp-opt easy-menu-define
                  easy-menu-add-item
                  current-idle-time shell-command-to-string
                  call-process-shell-command
                  bound-and-true-p
                  emacs-stub--add-hook emacs-stub--remove-hook
                  emacs-stub--run-hook
                  emacs-stub--advice-add emacs-stub--advice-remove
                  emacs-stub--advice-member-p
                  emacs-stub--add-function-value
                  emacs-stub--remove-function-value
                  emacs-stub--run-with-timer
                  emacs-stub--run-with-idle-timer
                  emacs-stub--cancel-timer
                  line-number-display-width
                  syntax-propertize-rules cc-require cc-provide
                  version-to-list version-list-< version-list-<=
                  version< version<= combine-change-calls define-advice
                  c-add-style
                  android-read-build-system android-read-build-time
                  emacs-version version
                  emacs-repository-version-git
                  emacs-repository-version-android
                  emacs-repository-get-version
                  emacs-repository-branch-android
                  emacs-repository-branch-git
                  emacs-repository-get-branch
                  emacs-bzr-get-version
                  gui-set-selection x-set-selection
                  gui-get-selection x-get-selection
                  make-help-screen help--help-screen))
    (should (fboundp sym)))
  (should (featurep 'help-macro))
  (should (boundp 'emacs-display-system))
  (should (boundp 'emacs-basic-display))
  (should (boundp 'initial-window-system))
  (should (boundp 'custom-current-group-alist))
  (should (boundp 'user-mail-address))
  (should (boundp 'user-full-name))
  (should (boundp 'display-line-numbers))
  (should (boundp 'display-line-numbers-width))
  (should (boundp 'display-line-numbers-widen))
  (should (boundp 'display-line-numbers-current-absolute))
  (should (boundp 'outline-mode-syntax-table))
  (should (boundp 'text-mode-syntax-table))
  (should (integerp emacs-major-version))
  (should (integerp emacs-minor-version))
  (should (boundp 'three-step-help))
  (should (boundp 'help-for-help-use-variable-pitch)))

(ert-deftest emacs-stub-residuals-test/custom-current-group-alist-and-fn-availability ()
  (should (boundp 'custom-current-group-alist))
  (should (fboundp 'custom-current-group)))

(ert-deftest emacs-stub-residuals-test/advertised-calling-convention-metadata ()
  (let ((sym (make-symbol "emacs-stub-test--advertised")))
    (should (equal '(arg)
                   (set-advertised-calling-convention sym '(arg) "30.1")))
    (should (get-advertised-calling-convention sym))))

(ert-deftest emacs-stub-residuals-test/function-get-reads-symbol-property ()
  "Doc 15 B4 breadth: `function-get' returns a function symbol's property.
It was void on the reader, blocking `define-inline' / cl-generic users."
  (let ((sym (make-symbol "emacs-stub-test--fg")))
    (should (null (function-get sym 'no-such-prop)))
    (put sym 'my-prop 123)
    (should (equal 123 (function-get sym 'my-prop)))))

(ert-deftest emacs-stub-residuals-test/hook-helpers-store-run-and-remove ()
  (let ((hook (make-symbol "emacs-stub-test--hook"))
        (seen nil))
    (fset 'emacs-stub-test--hook-a (lambda () (push 'a seen)))
    (fset 'emacs-stub-test--hook-b (lambda () (push 'b seen)))
    (unwind-protect
        (progn
          (emacs-stub--add-hook hook 'emacs-stub-test--hook-a)
          (emacs-stub--add-hook hook 'emacs-stub-test--hook-b t)
          (should (equal '(emacs-stub-test--hook-a emacs-stub-test--hook-b)
                         (symbol-value hook)))
          ;; Re-adding moves the function rather than duplicating it.
          (emacs-stub--add-hook hook 'emacs-stub-test--hook-a t)
          (should (equal '(emacs-stub-test--hook-b emacs-stub-test--hook-a)
                         (symbol-value hook)))
          (emacs-stub--run-hook hook nil)
          (should (equal '(a b) seen))
          (emacs-stub--remove-hook hook 'emacs-stub-test--hook-b)
          (should (equal '(emacs-stub-test--hook-a) (symbol-value hook))))
      (fmakunbound 'emacs-stub-test--hook-a)
      (fmakunbound 'emacs-stub-test--hook-b))))

(ert-deftest emacs-stub-residuals-test/hook-helpers-support-args-and-depth ()
  (let ((hook (make-symbol "emacs-stub-test--abnormal-hook"))
        (seen nil))
    (fset 'emacs-stub-test--hook-low (lambda (x) (push (list 'low x) seen)))
    (fset 'emacs-stub-test--hook-high (lambda (x) (push (list 'high x) seen)))
    (unwind-protect
        (progn
          (emacs-stub--add-hook hook 'emacs-stub-test--hook-high 90)
          (emacs-stub--add-hook hook 'emacs-stub-test--hook-low -10)
          (should (equal '((-10 . emacs-stub-test--hook-low)
                           (90 . emacs-stub-test--hook-high))
                         (symbol-value hook)))
          (emacs-stub--run-hook hook '(value))
          (should (equal '((high value) (low value)) seen)))
      (fmakunbound 'emacs-stub-test--hook-low)
      (fmakunbound 'emacs-stub-test--hook-high))))

(ert-deftest emacs-stub-residuals-test/advice-helpers-support-override-and-remove ()
  (let ((target (make-symbol "emacs-stub-test--advised-target"))
        (advice (make-symbol "emacs-stub-test--override-advice")))
    (fset target (lambda (x) (list 'orig x)))
    (fset advice (lambda (x) (list 'override x)))
    (should (eq advice
                (emacs-stub--advice-add target :override advice)))
    (should (emacs-stub--advice-member-p advice target))
    (should (equal '(override value) (funcall target 'value)))
    (should-not (emacs-stub--advice-remove target advice))
    (should-not (emacs-stub--advice-member-p advice target))
    (should (equal '(orig value) (funcall target 'value)))))

(ert-deftest emacs-stub-residuals-test/advice-helpers-support-around-before-after ()
  (let ((target (make-symbol "emacs-stub-test--advised-target"))
        (before (make-symbol "emacs-stub-test--before-advice"))
        (after (make-symbol "emacs-stub-test--after-advice"))
        (around (make-symbol "emacs-stub-test--around-advice"))
        (seen nil))
    (fset target (lambda (x) (list 'orig x)))
    (fset before (lambda (x) (push (list 'before x) seen)))
    (fset after (lambda (x) (push (list 'after x) seen)))
    (fset around
          (lambda (fn x) (cons 'around (funcall fn x))))
    (emacs-stub--advice-add target :before before)
    (emacs-stub--advice-add target :around around)
    (emacs-stub--advice-add target :after after)
    (should (equal '(around orig value) (funcall target 'value)))
    (should (equal '((after value) (before value)) seen))))

(ert-deftest emacs-stub-residuals-test/advice-helpers-support-filters ()
  (let ((target (make-symbol "emacs-stub-test--filter-target"))
        (filter-args (make-symbol "emacs-stub-test--filter-args"))
        (filter-return (make-symbol "emacs-stub-test--filter-return")))
    (fset target (lambda (x) (concat x "-body")))
    (fset filter-args (lambda (args) (list (concat (car args) "-args"))))
    (fset filter-return (lambda (value) (concat value "-return")))
    (emacs-stub--advice-add target :filter-args filter-args)
    (emacs-stub--advice-add target :filter-return filter-return)
    (should (equal "x-args-body-return" (funcall target "x")))))

(ert-deftest emacs-stub-residuals-test/add-function-value-filter-return ()
  "The standalone add-function substrate composes :filter-return advice."
  (let* ((base (lambda (s) (concat s "-base")))
         (filter (lambda (s) (concat s "-filtered")))
         (wrapped (emacs-stub--add-function-value :filter-return base filter)))
    (should (eq (get wrapped 'emacs-stub--advice-original) base))
    (should (equal "x-base-filtered" (funcall wrapped "x")))
    (setq wrapped (emacs-stub--remove-function-value wrapped filter))
    (should (eq wrapped base))
    (should (equal "x-base" (funcall wrapped "x")))))

(ert-deftest emacs-stub-residuals-test/add-function-symbol-filter-return ()
  "The standalone add-function substrate can update a function variable."
  (let ((sym (make-symbol "emacs-stub-test--filter-var"))
        (filter (lambda (s) (concat s ":filtered"))))
    (set sym (lambda (beg end &optional delete)
               (format "%s:%s:%s" beg end delete)))
    (emacs-stub--add-function-symbol :filter-return sym filter nil nil)
    (should (equal "1:2:nil:filtered" (funcall (symbol-value sym) 1 2 nil)))
    (emacs-stub--remove-function-symbol sym filter nil)
    (should (equal "1:2:nil" (funcall (symbol-value sym) 1 2 nil)))))

(ert-deftest emacs-stub-residuals-test/timer-helpers-create-and-cancel ()
  (let ((timer-list nil)
        (timer-idle-list nil))
    (let ((timer (emacs-stub--run-with-timer 10 nil #'ignore 'arg)))
      (should (emacs-stub--timer-p timer))
      (should (memq timer timer-list))
      (should (equal #'ignore (aref timer 3)))
      (should (equal '(arg) (aref timer 4)))
      (should-not (emacs-stub--cancel-timer timer))
      (should-not (memq timer timer-list)))
    (let ((timer (emacs-stub--run-with-idle-timer 3 t #'ignore)))
      (should (emacs-stub--timer-p timer))
      (should (memq timer timer-idle-list))
      (should (= 3 (aref timer 5)))
      (should (aref timer 2)))))

(ert-deftest emacs-stub-residuals-test/timer-fallbacks-have-real-bodies ()
  (let ((file (emacs-stub-residuals-test--source-file "emacs-stub")))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((source (buffer-string)))
        (should (string-match-p "defun emacs-stub--run-with-timer" source))
        (should (string-match-p "defun run-with-timer" source))
        (should (string-match-p "defun run-at-time" source))
        (should (string-match-p "defun timerp" source))
        (should (string-match-p "defun cancel-function-timers" source))))))

(ert-deftest emacs-stub-residuals-test/define-inline-lowers-inline-dsl ()
  "Doc 15 B4: runtime define-inline lowers the inline DSL against the
backquote (comma X) representation to a plain defun (function version).
The helper is unconditional; the macro itself is reader-gated."
  ;; inline-quote: (comma X) -> X
  (should (equal '(defun f (x) (+ x 1))
                 (emacs-stub--define-inline
                  'f '(x) '((inline-quote (+ (comma x) 1))))))
  ;; inline-letevals wrapping inline-quote (ht-get* shape)
  (should (equal '(defun g (table key) (gethash key table))
                 (emacs-stub--define-inline
                  'g '(table key)
                  '((inline-letevals (table key)
                      (inline-quote (gethash (comma key) (comma table))))))))
  ;; leading docstring is stripped
  (should (equal '(defun h (x) x)
                 (emacs-stub--define-inline
                  'h '(x) '("doc" (inline-quote (comma x)))))))

(ert-deftest emacs-stub-residuals-test/define-inline-lowers-artifact-comma-symbol-heads ()
  "Doc 15 B4: runtime define-inline lowers the inline DSL against the
artifact-reader comma symbols to a plain defun.
The helper is unconditional; the macro itself is reader-gated."
  (let ((comma (intern ",")))
    (should (equal '(defun org-element-type-p (node types)
                      (if (listp types)
                          (memq (org-element-type node t) types)
                        (eq (org-element-type node t) types)))
                   (emacs-stub--define-inline
                    'org-element-type-p '(node types)
                    (list (list 'inline-letevals '(node types)
                                (list 'if
                                      (list 'listp
                                            (list 'inline-const-val 'types))
                                      (list 'inline-quote
                                            (list 'memq
                                                  (list 'org-element-type
                                                        (list comma 'node)
                                                        t)
                                                  (list comma 'types)))
                                      (list 'inline-quote
                                            (list 'eq
                                                  (list 'org-element-type
                                                        (list comma 'node)
                                                        t)
                                                  (list comma 'types)))))))))
    (cl-labels ((comma-call-head-p (form)
                  (cond
                   ((not (consp form)) nil)
                   ((let ((head (car form)))
                      (and (symbolp head)
                           (member (symbol-name head) '("," ",@"))))
                    t)
                   (t (or (comma-call-head-p (car form))
                          (comma-call-head-p (cdr form)))))))
      (should-not (comma-call-head-p
                   (emacs-stub--define-inline
                    'org-element-type-p '(node types)
                    (list (list 'inline-letevals '(node types)
                                (list 'if
                                      (list 'listp
                                            (list 'inline-const-val 'types))
                                      (list 'inline-quote
                                            (list 'memq
                                                  (list 'org-element-type
                                                        (list comma 'node)
                                                        t)
                                                  (list comma 'types)))
                                      (list 'inline-quote
                                            (list 'eq
                                                  (list 'org-element-type
                                                        (list comma 'node)
                                                        t)
                                                  (list comma 'types))))))))))))

(ert-deftest emacs-stub-residuals-test/define-inline-generated-function-runs ()
  (let* ((comma (intern ","))
         (comma-at (intern ",@"))
         (generated
          (emacs-stub--define-inline
           'emacs-stub-test--inline '(table key)
           (list (list 'inline-letevals '(table key)
                       (list 'inline-quote
                             (list 'gethash
                                   (list comma 'key)
                                   (list comma-at 'table))))))))
    (eval generated t)
    (unwind-protect
        (let ((table (let ((ht (make-hash-table :test 'equal)))
                       (puthash "k" "v" ht)
                       ht)))
          (should (equal "v" (emacs-stub-test--inline table "k"))))
      (fmakunbound 'emacs-stub-test--inline))))

(ert-deftest emacs-stub-residuals-test/display-line-number-core-defaults ()
  (should (integerp (line-number-display-width)))
  (should-not display-line-numbers)
  (should-not display-line-numbers-width)
  (should-not display-line-numbers-widen)
  (should display-line-numbers-current-absolute)
  (let ((file (emacs-stub-residuals-test--source-file "emacs-stub")))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (search-forward "(defun line-number-display-width" nil t))
      (should (search-forward "    1))" nil t)))))

;;;; B. define-key-after has a real keymap substrate

(ert-deftest emacs-stub-residuals-test/define-key-after-bridged-by-keymap-builtins ()
  (require 'emacs-keymap-builtins)
  (let ((map (emacs-keymap-make-sparse-keymap))
        (seen '()))
    (should (fboundp 'emacs-keymap-define-key-after))
    (emacs-keymap-define-key map "a" 'cmd-a)
    (emacs-keymap-define-key map "b" 'cmd-b)
    (should (eq 'cmd-c
                (emacs-keymap-define-key-after map "c" 'cmd-c ?b)))
    (emacs-keymap-map-keymap (lambda (k _v) (push k seen)) map)
    (should (equal (nreverse seen) (list ?b ?c ?a)))))

;;;; C. display-* / window-system dispatch against emacs-display-system
;;
;; Phase 1.E (2026-05-05) — the display probes left stub-land in this
;; phase: they now consult `emacs-display-system' so display backends
;; (= nelisp-emacs-gtk) can flip the values that init.el branches on.
;; These tests pin the dispatch matrix against the documented values.

(ert-deftest emacs-stub-residuals-test/display-probes-default-nil ()
  ;; With no backend set, all probes return nil — the same behaviour
  ;; the old hard-coded stubs had, preserved as the default path.
  (let ((emacs-display-system nil))
    (should-not (emacs-display-window-system))
    (should-not (emacs-display-graphic-p))
    (should-not (emacs-display-color-p))
    (should-not (emacs-display-multi-frame-p))))

(ert-deftest emacs-stub-residuals-test/display-probes-graphic-backend ()
  ;; A graphic backend (= 'gtk / 'x / 'pgtk / 'w32 / 'ns) flips
  ;; window-system + display-graphic-p + display-color-p +
  ;; display-multi-frame-p all to truthy.
  (let ((emacs-display-system 'gtk))
    (should (eq 'gtk (emacs-display-window-system)))
    (should (emacs-display-graphic-p))
    (should (emacs-display-color-p))
    (should (emacs-display-multi-frame-p))))

(ert-deftest emacs-stub-residuals-test/display-probes-tui-backend ()
  ;; A TUI backend (= 'tui) sets window-system + display-multi-frame-p
  ;; non-nil but display-graphic-p stays nil — that's how callers
  ;; distinguish "have a display" from "have a graphical display".
  (let ((emacs-display-system 'tui))
    (should (eq 'tui (emacs-display-window-system)))
    (should-not (emacs-display-graphic-p))
    (should-not (emacs-display-color-p))
    (should (emacs-display-multi-frame-p))))

(ert-deftest emacs-stub-residuals-test/display-probe-install-overwrites-standalone-stubs ()
  ;; The display map lives in `emacs-stub.el' itself, after the old
  ;; no-op stubs.  Standalone NeLisp must overwrite those earlier
  ;; definitions; host Emacs must keep its C builtins.
  (should (fboundp 'emacs-stub--install-function-p))
  (should-not (emacs-stub--install-function-p 'display-graphic-p))
  (let* ((file (locate-library "emacs-stub"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(window-system display-graphic-p display-color-p
                                   display-multi-frame-p))
        (goto-char (point-min))
        (should (search-forward
                 (format "(when (emacs-stub--install-function-p '%s)" sym)
                 nil t))))))

(ert-deftest emacs-stub-residuals-test/builtin-bridges-have-standalone-install-gates ()
  "Every builtin bridge must have an install gate aware of standalone NeLisp.

This is a coarse regression guard for the old `(unless (fboundp ...))'
pattern: host Emacs should keep its builtins, but standalone NeLisp
must be able to overwrite early bootstrap stubs with real substrates."
  (dolist (library emacs-stub-residuals-test--builtin-bridge-libraries)
    (let* ((gate (format "%s--install-function-p" library))
           (file (emacs-stub-residuals-test--source-file library)))
      (should (and file (file-exists-p file)))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (should (search-forward (concat "(defun " gate) nil t))
        (goto-char (point-min))
        (should (search-forward "(boundp 'emacs-version)" nil t))))))

;;;; D. buffer-local variable stubs preserve setq-local's contract

(ert-deftest emacs-stub-residuals-test/buffer-local-stubs-return-symbols ()
  ;; Standalone `setq-local' expands through `make-local-variable'; the
  ;; no-op fallback must return the original symbol, not nil.
  (let ((file (emacs-stub-residuals-test--source-file "emacs-stub")))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (search-forward "(get 'make-variable-buffer-local 'emacs-stub-bulk)" nil t))
      (goto-char (point-min))
      (should (search-forward "(defun make-variable-buffer-local (variable)" nil t))
      (should (search-forward "    variable))" nil t)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (search-forward "(get 'make-local-variable 'emacs-stub-bulk)" nil t))
      (goto-char (point-min))
      (should (search-forward "(defun make-local-variable (variable)" nil t))
      (should (search-forward "    variable))" nil t)))))

;;;; E. window-live-p has a real window substrate

(ert-deftest emacs-stub-residuals-test/window-live-p-bridged-by-window-builtins ()
  (require 'emacs-window-builtins)
  (emacs-window-reset)
  (unwind-protect
      (let* ((w1 (emacs-window-selected-window))
             (w2 (emacs-window-split-window-vertically)))
        (should (fboundp 'emacs-window-window-live-p))
        (should (emacs-window-window-live-p w1))
        (should (emacs-window-window-live-p w2))
        (emacs-window-delete-window w2)
        (should (emacs-window-window-live-p w1))
        (should-not (emacs-window-window-live-p w2)))
    (emacs-window-reset)))

;;;; F. frame-selected-window has a real window substrate

(ert-deftest emacs-stub-residuals-test/frame-selected-window-bridged-by-window-builtins ()
  (require 'emacs-window-builtins)
  (emacs-window-reset)
  (unwind-protect
      (let ((w1 (emacs-window-selected-window))
            (w2 (emacs-window-split-window-vertically)))
        (should (fboundp 'emacs-window-frame-selected-window))
        (should (eq w1 (emacs-window-frame-selected-window)))
        (emacs-window-select-window w2)
        (should (eq w2 (emacs-window-frame-selected-window 'ignored-frame))))
    (emacs-window-reset)))

;;;; G. Custom metadata helpers

(ert-deftest emacs-stub-residuals-test/custom-add-option-deduplicates ()
  (let ((sym (make-symbol "nelisp-emacs-custom-option")))
    (custom-add-option sym 'turn-on-auto-fill)
    (custom-add-option sym 'turn-on-auto-fill)
    (custom-add-option sym 'flyspell-mode)
    (should (equal (get sym 'custom-options)
                   '(flyspell-mode turn-on-auto-fill)))))

(ert-deftest emacs-stub-residuals-test/custom-loads-preserve-existing-metadata ()
  (let ((sym (make-symbol "nelisp-emacs-custom-loads")))
    (custom-add-load sym 'initial-lib)
    (custom-add-load sym 'initial-lib)
    (custom--add-custom-loads sym '(new-lib initial-lib))
    (should (equal (get sym 'custom-loads)
                   '(new-lib initial-lib)))))

(ert-deftest emacs-stub-residuals-test/custom-autoload-records-load-and-marker ()
  (let ((sym (make-symbol "nelisp-emacs-custom-autoload")))
    (custom-autoload sym 'autoload-lib)
    (should (eq (get sym 'custom-autoload) t))
    (should (equal (get sym 'custom-loads) '(autoload-lib)))
    (custom-autoload sym 'noset-lib t)
    (should (eq (get sym 'custom-autoload) 'noset))
    (should (equal (get sym 'custom-loads)
                   '(noset-lib autoload-lib)))))

(ert-deftest emacs-stub-residuals-test/custom-variable-p-metadata ()
  (let ((standard (make-symbol "nelisp-emacs-custom-standard"))
        (autoloaded (make-symbol "nelisp-emacs-custom-autoloaded"))
        (plain (make-symbol "nelisp-emacs-custom-plain")))
    (put standard 'standard-value '(42))
    (put autoloaded 'custom-autoload t)
    (should (custom-variable-p standard))
    (should (custom-variable-p autoloaded))
    (should-not (custom-variable-p plain))
    (should-not (custom-variable-p "not-a-symbol"))))

(ert-deftest emacs-stub-residuals-test/custom-declare-variable-metadata-shape ()
  (let ((symbol (make-symbol "nelisp-emacs-custom-declare-variable")))
    (cl-letf (((symbol-function 'custom-declare-variable)
               (lambda (sym default doc &rest args)
                 (unless (boundp sym)
                   (set sym default))
                 (put sym 'standard-value (list default))
                 (put sym 'variable-documentation doc)
                 (put sym 'custom-args args)
                 sym)))
      (should (eq symbol
                  (custom-declare-variable symbol 42 "doc" :type 'integer)))
      (should (= 42 (symbol-value symbol)))
      (should (equal '(42) (get symbol 'standard-value)))
      (should (equal "doc" (get symbol 'variable-documentation)))
      (should (equal '(:type integer) (get symbol 'custom-args))))))

(ert-deftest emacs-stub-residuals-test/custom-declare-face-metadata-shape ()
  (let ((face (make-symbol "nelisp-emacs-custom-declare-face"))
        (spec '((t :inherit bold))))
    (cl-letf (((symbol-function 'custom-declare-face)
               (lambda (name value doc &rest args)
                 (put name 'face-defface-spec value)
                 (put name 'face-documentation doc)
                 (put name 'custom-args args)
                 name)))
      (should (eq face
                  (custom-declare-face face spec "face doc" :group 'faces)))
      (should (equal spec (get face 'face-defface-spec)))
      (should (equal "face doc" (get face 'face-documentation)))
      (should (equal '(:group faces) (get face 'custom-args))))))

(ert-deftest emacs-stub-residuals-test/custom-face-attributes-fallback-registers-supported-keys ()
  (emacs-stub-residuals-test--with-reloaded-custom-face-metadata
   (lambda ()
     (should (boundp 'custom-face-attributes))
     (should (equal (mapcar #'car custom-face-attributes)
                    '(:family :foundry :width :height :weight :slant
                      :underline :overline :strike-through :box
                      :inverse-video :foreground :distant-foreground
                      :background :stipple :extend :inherit)))
     (should (assq :inherit custom-face-attributes)))))

(ert-deftest emacs-stub-residuals-test/org-latex-and-related-uses-inherit-underline ()
  (emacs-stub-residuals-test--with-reloaded-custom-face-metadata
   (lambda ()
     (let* ((stub-source (emacs-stub-residuals-test--source-file "emacs-stub"))
            (org-faces-file
             (expand-file-name "../vendor/emacs-lisp/org/org-faces.el"
                               (file-name-directory stub-source)))
            (captured-spec nil)
            (form (emacs-stub-residuals-test--read-first-form-matching
                   org-faces-file
                   (lambda (candidate)
                     (and (consp candidate)
                          (eq (car candidate) 'defface)
                          (eq (cadr candidate) 'org-latex-and-related))))))
       (should form)
       (cl-letf (((symbol-function 'custom-declare-face)
                  (lambda (_face spec _doc &rest _args)
                    (setq captured-spec spec)
                    nil)))
         (eval form))
       (should captured-spec)
       (should (equal '(:inherit underline)
                      (cadr (assq 't captured-spec))))))))

(ert-deftest emacs-stub-residuals-test/custom-metadata-fallback-semantics ()
  (emacs-stub-residuals-test--with-reloaded-custom-fallbacks
   (lambda ()
     (let ((group (make-symbol "nelisp-emacs-custom-group"))
           (option (make-symbol "nelisp-emacs-custom-option"))
           (symbol (make-symbol "nelisp-emacs-custom-symbol")))
       (should (not (subrp (symbol-function 'custom-add-to-group))))
       (custom-add-to-group group option 'wid)
       (custom-add-to-group group option 'wid)
       (custom-add-to-group group option 'wid2)
       (should (equal (get group 'custom-group)
                      (list (list option 'wid)
                            (list option 'wid2))))
       (custom-add-link symbol 'wid)
       (custom-add-link symbol 'wid2)
       (custom-add-link symbol 'wid)
       (should (equal (get symbol 'custom-links) '(wid2 wid)))
       (custom-add-version symbol 'old)
       (custom-add-version symbol 'new)
       (should (equal (get symbol 'custom-version) 'new))
       (custom-add-package-version symbol 'old)
       (custom-add-package-version symbol 'new)
       (should (equal (get symbol 'custom-package-version) 'new))
       (put symbol 'custom-dependencies '(a b))
       (custom-add-dependencies symbol '(b c a d))
       (should (equal (get symbol 'custom-dependencies) '(d c a b)))
       (custom-add-load symbol 'load-a)
       (custom-add-load symbol 'load-a)
       (custom-add-load symbol 'load-b)
       (should (equal (get symbol 'custom-loads) '(load-b load-a)))))))

(ert-deftest emacs-stub-residuals-test/customize-package-emacs-version-alist-local-add-to-list ()
  (should (boundp 'customize-package-emacs-version-alist))
  (should (listp customize-package-emacs-version-alist))
  (let ((host-value (copy-tree (symbol-value 'customize-package-emacs-version-alist)))
        (entry '(so-long ("33" . "30.1"))))
    (let ((customize-package-emacs-version-alist nil))
      (add-to-list 'customize-package-emacs-version-alist entry)
      (add-to-list 'customize-package-emacs-version-alist entry)
      (should (equal customize-package-emacs-version-alist (list entry))))
    (should (equal (symbol-value 'customize-package-emacs-version-alist) host-value))))

(ert-deftest emacs-stub-residuals-test/custom-current-group-fallback-semantics ()
  (emacs-stub-residuals-test--with-reloaded-custom-fallbacks
   (lambda ()
      (let ((custom-current-group-alist '(("/tmp/custom.el" . group-one)
                                        ("/tmp/custom.el.bak" . group-two)))
           (load-file-name-was-bound (boundp 'load-file-name))
           (saved-load-file-name (and (boundp 'load-file-name)
                                     load-file-name)))
       (unwind-protect
           (progn
             (setq load-file-name "/tmp/custom.el")
             (should (eq (custom-current-group) 'group-one))
             (setq load-file-name "/tmp/custom.el.bak")
             (should (eq (custom-current-group) 'group-two))
             (setq load-file-name "/tmp/custom.el~")
             (should (null (custom-current-group)))
             (setq load-file-name nil)
             (should (null (custom-current-group))))
         (if load-file-name-was-bound
             (setq load-file-name saved-load-file-name)
           (makunbound 'load-file-name)))))))

(ert-deftest emacs-stub-residuals-test/custom-current-group-restores-host-binding ()
  (let* ((custom-current-group-was-bound (fboundp 'custom-current-group))
         (custom-current-group-host-fn (and custom-current-group-was-bound
                                           (symbol-function 'custom-current-group)))
         (custom-current-group-host-subr (and custom-current-group-host-fn
                                            (subrp custom-current-group-host-fn))))
    (emacs-stub-residuals-test--with-reloaded-custom-fallbacks
     (lambda ()
       (let ((custom-current-group-alist '(("/tmp/custom.el" . group-one)))
             (load-file-name-was-bound (boundp 'load-file-name))
             (saved-load-file-name (and (boundp 'load-file-name)
                                       load-file-name)))
         (unwind-protect
             (progn
               (setq load-file-name "/tmp/custom.el")
               (should (not (subrp (symbol-function 'custom-current-group))))
               (should (eq (custom-current-group) 'group-one))
               (setq load-file-name nil)
               (should (null (custom-current-group))))
           (if load-file-name-was-bound
               (setq load-file-name saved-load-file-name)
             (makunbound 'load-file-name))))))
    (when custom-current-group-was-bound
      (should (eq custom-current-group-host-fn (symbol-function 'custom-current-group)))
      (when custom-current-group-host-subr
        (should (subrp (symbol-function 'custom-current-group))))
      (should (fboundp 'custom-current-group)))))

(ert-deftest emacs-stub-residuals-test/custom-handle-keyword-fallback-validation ()
  (emacs-stub-residuals-test--with-reloaded-custom-fallbacks
   (lambda ()
     (let ((symbol (make-symbol "nelisp-emacs-custom-keyword"))
           (group (make-symbol "nelisp-emacs-custom-keyword-group")))
       (should (not (subrp (symbol-function 'custom-handle-keyword))))
       (should (equal (custom-handle-keyword symbol :load 'load-a 'custom-variable)
                      '(load-a)))
       (should (equal (get symbol 'custom-loads) '(load-a)))
       (should (equal (custom-handle-keyword symbol :group group 'custom-variable)
                      (list (list symbol 'custom-variable))))
       (should (equal (get group 'custom-group)
                      (list (list symbol 'custom-variable))))
       (should (equal (custom-handle-keyword symbol :version '1 'custom-variable)
                      1))
       (should (equal (get symbol 'custom-version) 1))
       (should (equal (custom-handle-keyword symbol :package-version '2 'custom-variable)
                      2))
       (should (equal (get symbol 'custom-package-version) 2))
       (should (equal (custom-handle-keyword symbol :link 'widget 'custom-variable)
                      '(widget)))
       (should (equal (get symbol 'custom-links) '(widget)))
       (should (equal (custom-handle-keyword symbol :set-after '(dep-a dep-b) 'custom-variable)
                      '(dep-b dep-a)))
       (should (equal (get symbol 'custom-dependencies) '(dep-b dep-a)))
       (should (equal (custom-handle-keyword symbol :tag 'tagged 'custom-variable)
                      'tagged))
       (should (equal (get symbol 'custom-tag) 'tagged))
       (should (equal (emacs-stub-residuals-test--error-message
                       (lambda ()
                         (custom-handle-keyword symbol :bogus 'v 'custom-variable)))
                      "Unknown keyword :bogus"))
       (should (equal (emacs-stub-residuals-test--error-message
                       (lambda ()
                         (custom-handle-all-keywords
                          symbol '(:load load-a :group) 'custom-variable)))
                      "Keyword :group is missing an argument"))
       (should (equal (emacs-stub-residuals-test--error-message
                       (lambda ()
                         (custom-handle-all-keywords
                          symbol '(:load load-a 42 y) 'custom-variable)))
                      "Junk in args (y)"))))))

(ert-deftest emacs-stub-residuals-test/custom-declarations-have-standalone-fallbacks ()
  (let ((file (emacs-stub-residuals-test--source-file "emacs-stub")))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((source (buffer-string)))
        (should (string-match-p "defmacro defgroup" source))
        (should (string-match-p "defmacro defcustom" source))
        (should (string-match-p "defun custom-declare-variable" source))
        (should (string-match-p "defun custom-declare-face" source))
        (should (string-match-p "standard-value" source))
        (should (string-match-p "custom-args" source))))))

(ert-deftest emacs-stub-residuals-test/defcustom-resync-is-version-skew-guarded ()
  (let ((file (emacs-stub-residuals-test--source-file "emacs-stub")))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((source (buffer-string)))
        (should (string-match-p
                 "when (fboundp 'nelisp--defvaralias-resync)"
                 source))))))

(ert-deftest emacs-stub-residuals-test/headless-selection-storage-roundtrip ()
  (let ((emacs-stub--selection-storage nil))
    (should (equal "clip"
                   (emacs-stub--set-selection 'CLIPBOARD "clip")))
    (should (equal "clip"
                   (emacs-stub--get-selection 'CLIPBOARD)))
    (should (equal "primary"
                   (emacs-stub--set-selection nil "primary")))
    (should (equal "primary"
                   (emacs-stub--get-selection 'PRIMARY)))
    (should (equal "clip-2"
                   (emacs-stub--set-selection 'CLIPBOARD "clip-2")))
    (should (equal "clip-2"
                   (emacs-stub--get-selection 'CLIPBOARD)))))

(ert-deftest emacs-stub-residuals-test/file-size-human-readable-kilobytes ()
  (should (equal "10k" (file-size-human-readable 10240))))

(ert-deftest emacs-stub-residuals-test/parse-colon-path-oracle-cases ()
  (should (equal '("/usr/bin/" "/home/x/bin/")
                 (parse-colon-path "/usr/bin:/home/x/bin")))
  (should (equal '(nil)
                 (parse-colon-path "")))
  (should (equal '("/usr/bin/" nil "/home/x/bin/")
                 (parse-colon-path "/usr/bin::/home/x/bin"))))

(ert-deftest emacs-stub-residuals-test/convert-standard-filename-identity ()
  (should (equal "~/.notes" (convert-standard-filename "~/.notes"))))

(ert-deftest emacs-stub-residuals-test/string-to-list-character-codes ()
  (should (equal '(65 122 48) (string-to-list "Az0"))))

(ert-deftest emacs-stub-residuals-test/vendor-load-helpers-have-fallbacks ()
  (let ((file (emacs-stub-residuals-test--source-file "emacs-stub")))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((source (buffer-string)))
        (should (string-match-p "defun regexp-opt" source))
        (should (string-match-p "defmacro easy-menu-define" source))
        (should (string-match-p "defun easy-menu-add-item" source))
        (should (string-match-p "defun current-idle-time" source))
        (should (string-match-p "defun shell-command-to-string" source))
        (should (string-match-p "defun call-process-shell-command" source))
        (should (string-match-p "defmacro bound-and-true-p" source))
        (should (string-match-p "defun emacs-stub--add-hook" source))
        (should (string-match-p "defun emacs-stub--run-hook" source))
        (should (string-match-p "defun emacs-stub--advice-add" source))
        (should (string-match-p "defun advice-add" source))
        (should (string-match-p "defmacro syntax-propertize-rules" source))
        (should (string-match-p "defun make-syntax-table" source))
        (should (string-match-p "defmacro cc-require" source))
        (should (string-match-p "defmacro cc-provide" source))
        (should (string-match-p "defun version<" source))
        (should (string-match-p "defun version<=" source))
        (should (string-match-p "defmacro combine-change-calls" source))
        (should (string-match-p "defmacro define-advice" source))
        (should (string-match-p "defun c-add-style" source))
        (should (string-match-p "cpp-font-lock-keywords" source))))))

(ert-deftest emacs-stub-residuals-test/help-macro-shims-present ()
  (let ((file (emacs-stub-residuals-test--source-file "emacs-stub")))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((source (buffer-string)))
        (should (string-match-p "defvar three-step-help" source))
        (should (string-match-p "defvar help-for-help-use-variable-pitch" source))
        (should (string-match-p "defun help--help-screen" source))
        (should (string-match-p "defmacro make-help-screen" source))
        (should (string-match-p "provide 'help-macro" source))))))

(ert-deftest emacs-stub-residuals-test/version-compare-numeric-components ()
  (should (= -1 (emacs-stub--version-compare "27.1" "29")))
  (should (= -1 (emacs-stub--version-compare "29" "29.1")))
  (should (= 0 (emacs-stub--version-compare "29" "29.0")))
  (should (= 1 (emacs-stub--version-compare "30.1" "29.99")))
  (should (= -1 (emacs-stub--version-compare "29.0.50" "29.1"))))

(ert-deftest emacs-stub-residuals-test/version-to-list-matches-host-oracle ()
  (dolist (version '(".5"
                     "0.9 alpha"
                     "0.9AlphA1"
                     "0.9snapshot"
                     "1.0-git"
                     "1.0.7.5"
                     "1.0.cvs"
                     "1.0PRE2"
                     "1.0pre2"
                     "22.8 Beta3"
                     "22.8beta3"))
    (should (equal (version-to-list version)
                   (funcall (symbol-function 'version-to-list) version)))))

(ert-deftest emacs-stub-residuals-test/version-list-ordering-semantics ()
  (should (version-list-< '(1 -1) '(1 0)))
  (should (version-list-< '(1 0 9) '(1 1)))
  (should (version-list-<= '(1 0) '(1)))
  (should (version-list-<= '(1 -2) '(1 -1)))
  (should-not (version-list-< '(1 0) '(1 0 0))))

(ert-deftest emacs-stub-residuals-test/buttonize-headless-fallback-shape ()
  (let ((emacs-stub--buttonize-state 'fallback))
    (should (equal "plain"
                   (buttonize "plain" #'ignore nil "help")))))

(ert-deftest emacs-stub-residuals-test/keyboard-and-xterm-shims-are-noops ()
  (let ((file (emacs-stub-residuals-test--source-file "emacs-stub")))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((source (buffer-string)))
        (should (string-match-p "defun set-keyboard-coding-system" source))
        (should (string-match-p "defun terminal-init-xterm" source))
        (should (string-match-p "Headless standalone fallback" source))))))

;;;; H. Idempotence — re-loading emacs-stub leaves bindings unchanged

(ert-deftest emacs-stub-residuals-test/require-is-idempotent ()
  (let ((before-define-key-after     (symbol-function 'define-key-after))
        (before-window-live-p        (symbol-function 'window-live-p))
        (before-frame-selected-win   (symbol-function 'frame-selected-window))
        (before-display-graphic-p    (symbol-function 'display-graphic-p)))
    (require 'emacs-stub)
    (should (eq before-define-key-after   (symbol-function 'define-key-after)))
    (should (eq before-window-live-p      (symbol-function 'window-live-p)))
    (should (eq before-frame-selected-win (symbol-function 'frame-selected-window)))
    (should (eq before-display-graphic-p  (symbol-function 'display-graphic-p)))))

;;;; I. Doc 16 breadth — foundational subr builtins (xor / ntake / char-uppercase-p)

(ert-deftest emacs-stub-residuals-test/doc16-breadth-subr-builtins ()
  "Doc 16 breadth: `xor' / `ntake' / `char-uppercase-p' were void in the
standalone runtime.  Host Emacs supplies the real builtins, so this pins
the contract the gated polyfills mirror."
  ;; xor
  (should (eq t (xor t nil)))
  (should (eq 5 (xor nil 5)))
  (should-not (xor t t))
  (should-not (xor nil nil))
  ;; ntake (destructive prefix)
  (should (equal '(1 2) (ntake 2 (list 1 2 3 4))))
  (should-not (ntake 0 (list 1 2)))
  (should (equal '(1 2 3) (ntake 5 (list 1 2 3))))
  ;; char-uppercase-p
  (should (char-uppercase-p ?A))
  (should (char-uppercase-p ?Z))
  (should-not (char-uppercase-p ?a))
  (should-not (char-uppercase-p ?5)))

;;;; J. Doc 16 round 7 — subr.el binding macros (ignore-error / while-let / and-let*)

(ert-deftest emacs-stub-residuals-test/doc16-round7-binding-macros ()
  "Doc 16 round 7: ignore-error / while-let / and-let* were void in the
standalone runtime; this pins the contract the gated shims mirror."
  ;; ignore-error
  (should (equal 42 (ignore-error error 42)))
  (should-not (ignore-error error (error "boom") 5))
  ;; while-let (0 is non-nil, so the loop runs three times)
  (should (equal '(2 1 0)
                 (let ((i 0) (acc nil))
                   (while-let ((x (and (< i 3) i)))
                     (push x acc)
                     (setq i (1+ i)))
                   acc)))
  ;; and-let*
  (should (equal 11 (and-let* ((x 5) (y (1+ x))) (+ x y))))
  (should-not (and-let* ((x 5) (y nil)) (+ x y)))
  (should (equal 5 (and-let* ((x 5)))) )       ; empty body -> last binding value
  (should (equal 7 (and-let* ((x 5) (y 7)))))
  (should (equal 5 (and-let* ((x 5) ((> x 3))) x))))

;;;; K. Doc 16 round 8 — extra setf places + with-memoization

(ert-deftest emacs-stub-residuals-test/doc16-round8-setf-and-memoization ()
  "Doc 16 round 8: gethash/get setf places + with-memoization.  On host
these use gv.el / the real macro; the standalone shims pin this contract."
  ;; setf places
  (let ((h (make-hash-table)))
    (setf (gethash 'k h) 9)
    (should (equal 9 (gethash 'k h))))
  (setf (get 'emacs-stub-residuals-test--r8 'prop) 7)
  (should (equal 7 (get 'emacs-stub-residuals-test--r8 'prop)))
  ;; with-memoization caches and does not re-run the body on a hit
  (let ((h (make-hash-table)) (n 0))
    (let ((a (with-memoization (gethash 'k h) (setq n (1+ n)) 100))
          (b (with-memoization (gethash 'k h) (setq n (1+ n)) 200)))
      (should (equal 100 a))
      (should (equal 100 b))
      (should (equal 1 n)))))

;;;; L. Doc 16 round 12 — subr.el / macroexp list helpers

(ert-deftest emacs-stub-residuals-test/doc16-round12-subr-list-helpers ()
  "Doc 16 round 12: delete-consecutive-dups / rassq-delete-all / macroexp-quote."
  (should (equal '(1 2 3 1) (delete-consecutive-dups (list 1 1 2 2 2 3 1 1))))
  (should (equal '(1 2 3) (delete-consecutive-dups (list 1 2 3 1) t)))
  (should (equal '((a . 1))
                 (rassq-delete-all 2 (list (cons 'a 1) (cons 'b 2) (cons 'c 2)))))
  (should (equal 5 (macroexp-quote 5)))
  (should (equal :k (macroexp-quote :k)))
  (should (equal "x" (macroexp-quote "x")))
  (should (equal '(quote foo) (macroexp-quote 'foo)))
  (should (equal '(quote (1 2)) (macroexp-quote (list 1 2)))))

;;;; M. Doc 16 round 15 — copy-hash-table (unblocks map-copy on hash tables)

(ert-deftest emacs-stub-residuals-test/doc16-round15-copy-hash-table ()
  "Doc 16 round 15: copy-hash-table was void, breaking map-copy on hashes."
  (let ((h (make-hash-table)))
    (puthash 'a 1 h)
    (puthash 'b 2 h)
    (let ((c (copy-hash-table h)))
      (should (equal 1 (gethash 'a c)))
      (should (equal 2 (gethash 'b c)))
      (should (equal 2 (hash-table-count c)))
      ;; the copy is independent of the original
      (puthash 'z 9 c)
      (should (equal 9 (gethash 'z c)))
      (should-not (gethash 'z h)))))

(provide 'emacs-stub-residuals-test)

;;; emacs-stub-residuals-test.el ends here
