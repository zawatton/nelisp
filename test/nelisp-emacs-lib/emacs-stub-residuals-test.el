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
                  convert-standard-filename string-to-list
                  regexp-quote regexp-opt easy-menu-define
                  easy-menu-add-item
                  current-idle-time shell-command-to-string
                  call-process-shell-command
                  bound-and-true-p
                  line-number-display-width
                  syntax-propertize-rules cc-require cc-provide
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
                  make-help-screen help--help-screen))
    (should (fboundp sym)))
  (should (featurep 'help-macro))
  (should (boundp 'emacs-display-system))
  (should (boundp 'emacs-basic-display))
  (should (boundp 'initial-window-system))
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

(ert-deftest emacs-stub-residuals-test/function-get-reads-symbol-property ()
  "Doc 15 B4 breadth: `function-get' returns a function symbol's property.
It was void on the reader, blocking `define-inline' / cl-generic users."
  (let ((sym (make-symbol "emacs-stub-test--fg")))
    (should (null (function-get sym 'no-such-prop)))
    (put sym 'my-prop 123)
    (should (equal 123 (function-get sym 'my-prop)))))

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

(ert-deftest emacs-stub-residuals-test/custom-declarations-have-standalone-fallbacks ()
  (let ((file (emacs-stub-residuals-test--source-file "emacs-stub")))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((source (buffer-string)))
        (should (string-match-p "defmacro defgroup" source))
        (should (string-match-p "defmacro defcustom" source))
        (should (string-match-p "standard-value" source))
        (should (string-match-p "custom-args" source))))))

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
