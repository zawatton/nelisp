;;; emacs-mode-builtins-test.el --- ERT for emacs-mode  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 major-mode framework (Track H).  Under
;; host Emacs the unprefixed bridges (fundamental-mode etc.) are
;; gated off (= host's simple.el / files.el wins), so behavioural
;; assertions exercise the prefixed `emacs-mode-*' API directly
;; against the substrate state.  Featurep / fboundp / boundp parity
;; is checked separately.

;;; Code:

(require 'ert)
(require 'emacs-mode-builtins)
(require 'cl-lib)

(defmacro emacs-mode-builtins-test--with-fresh-mode (&rest body)
  "Run BODY with a clean substrate mode state."
  (declare (indent 0) (debug (body)))
  `(let ((emacs-mode--current-major-mode 'fundamental-mode)
         (emacs-mode--current-mode-name  "Fundamental")
         (emacs-mode--registered nil)
         (emacs-mode--auto-mode-alist nil))
     (emacs-mode-reset)
     (unwind-protect
         (progn ,@body)
       (emacs-mode-reset))))

;;;; A. Load cleanly + fboundp / boundp parity

(ert-deftest emacs-mode-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-mode-builtins))
  (should (featurep 'emacs-mode))
  (dolist (sym '(fundamental-mode text-mode emacs-lisp-mode
                 run-mode-hooks kill-all-local-variables
                 make-mode-line-mouse-map substitute-command-keys
                 set-auto-mode define-derived-mode))
    (should (fboundp sym)))
  (dolist (sym '(major-mode mode-name auto-mode-alist
                 fundamental-mode-hook text-mode-hook
                 emacs-lisp-mode-hook
                 change-major-mode-after-body-hook
                 after-change-major-mode-hook))
    (should (boundp sym))))

(ert-deftest emacs-mode-builtins-test/install-p-uses-function-cell ()
  (cl-letf (((symbol-function 'fboundp)
             (lambda (symbol)
               (or (eq symbol 'emacs-mode-builtins-test--missing)
                   (fboundp symbol)))))
    (should (emacs-mode-builtins--install-function-p
             'emacs-mode-builtins-test--missing))))

;;;; B. fundamental-mode

(ert-deftest emacs-mode-builtins-test/fundamental-mode-sets-vars ()
  (emacs-mode-builtins-test--with-fresh-mode
    (emacs-mode-fundamental-mode)
    (should (eq 'fundamental-mode (emacs-mode-major-mode)))
    (should (equal "Fundamental" (emacs-mode-mode-name)))))

;;;; C. text-mode

(ert-deftest emacs-mode-builtins-test/text-mode-sets-vars ()
  (emacs-mode-builtins-test--with-fresh-mode
    (emacs-mode-text-mode)
    (should (eq 'text-mode (emacs-mode-major-mode)))
    (should (equal "Text" (emacs-mode-mode-name)))))

;;;; D. emacs-lisp-mode

(ert-deftest emacs-mode-builtins-test/emacs-lisp-mode-sets-vars ()
  (emacs-mode-builtins-test--with-fresh-mode
    (emacs-mode-emacs-lisp-mode)
    (should (eq 'emacs-lisp-mode (emacs-mode-major-mode)))
    (should (equal "Emacs-Lisp" (emacs-mode-mode-name)))))

;;;; E. mode hooks fire

(ert-deftest emacs-mode-builtins-test/text-mode-hook-fires ()
  (emacs-mode-builtins-test--with-fresh-mode
    (let ((fired 0))
      (let ((emacs-mode-text-mode-hook
             (list (lambda () (setq fired (1+ fired))))))
        (emacs-mode-text-mode)
        (should (= 1 fired))))))

(ert-deftest emacs-mode-builtins-test/base-mode-prefixed-hooks-fire ()
  (emacs-mode-builtins-test--with-fresh-mode
    (let ((fundamental-fired 0)
          (elisp-fired 0))
      (let ((emacs-mode-fundamental-mode-hook
             (list (lambda () (setq fundamental-fired
                                    (1+ fundamental-fired)))))
            (emacs-mode-emacs-lisp-mode-hook
             (list (lambda () (setq elisp-fired (1+ elisp-fired))))))
        (emacs-mode-fundamental-mode)
        (should (= 1 fundamental-fired))
        (emacs-mode-emacs-lisp-mode)
        ;; `emacs-lisp-mode' derives through `fundamental-mode'.
        (should (= 2 fundamental-fired))
        (should (= 1 elisp-fired))))))

;;;; F. define-derived-mode (the macro)

(emacs-mode-define-derived-mode my-test-derived-mode emacs-mode-text-mode
  "MyDerived"
  "Test-only derived mode for ERT."
  ;; body: nothing.
  )

(emacs-mode-define-derived-mode my-test-parent-derived-mode emacs-mode-text-mode
  "MyParentDerived"
  "Test-only parent derived mode for nested mode checks.")

(emacs-mode-define-derived-mode my-test-nested-derived-mode
  my-test-parent-derived-mode
  "MyNestedDerived"
  "Test-only nested derived mode for ERT."
  (setq my-test-nested-derived-body-mode major-mode))

(defvar my-test-derived-sequence-events nil
  "Event list used by the derived-mode sequencing regression test.")

(emacs-mode-define-derived-mode my-test-derived-sequence-mode
  my-test-nested-derived-mode
  "MyDerivedSequence"
  "Test-only mode proving body forms survive after a nested parent."
  (push 'body-a my-test-derived-sequence-events)
  (push 'body-b my-test-derived-sequence-events))

(defvar my-test-derived-reassert-hook-mode nil
  "Mode observed by the derived-mode reassertion regression hook.")

(emacs-mode-define-derived-mode my-test-derived-reassert-mode
  emacs-mode-text-mode
  "MyDerivedReassert"
  "Test-only mode proving body-side mode drift is corrected before hooks."
  (setq major-mode 'wrong-mode)
  (setq emacs-mode--current-major-mode 'wrong-mode))

(ert-deftest emacs-mode-builtins-test/define-derived-mode-registers ()
  (emacs-mode-builtins-test--with-fresh-mode
    ;; Activate the test-defined derived mode.
    (my-test-derived-mode)
    (should (eq 'my-test-derived-mode (emacs-mode-major-mode)))
    (should (equal "MyDerived" (emacs-mode-mode-name)))))

(ert-deftest emacs-mode-builtins-test/define-derived-mode-creates-hook-var ()
  ;; The hook defvar must exist after macro expansion.
  (should (boundp 'my-test-derived-mode-hook))
  (should (boundp 'emacs-mode-my-test-derived-mode-hook)))

(ert-deftest emacs-mode-builtins-test/define-derived-mode-runs-parent ()
  (emacs-mode-builtins-test--with-fresh-mode
    (let ((parent-fired 0))
      (let ((emacs-mode-text-mode-hook
             (list (lambda () (setq parent-fired (1+ parent-fired))))))
        (my-test-derived-mode)
        ;; Parent's hook fired (= because parent ran before body).
        (should (= 1 parent-fired))))))

(ert-deftest emacs-mode-builtins-test/define-derived-mode-nested-finalizes-child ()
  (emacs-mode-builtins-test--with-fresh-mode
    (setq my-test-nested-derived-body-mode nil)
    (my-test-nested-derived-mode)
    (should (eq 'my-test-nested-derived-mode (emacs-mode-major-mode)))
    (should (eq 'my-test-nested-derived-mode major-mode))
    (should (eq 'my-test-nested-derived-mode
                my-test-nested-derived-body-mode))))

(ert-deftest emacs-mode-builtins-test/define-derived-mode-sequences-after-parent ()
  (emacs-mode-builtins-test--with-fresh-mode
    (let ((my-test-derived-sequence-mode-hook
           (list (lambda ()
                   (push 'hook my-test-derived-sequence-events)))))
      (setq my-test-derived-sequence-events nil)
      (my-test-derived-sequence-mode)
      (should (eq 'my-test-derived-sequence-mode (emacs-mode-major-mode)))
      (should (equal '(hook body-b body-a)
                     my-test-derived-sequence-events)))))

(ert-deftest emacs-mode-builtins-test/define-derived-mode-reasserts-before-hooks ()
  (emacs-mode-builtins-test--with-fresh-mode
    (let ((my-test-derived-reassert-mode-hook
           (list (lambda ()
                   (setq my-test-derived-reassert-hook-mode major-mode)))))
      (setq my-test-derived-reassert-hook-mode nil)
      (my-test-derived-reassert-mode)
      (should (eq 'my-test-derived-reassert-mode major-mode))
      (should (eq 'my-test-derived-reassert-mode (emacs-mode-major-mode)))
      (should (eq 'my-test-derived-reassert-mode
                  my-test-derived-reassert-hook-mode)))))

;;;; F2. define-derived-mode host parity — MODE-map / -syntax-table /
;;;;     -abbrev-table / -hook, keyword args, and existing-value
;;;;     preservation, matching real Emacs's `derived.el'.

(define-derived-mode t58-parity-host-mode special-mode "T58Host"
  "Host-macro fixture for the T58 `define-derived-mode' parity test.")

(emacs-mode-define-derived-mode t58-parity-standalone-mode special-mode
  "T58Standalone"
  "Standalone-macro fixture for the T58 `define-derived-mode' parity test.")

(ert-deftest emacs-mode-builtins-test/define-derived-mode-matches-host-derived-symbols ()
  "`emacs-mode-define-derived-mode' must define the same kind of
MODE-map / MODE-syntax-table / MODE-abbrev-table / MODE-hook symbols,
with the same parent-chaining behaviour, as host Emacs's own
`define-derived-mode' produces for an equivalent form (both derived
from `special-mode', so both inherit a real, populated parent map)."
  (with-temp-buffer (t58-parity-host-mode))
  (with-temp-buffer (t58-parity-standalone-mode))
  (should (boundp 't58-parity-host-mode-hook))
  (should (boundp 't58-parity-standalone-mode-hook))
  (should (keymapp t58-parity-host-mode-map))
  (should (keymapp t58-parity-standalone-mode-map))
  (should (eq (keymap-parent t58-parity-host-mode-map) special-mode-map))
  (should (eq (keymap-parent t58-parity-standalone-mode-map) special-mode-map))
  (should (char-table-p t58-parity-host-mode-syntax-table))
  (should (char-table-p t58-parity-standalone-mode-syntax-table))
  (should (eq (char-table-subtype t58-parity-host-mode-syntax-table)
              'syntax-table))
  (should (eq (char-table-subtype t58-parity-standalone-mode-syntax-table)
              'syntax-table))
  (should (abbrev-table-p t58-parity-host-mode-abbrev-table))
  (should (abbrev-table-p t58-parity-standalone-mode-abbrev-table))
  (should (eq (get 't58-parity-host-mode 'derived-mode-parent) 'special-mode))
  (should (eq (get 't58-parity-standalone-mode 'derived-mode-parent)
              'special-mode)))

(ert-deftest emacs-mode-builtins-test/define-derived-mode-preserves-existing-map ()
  "A second expansion for the same CHILD must not clobber a MODE-map the
first expansion (or the user) already populated — mirrors GNU's `unless
(boundp ...)' guard around the MODE-map / MODE-syntax-table /
MODE-abbrev-table defvars."
  (emacs-mode-define-derived-mode t58-parity-reload-mode nil "T58Reload"
    "Reload fixture for the T58 existing-value-preservation test.")
  (define-key t58-parity-reload-mode-map (kbd "C-c C-c") 'ignore)
  ;; Re-expand, the same way reloading the defining file would, and
  ;; confirm the user's binding survived.
  (emacs-mode-define-derived-mode t58-parity-reload-mode nil "T58Reload"
    "Reload fixture for the T58 existing-value-preservation test.")
  (should (eq (lookup-key t58-parity-reload-mode-map (kbd "C-c C-c")) 'ignore)))

(ert-deftest emacs-mode-builtins-test/define-derived-mode-keyword-args ()
  "`:interactive nil' must omit `(interactive)'; `:after-hook' must run
once, after the mode hook."
  (let (events)
    (emacs-mode-define-derived-mode t58-parity-keyword-mode nil "T58Keyword"
      "Keyword-args fixture for the T58 parity test."
      :interactive nil
      :after-hook (push 'after-hook events)
      (push 'body events))
    (let ((t58-parity-keyword-mode-hook
           (list (lambda () (push 'hook events)))))
      (should-not (commandp 't58-parity-keyword-mode))
      (t58-parity-keyword-mode)
      (should (equal '(after-hook hook body) events)))))

;;;; G. run-mode-hooks

(ert-deftest emacs-mode-builtins-test/run-mode-hooks-fires-each ()
  (emacs-mode-builtins-test--with-fresh-mode
    (let* ((a-fired 0)
           (b-fired 0)
           (hook-a (list (lambda () (setq a-fired (1+ a-fired)))))
           (hook-b (list (lambda () (setq b-fired (1+ b-fired))))))
      (let ((my-hook-a hook-a)
            (my-hook-b hook-b))
        (defvar my-hook-a)
        (defvar my-hook-b)
        (set 'my-hook-a hook-a)
        (set 'my-hook-b hook-b)
        (emacs-mode-run-mode-hooks 'my-hook-a 'my-hook-b)
        (should (= 1 a-fired))
        (should (= 1 b-fired))))))

;;;; H. kill-all-local-variables resets to fundamental

(ert-deftest emacs-mode-builtins-test/kill-all-local-variables-resets ()
  (emacs-mode-builtins-test--with-fresh-mode
    (emacs-mode-text-mode)
    (should (eq 'text-mode (emacs-mode-major-mode)))
    (emacs-mode-kill-all-local-variables)
    (should (eq 'fundamental-mode (emacs-mode-major-mode)))))

(ert-deftest emacs-mode-builtins-test/set-major-mode-direct-contract ()
  (emacs-mode-builtins-test--with-fresh-mode
    (should (eq 'custom-mode
                (emacs-mode-set-major-mode 'custom-mode "Custom")))
    (should (eq 'custom-mode (emacs-mode-major-mode)))
    (should (equal "Custom" (emacs-mode-mode-name)))
    (should-error (emacs-mode-set-major-mode "bad-mode")
                  :type 'wrong-type-argument)))

;;;; I. auto-mode-alist + set-auto-mode

(ert-deftest emacs-mode-builtins-test/set-auto-mode-matches-extension ()
  (emacs-mode-builtins-test--with-fresh-mode
    (emacs-mode-set-auto-mode-alist
     '(("\\.txt\\'" . emacs-mode-text-mode)
       ("\\.el\\'"  . emacs-mode-emacs-lisp-mode)))
    (should (equal '(("\\.txt\\'" . emacs-mode-text-mode)
                     ("\\.el\\'"  . emacs-mode-emacs-lisp-mode))
                   (emacs-mode-auto-mode-alist)))
    (let ((m1 (emacs-mode-set-auto-mode "/tmp/x.txt")))
      (should (eq 'emacs-mode-text-mode m1))
      (should (eq 'text-mode (emacs-mode-major-mode))))
    (let ((m2 (emacs-mode-set-auto-mode "/tmp/y.el")))
      (should (eq 'emacs-mode-emacs-lisp-mode m2))
      (should (eq 'emacs-lisp-mode (emacs-mode-major-mode))))))

(ert-deftest emacs-mode-builtins-test/set-auto-mode-no-match-returns-nil ()
  (emacs-mode-builtins-test--with-fresh-mode
    (emacs-mode-set-auto-mode-alist '(("\\.txt\\'" . emacs-mode-text-mode)))
    (let ((r (emacs-mode-set-auto-mode "/tmp/x.html")))
      (should (null r)))))

;;;; J. Idempotent require

(ert-deftest emacs-mode-builtins-test/require-is-idempotent ()
  (let ((before-fund (symbol-function 'fundamental-mode))
        (before-text (symbol-function 'text-mode))
        (before-rmh  (symbol-function 'run-mode-hooks))
        (before-set-auto (symbol-function 'set-auto-mode)))
    (require 'emacs-mode-builtins)
    (should (eq before-fund (symbol-function 'fundamental-mode)))
    (should (eq before-text (symbol-function 'text-mode)))
    (should (eq before-rmh  (symbol-function 'run-mode-hooks)))
    (should (eq before-set-auto (symbol-function 'set-auto-mode)))))

(ert-deftest emacs-mode-builtins-test/bridge-overwrites-standalone-stubs-in-source ()
  (should (fboundp 'emacs-mode-builtins--install-function-p))
  (should-not (emacs-mode-builtins--install-function-p 'fundamental-mode))
  (let* ((file (locate-library "emacs-mode-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(fundamental-mode text-mode emacs-lisp-mode
                     run-mode-hooks kill-all-local-variables
                     set-auto-mode define-derived-mode))
        (goto-char (point-min))
        (should (search-forward
                 (format "(when (emacs-mode-builtins--install-function-p '%s)"
                         sym)
                 nil t))))))

(ert-deftest emacs-mode-builtins-test/install-gate-overwrites-bulk-stubs ()
  (let ((original (get 'define-derived-mode 'emacs-stub-bulk)))
    (unwind-protect
        (progn
          (put 'define-derived-mode 'emacs-stub-bulk t)
          (should (emacs-mode-builtins--install-function-p
                   'define-derived-mode)))
      (put 'define-derived-mode 'emacs-stub-bulk original))))

(ert-deftest emacs-mode-builtins-test/make-mode-line-mouse-map-binds-mode-line-event ()
  (let* ((map (emacs-mode-builtins--make-mode-line-mouse-map
               'mouse-1
               'calendar-scroll-right))
         (binding (lookup-key map (vector 'mode-line 'mouse-1))))
    (should (keymapp map))
    (should (eq binding 'calendar-scroll-right))))

(ert-deftest emacs-mode-builtins-test/substitute-command-keys-reduced-removes-map-and-expands-command ()
  (should
   (equal
    (emacs-mode-builtins--substitute-command-keys-reduced
     "\\<calendar-mode-map>\\[calendar-scroll-right] previous month")
    "M-x calendar-scroll-right previous month")))

(ert-deftest emacs-mode-builtins-test/substitute-command-keys-reduced-unescapes-quoted-forms ()
  (should
   (equal
    (emacs-mode-builtins--substitute-command-keys-reduced
     "literal \\=\\= and \\=\\[calendar-other-month]")
    "literal = and M-x calendar-other-month")))

(ert-deftest emacs-mode-builtins-test/headless-noop-modes-alias-ignore-in-source ()
  (let* ((file (locate-library "emacs-mode-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(global-hl-line-mode global-so-long-mode show-paren-mode))
        (goto-char (point-min))
        (should (search-forward
                 (format "(defalias ',name #'ignore)")
                 nil t))))))

;;;; I. mode-line-major-mode-keymap / mode-line-minor-mode-keymap (T62)

;; Both were void anywhere in src/ (load matrix: `minions' hit
;; `(void-variable mode-line-major-mode-keymap)', then, once fixed, the
;; sibling `(void-variable mode-line-minor-mode-keymap)').  These are
;; reduced values (real Emacs also binds a minor-modes/major-mode mouse
;; menu with no headless equivalent -- see the defvar commentary), so
;; tests assert "kind, not content" (Doc 51 T52 precedent) plus the one
;; real binding each keeps.
(ert-deftest emacs-mode-builtins-test/mode-line-major-mode-keymap-is-a-keymap ()
  (should (boundp 'mode-line-major-mode-keymap))
  (should (keymapp mode-line-major-mode-keymap))
  (should (eq 'describe-mode
              (lookup-key mode-line-major-mode-keymap [mode-line mouse-2]))))

(ert-deftest emacs-mode-builtins-test/mode-line-minor-mode-keymap-is-a-keymap ()
  (should (boundp 'mode-line-minor-mode-keymap))
  (should (keymapp mode-line-minor-mode-keymap))
  (should (eq 'mode-line-minor-mode-help
              (lookup-key mode-line-minor-mode-keymap [mode-line mouse-2])))
  (should (eq 'mouse-minor-mode-menu
              (lookup-key mode-line-minor-mode-keymap [mode-line down-mouse-1]))))

;;;; J. special-mode / special-mode-map (T62)

;; Both were void anywhere in src/ (load matrix: `diff-hl' hit
;; `(void-variable special-mode-map)' via real Emacs `vc-dir'/`log-view',
;; which derive their own major modes from `special-mode').  Real
;; Emacs's own `special-mode-map' bindings are copied verbatim (see the
;; defvar commentary in `emacs-mode-builtins.el'), so these assertions
;; hold against BOTH host Emacs's real `special-mode'/`special-mode-map'
;; (the `unless'/`when' install guards leave host's own C-preloaded
;; values untouched) and this substrate's ported values -- verified
;; against host Emacs 31.1's actual `special-mode-map' bindings.
(ert-deftest emacs-mode-builtins-test/special-mode-is-defined-special ()
  (should (fboundp 'special-mode))
  (should (eq 'special (get 'special-mode 'mode-class))))

(ert-deftest emacs-mode-builtins-test/special-mode-map-is-a-keymap ()
  (should (boundp 'special-mode-map))
  (should (keymapp special-mode-map)))

(ert-deftest emacs-mode-builtins-test/special-mode-map-bindings-match-gnu-simple ()
  (should (eq 'quit-window          (lookup-key special-mode-map "q")))
  (should (eq 'scroll-up-command    (lookup-key special-mode-map " ")))
  (should (eq 'scroll-down-command  (lookup-key special-mode-map "\d")))
  (should (eq 'describe-mode        (lookup-key special-mode-map "?")))
  (should (eq 'describe-mode        (lookup-key special-mode-map "h")))
  (should (eq 'end-of-buffer        (lookup-key special-mode-map ">")))
  (should (eq 'beginning-of-buffer  (lookup-key special-mode-map "<")))
  (should (eq 'revert-buffer        (lookup-key special-mode-map "g"))))

(provide 'emacs-mode-builtins-test)

;;; emacs-mode-builtins-test.el ends here
