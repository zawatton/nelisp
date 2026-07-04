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
                 set-auto-mode define-derived-mode))
    (should (fboundp sym)))
  (dolist (sym '(major-mode mode-name auto-mode-alist
                 fundamental-mode-hook text-mode-hook
                 emacs-lisp-mode-hook
                 change-major-mode-after-body-hook
                 after-change-major-mode-hook))
    (should (boundp sym))))

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

(provide 'emacs-mode-builtins-test)

;;; emacs-mode-builtins-test.el ends here
