;;; nemacs-loadup-test.el --- ERT for nemacs-loadup  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Track J bootstrap entry point.  Verifies that the
;; whole stack (= emacs-init → all bridges → fundamental-mode +
;; scratch buffer) can be wired up cleanly under host Emacs.

;;; Code:

(require 'ert)
(require 'nemacs-loadup)
(require 'cl-lib)

(defmacro nemacs-loadup-test--with-fresh-bootstrap (&rest body)
  "Run BODY with a clean bootstrap state."
  (declare (indent 0) (debug (body)))
  `(progn
     (nemacs-uninit)
     (let ((nemacs-startup-hook nil)
           (init-file-user nil))
       (unwind-protect
           (progn ,@body)
         (nemacs-uninit)))))

;;;; A. Load cleanly

(ert-deftest nemacs-loadup-test/require-loads-cleanly ()
  (should (featurep 'nemacs-loadup))
  (should (featurep 'emacs-init))
  (should (boundp 'nemacs-version))
  (should (boundp 'nemacs-startup-hook))
  (should (boundp 'nemacs-initialized))
  (should (fboundp 'nemacs-init))
  (should (fboundp 'nemacs-uninit))
  (should (fboundp 'nemacs-status)))

;;;; B. Version is non-empty

(ert-deftest nemacs-loadup-test/version-is-non-empty-string ()
  (should (stringp nemacs-version))
  (should (> (length nemacs-version) 0)))

;;;; C. nemacs-init flips initialized flag + returns 'ready

(ert-deftest nemacs-loadup-test/init-flips-flag ()
  (nemacs-loadup-test--with-fresh-bootstrap
    (should-not nemacs-initialized)
    (let ((r (nemacs-init t)))
      (should (eq 'ready r)))
    (should nemacs-initialized)))

;;;; D. nemacs-init twice signals

(ert-deftest nemacs-loadup-test/init-twice-signals ()
  (nemacs-loadup-test--with-fresh-bootstrap
    (nemacs-init t)
    (should-error (nemacs-init t) :type 'nemacs-already-initialized)))

;;;; E. nemacs-uninit resets the flag

(ert-deftest nemacs-loadup-test/uninit-resets ()
  (nemacs-loadup-test--with-fresh-bootstrap
    (nemacs-init t)
    (should nemacs-initialized)
    (nemacs-uninit)
    (should-not nemacs-initialized)
    ;; Re-init OK after uninit.
    (let ((r (nemacs-init t)))
      (should (eq 'ready r)))))

;;;; F. nemacs-startup-hook fires

(ert-deftest nemacs-loadup-test/startup-hook-fires ()
  (nemacs-loadup-test--with-fresh-bootstrap
    (let* ((fired 0)
           (nemacs-startup-hook
            (list (lambda () (setq fired (1+ fired))))))
      (nemacs-init t)
      (should (= 1 fired)))))

;;;; G. fundamental-mode is active after init

(ert-deftest nemacs-loadup-test/fundamental-mode-after-init ()
  (nemacs-loadup-test--with-fresh-bootstrap
    (nemacs-init t)
    (should (eq 'fundamental-mode (emacs-mode-major-mode)))))

;;;; H. scratch buffer is created

(ert-deftest nemacs-loadup-test/scratch-buffer-created ()
  (nemacs-loadup-test--with-fresh-bootstrap
    (should-not nemacs--initial-buffer)
    (nemacs-init t)
    (should nemacs--initial-buffer)))

;;;; I. nemacs-status returns the expected keys

(ert-deftest nemacs-loadup-test/status-keys-present ()
  (nemacs-loadup-test--with-fresh-bootstrap
    (nemacs-init t)
    (let ((s (nemacs-status)))
      (should (plist-member s :version))
      (should (plist-member s :initialized))
      (should (plist-member s :initial-buffer))
      (should (plist-member s :major-mode))
      (should (plist-member s :feature-count))
      (should (plist-get s :initialized))
      (should (eq 'fundamental-mode (plist-get s :major-mode))))))

;;;; J. Full feature surface is loaded after bootstrap

(ert-deftest nemacs-loadup-test/full-feature-surface-loaded ()
  (nemacs-loadup-test--with-fresh-bootstrap
    (nemacs-init t)
    ;; A representative slice of every Track's bridge module.
    (dolist (feat '(emacs-init
                    nelisp-emacs
                    nemacs-init-transport
                    emacs-buffer-builtins
                    emacs-search-builtins
                    emacs-line-builtins
                    emacs-io
                    emacs-fileio-builtins
                    emacs-edit-builtins
                    emacs-minibuffer-builtins
                    emacs-keymap-builtins
                    emacs-frame-builtins
                    emacs-window-builtins
                    emacs-command-loop-builtins
                    emacs-undo-builtins
                    emacs-faces-builtins
                    emacs-mode-builtins
                    emacs-process-builtins))
      (should (featurep feat)))))

;;;; K. UX #18 Session A — `init-file-user' nil gates user init loading

(defmacro nemacs-loadup-test--with-fixture-init (&rest body)
  "Run BODY with `NEMACS_USER_EMACS_DIRECTORY' pointing at a fresh temp
directory containing an init.el that flips a marker variable, so BODY can
observe whether `nemacs-load-user-init-files' actually loaded it."
  (declare (indent 0) (debug (body)))
  `(let* ((dir (file-name-as-directory (make-temp-file "nemacs-loadup-test-fixture-" t)))
          (process-environment
           (cons (format "NEMACS_USER_EMACS_DIRECTORY=%s" dir)
                 process-environment)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "init.el" dir)
             (insert "(defvar nemacs-loadup-test--fixture-init-ran nil)\n"
                     "(setq nemacs-loadup-test--fixture-init-ran t)\n"))
           ,@body)
       (delete-directory dir t))))

(ert-deftest nemacs-loadup-test/init-file-user-nil-skips-fixture-init ()
  "-q equivalent: init-file-user nil must not load the fixture's init.el."
  (nemacs-loadup-test--with-fixture-init
    (let ((init-file-user nil)
          (nemacs-init-file-loaded nil)
          (nemacs-user-emacs-directory nil)
          (user-init-file nil)
          (early-init-file nil))
      (defvar nemacs-loadup-test--fixture-init-ran nil)
      (setq nemacs-loadup-test--fixture-init-ran nil)
      (cl-letf (((symbol-function 'nemacs-activate-packages-at-startup)
                 (lambda () nil)))
        (nemacs-load-user-init-files))
      (should-not nemacs-loadup-test--fixture-init-ran)
      ;; Only the *loading* is skipped; the "init handling is done" flag
      ;; still flips, matching Emacs's own `-q' contract.
      (should nemacs-init-file-loaded))))

(ert-deftest nemacs-loadup-test/init-file-user-non-nil-loads-fixture-init ()
  "Control case: a non-nil init-file-user still loads the fixture's init.el."
  (nemacs-loadup-test--with-fixture-init
    (let ((init-file-user "")
          (nemacs-init-file-loaded nil)
          (nemacs-user-emacs-directory nil)
          (user-init-file nil)
          (early-init-file nil))
      (defvar nemacs-loadup-test--fixture-init-ran nil)
      (setq nemacs-loadup-test--fixture-init-ran nil)
      (cl-letf (((symbol-function 'nemacs-activate-packages-at-startup)
                 (lambda () nil)))
        (nemacs-load-user-init-files))
      (should nemacs-loadup-test--fixture-init-ran)
      (should nemacs-init-file-loaded))))

;;;; L. Loader reconcile Phase 3 — ~/.nemacs.d resolver precedence

(defmacro nemacs-loadup-test--with-fake-home (&rest body)
  "Run BODY with HOME pointing at a fresh empty temp directory and
`NEMACS_USER_EMACS_DIRECTORY'/`nemacs-user-emacs-directory' cleared, so
`nemacs-resolve-user-emacs-directory' exercises its real-directory
precedence tiers against a controlled filesystem instead of the
developer's actual dotfiles."
  (declare (indent 0) (debug (body)))
  `(let* ((home (directory-file-name (make-temp-file "nemacs-loadup-test-home-" t)))
          (process-environment
           (append (list (format "HOME=%s" home)
                         "NEMACS_USER_EMACS_DIRECTORY=")
                   process-environment))
          (nemacs-user-emacs-directory nil))
     (unwind-protect
         (progn ,@body)
       (delete-directory home t))))

(ert-deftest nemacs-loadup-test/resolver-defaults-to-dot-emacs-d-when-nothing-present ()
  "With neither ~/.nemacs.d nor ~/.emacs.d/~/.emacs/XDG present, the
resolver falls back to ~/.emacs.d (unchanged pre-Phase-3 default)."
  (nemacs-loadup-test--with-fake-home
    (should (equal (concat (getenv "HOME") "/.emacs.d/")
                    (nemacs-resolve-user-emacs-directory)))))

(ert-deftest nemacs-loadup-test/resolver-prefers-dot-emacs-d-when-only-that-exists ()
  "~/.emacs.d precedence is unaffected when no ~/.nemacs.d exists (native
driver back-compat)."
  (nemacs-loadup-test--with-fake-home
    (make-directory (concat (getenv "HOME") "/.emacs.d"))
    (should (equal (concat (getenv "HOME") "/.emacs.d/")
                    (nemacs-resolve-user-emacs-directory)))))

(ert-deftest nemacs-loadup-test/resolver-prefers-nemacs-d-over-dot-emacs-d ()
  "Loader reconcile Phase 3: when both ~/.nemacs.d and ~/.emacs.d exist,
~/.nemacs.d (the historical GUI M15 wrapped-init lane directory) now wins
-- the single resolver every driver/launcher defers to."
  (nemacs-loadup-test--with-fake-home
    (make-directory (concat (getenv "HOME") "/.emacs.d"))
    (make-directory (concat (getenv "HOME") "/.nemacs.d"))
    (should (equal (concat (getenv "HOME") "/.nemacs.d/")
                    (nemacs-resolve-user-emacs-directory)))))

(ert-deftest nemacs-loadup-test/resolver-env-override-wins-over-nemacs-d ()
  "An explicit `NEMACS_USER_EMACS_DIRECTORY' override still wins over the
~/.nemacs.d tier (test/frontend isolation is never shadowed)."
  (nemacs-loadup-test--with-fake-home
    (make-directory (concat (getenv "HOME") "/.nemacs.d"))
    (let* ((override (file-name-as-directory (make-temp-file "nemacs-loadup-test-override-" t)))
           (process-environment
            (cons (format "NEMACS_USER_EMACS_DIRECTORY=%s" override)
                  process-environment)))
      (unwind-protect
          (should (equal override (nemacs-resolve-user-emacs-directory)))
        (delete-directory override t)))))

(provide 'nemacs-loadup-test)

;;; nemacs-loadup-test.el ends here
