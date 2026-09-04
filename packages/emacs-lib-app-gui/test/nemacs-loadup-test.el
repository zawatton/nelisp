;;; nemacs-loadup-test.el --- ERT for nemacs-loadup  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Track J bootstrap entry point.  Verifies that the
;; whole stack (= emacs-init → all bridges → fundamental-mode +
;; scratch buffer) can be wired up cleanly under host Emacs.

;;; Code:

(require 'ert)
(require 'nemacs-loadup)
(require 'emacs-load)
(require 'cl-lib)

(defvar nemacs-loadup-test-order nil)

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

;;;; K. User init loading

(ert-deftest nemacs-loadup-test/user-init-loads-early-then-init ()
  (nemacs-loadup-test--with-fresh-bootstrap
    (let* ((dir (file-name-as-directory
                 (make-temp-file "nemacs-loadup-init-" t)))
           (process-environment (copy-sequence process-environment))
           (init-file-user "")
           (package-enable-at-startup nil)
           (nemacs-loadup-test-order nil))
      (unwind-protect
          (progn
            (setenv "NEMACS_USER_EMACS_DIRECTORY" dir)
            (write-region
             "(setq nemacs-loadup-test-order (append nemacs-loadup-test-order '(early)))\n"
             nil (expand-file-name "early-init.el" dir))
            (write-region
             "(setq nemacs-loadup-test-order (append nemacs-loadup-test-order '(init)))\n"
             nil (expand-file-name "init.el" dir))
            (nemacs-load-user-init-files)
            (should (equal nemacs-loadup-test-order '(early init)))
            (should (equal early-init-file
                           (expand-file-name "early-init.el" dir)))
            (should (equal user-init-file
                           (expand-file-name "init.el" dir)))
            (should (equal user-emacs-directory dir))
            (should nemacs-init-file-loaded)
            (should-not init-file-had-error)
            (should-not nemacs-init-file-error))
        (delete-directory dir t)))))

(ert-deftest nemacs-loadup-test/user-init-error-is-recorded ()
  (nemacs-loadup-test--with-fresh-bootstrap
    (let* ((dir (file-name-as-directory
                 (make-temp-file "nemacs-loadup-init-error-" t)))
           (process-environment (copy-sequence process-environment))
           (init-file-user "")
           (package-enable-at-startup nil))
      (unwind-protect
          (progn
            (setenv "NEMACS_USER_EMACS_DIRECTORY" dir)
            (write-region "(setq nemacs-loadup-test-early-ok t)\n"
                          nil (expand-file-name "early-init.el" dir))
            (write-region "(error \"fixture boom\")\n"
                          nil (expand-file-name "init.el" dir))
            (nemacs-load-user-init-files)
            (should (equal early-init-file
                           (expand-file-name "early-init.el" dir)))
            (should-not user-init-file)
            (should nemacs-init-file-loaded)
            (should init-file-had-error)
            (should (consp nemacs-init-file-error))
            (should (equal (car nemacs-init-file-error)
                           (expand-file-name "init.el" dir)))
            (should (string-match-p "fixture boom"
                                    (cdr nemacs-init-file-error))))
        (delete-directory dir t)))))

(provide 'nemacs-loadup-test)

;;; nemacs-loadup-test.el ends here
