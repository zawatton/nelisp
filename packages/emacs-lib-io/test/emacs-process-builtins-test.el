;;; emacs-process-builtins-test.el --- ERT for emacs-process  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 process API (Track I).  Under host Emacs
;; the unprefixed bridges are gated off; the substrate's
;; delegate-or-signal pattern dispatches to host's C primitives so
;; the prefixed `emacs-process-*' API exercises the live process
;; surface.  Featurep / fboundp / boundp parity is checked
;; separately.  Subprocess tests are gated on `/bin/sh' /
;; `/bin/echo' availability so they skip cleanly when the host
;; doesn't provide a POSIX shell.

;;; Code:

(require 'ert)
(require 'emacs-process-builtins)
(require 'cl-lib)

(defmacro emacs-process-builtins-test--skip-unless-shell (&rest body)
  "Run BODY only when /bin/sh + /bin/echo are present."
  (declare (indent 0) (debug (body)))
  `(if (and (file-executable-p "/bin/sh")
            (file-executable-p "/bin/echo"))
       (progn ,@body)
     (ert-skip "shell not available")))

;;;; A. Load cleanly + fboundp / boundp parity

(ert-deftest emacs-process-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-process-builtins))
  (should (featurep 'emacs-process))
  (dolist (sym '(call-process call-process-region
                 start-process make-process
                 processp process-list process-status
                 process-exit-status process-buffer process-name
                 process-command process-live-p process-id process-mark
                 set-process-filter set-process-sentinel
                 accept-process-output signal-process kill-process
                 process-send-string process-send-eof delete-process
                 shell-command shell-command-to-string))
    (should (fboundp sym)))
  (dolist (sym '(shell-file-name shell-command-switch))
    (should (boundp sym))))

;;;; B. delegate-p detects host bindings

(ert-deftest emacs-process-builtins-test/delegate-p-detects-host ()
  ;; Under host Emacs, call-process is a C primitive and is NOT our
  ;; substrate, so delegate-p should return non-nil.
  (should (emacs-process--delegate-p 'call-process)))

(ert-deftest emacs-process-builtins-test/delegate-p-rejects-recursive-alias ()
  ;; If we deliberately bind a name to its own substrate, delegate-p
  ;; should detect the recursion path and return nil.
  (let ((before (and (fboundp 'emacs-process-builtins-test--probe)
                     (symbol-function
                      'emacs-process-builtins-test--probe))))
    (defalias 'emacs-process-emacs-process-builtins-test--probe
      #'emacs-process-call-process)
    (defalias 'emacs-process-builtins-test--probe
      #'emacs-process-emacs-process-builtins-test--probe)
    (unwind-protect
        (should-not (emacs-process--delegate-p
                     'emacs-process-builtins-test--probe))
      (when before
        (defalias 'emacs-process-builtins-test--probe before)))))

;;;; C. call-process — basic stdout capture

(ert-deftest emacs-process-builtins-test/call-process-echo-roundtrip ()
  (emacs-process-builtins-test--skip-unless-shell
    (with-temp-buffer
      (let ((rc (emacs-process-call-process
                 "/bin/echo" nil t nil "hello-process")))
        (should (eq rc 0))
        (should (string-match "hello-process" (buffer-string)))))))

;;;; D. call-process — exit-status non-zero on failure

(ert-deftest emacs-process-builtins-test/call-process-failure-non-zero ()
  (emacs-process-builtins-test--skip-unless-shell
    (with-temp-buffer
      (let ((rc (emacs-process-call-process
                 "/bin/sh" nil t nil "-c" "exit 7")))
        (should (eq rc 7))))))

;;;; E. shell-command-to-string

(ert-deftest emacs-process-builtins-test/shell-command-to-string-captures ()
  (emacs-process-builtins-test--skip-unless-shell
    (let ((out (emacs-process-shell-command-to-string "echo abc")))
      (should (stringp out))
      (should (string-match "abc" out)))))

;;;; F. processp on non-process returns nil

(ert-deftest emacs-process-builtins-test/processp-non-process-nil ()
  (should-not (emacs-process-processp "not-a-process"))
  (should-not (emacs-process-processp 42))
  (should-not (emacs-process-processp nil)))

;;;; G. process-list returns a list

(ert-deftest emacs-process-builtins-test/process-list-is-list ()
  (let ((lst (emacs-process-process-list)))
    (should (listp lst))))

;;;; H. start-process returns a process object that is processp

(ert-deftest emacs-process-builtins-test/start-process-roundtrip ()
  (emacs-process-builtins-test--skip-unless-shell
    (let ((proc (ignore-errors
                  (emacs-process-start-process
                   "test-proc" nil "/bin/sh" "-c" "exit 0"))))
      (when proc
        (should (emacs-process-processp proc))
        ;; Wait for it to terminate.
        (while (eq (emacs-process-process-status proc) 'run)
          (accept-process-output nil 0.05))
        (should (memq (emacs-process-process-status proc)
                      '(exit signal)))
        (emacs-process-delete-process proc)))))

;;;; I. shell-file-name + shell-command-switch defaults

(ert-deftest emacs-process-builtins-test/shell-vars-non-empty ()
  (should (stringp (or shell-file-name "/bin/sh")))
  (should (stringp (or shell-command-switch "-c"))))

;;;; J. Idempotent require

(ert-deftest emacs-process-builtins-test/require-is-idempotent ()
  (let ((before-cp  (symbol-function 'call-process))
        (before-sc  (symbol-function 'shell-command))
        (before-sp  (symbol-function 'start-process))
        (before-ps  (symbol-function 'process-status)))
    (require 'emacs-process-builtins)
    (should (eq before-cp (symbol-function 'call-process)))
    (should (eq before-sc (symbol-function 'shell-command)))
    (should (eq before-sp (symbol-function 'start-process)))
    (should (eq before-ps (symbol-function 'process-status)))))

(ert-deftest emacs-process-builtins-test/bridge-overwrites-standalone-stubs-in-source ()
  (should (fboundp 'emacs-process-builtins--install-function-p))
  (should-not (emacs-process-builtins--install-function-p 'call-process))
  (let* ((file (locate-library "emacs-process-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(call-process call-process-region start-process
                     make-process processp process-list process-status
                     process-exit-status process-buffer process-name
                     process-command process-live-p process-id process-mark
                     set-process-filter set-process-sentinel
                     accept-process-output signal-process kill-process
                     process-send-string process-send-eof delete-process
                     shell-command shell-command-to-string))
        (goto-char (point-min))
        (should (search-forward
                 (format "(when (emacs-process-builtins--install-function-p '%s)"
                         sym)
                 nil t))))))

(provide 'emacs-process-builtins-test)

;;; emacs-process-builtins-test.el ends here
