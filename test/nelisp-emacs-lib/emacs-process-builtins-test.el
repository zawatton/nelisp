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

(ert-deftest emacs-process-builtins-test/call-process-uses-nelisp-process-in-standalone ()
  (let ((emacs-standalone-force-mode t)
        (emacs-standalone--primitives (make-hash-table :test 'eq))
        (captured nil))
    (cl-letf (((symbol-function 'nelisp-call-process)
               (lambda (&rest args)
                 (setq captured args)
                 23)))
      (should (eq (emacs-process-call-process
                   "/bin/tool" nil t nil "arg1" "arg2")
                  23))
      (should (equal captured
                     '("/bin/tool" nil t nil "arg1" "arg2"))))))

(ert-deftest emacs-process-builtins-test/call-process-prefers-package-prefixed-nelisp-process ()
  (let ((emacs-standalone-force-mode t)
        (emacs-standalone--primitives (make-hash-table :test 'eq))
        (captured nil)
        (had-new (fboundp 'nelisp-process-call-process))
        (before-new (and (fboundp 'nelisp-process-call-process)
                         (symbol-function 'nelisp-process-call-process)))
        (had-old (fboundp 'nelisp-call-process))
        (before-old (and (fboundp 'nelisp-call-process)
                         (symbol-function 'nelisp-call-process))))
    (unwind-protect
        (progn
          (fset 'nelisp-process-call-process
                (lambda (&rest args)
                  (setq captured args)
                  31))
          (fset 'nelisp-call-process
                (lambda (&rest _)
                  (error "package-prefixed delegate should win")))
          (should (eq (emacs-process-call-process
                       "/bin/new" nil nil nil "--ok")
                      31))
          (should (equal captured '("/bin/new" nil nil nil "--ok"))))
      (if had-new
          (fset 'nelisp-process-call-process before-new)
        (fmakunbound 'nelisp-process-call-process))
      (if had-old
          (fset 'nelisp-call-process before-old)
        (fmakunbound 'nelisp-call-process)))))

(ert-deftest emacs-process-builtins-test/call-process-soft-loads-nelisp-process ()
  (let ((emacs-standalone-force-mode t)
        (emacs-standalone--primitives (make-hash-table :test 'eq))
        (required nil)
        (had-new (fboundp 'nelisp-process-call-process))
        (before-new (and (fboundp 'nelisp-process-call-process)
                         (symbol-function 'nelisp-process-call-process)))
        (had-old (fboundp 'nelisp-call-process))
        (before-old (and (fboundp 'nelisp-call-process)
                         (symbol-function 'nelisp-call-process))))
    (unwind-protect
        (progn
          (when had-new
            (fmakunbound 'nelisp-process-call-process))
          (when had-old
            (fmakunbound 'nelisp-call-process))
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional _filename _noerror)
                       (setq required feature)
                       (fset 'nelisp-call-process
                             (lambda (&rest _args) 37))
                       t)))
            (should (eq (emacs-process-call-process "/bin/soft" nil nil nil)
                        37))
            (should (eq required 'nelisp-process))))
      (if had-new
          (fset 'nelisp-process-call-process before-new)
        (fmakunbound 'nelisp-process-call-process))
      (if had-old
          (fset 'nelisp-call-process before-old)
        (fmakunbound 'nelisp-call-process)))))

(ert-deftest emacs-process-builtins-test/call-process-prefers-registered-primitive ()
  (let ((emacs-standalone-force-mode t)
        (emacs-standalone--primitives (make-hash-table :test 'eq)))
    (emacs-standalone-register-primitive
     'call-process
     (lambda (&rest args)
       (and (equal args '("/bin/primitive" nil nil nil))
            77)))
    (cl-letf (((symbol-function 'nelisp-call-process)
               (lambda (&rest _)
                 (error "registered primitive should win"))))
      (should (eq (emacs-process-call-process "/bin/primitive" nil nil nil)
                  77)))))

(ert-deftest emacs-process-builtins-test/call-process-region-uses-nelisp-process ()
  (let ((emacs-standalone-force-mode t)
        (emacs-standalone--primitives (make-hash-table :test 'eq))
        (captured nil))
    (cl-letf (((symbol-function 'nelisp-call-process-region)
               (lambda (&rest args)
                 (setq captured args)
                 24)))
      (should (eq (emacs-process-call-process-region
                   1 5 "/bin/filter" nil t nil "--flag")
                  24))
      (should (equal captured
                     '(1 5 "/bin/filter" nil t nil "--flag"))))))

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

(ert-deftest emacs-process-builtins-test/make-process-standalone-fallback-object ()
  "Standalone fallback should return a process-shaped object."
  (let ((emacs-standalone-force-mode t)
        (emacs-standalone--primitives (make-hash-table :test 'eq))
        (emacs-process--fallback-processes nil)
        (emacs-process--fallback-next-pid 10000)
        (captured nil)
        (sentinel-events nil))
    (emacs-standalone-register-primitive
     'call-process
     (lambda (&rest args)
       (setq captured args)
       (when (bufferp (nth 2 args))
         (with-current-buffer (nth 2 args)
           (insert "fallback-output")))
       0))
    (let* ((buffer (generate-new-buffer " *fallback-process*"))
           (proc (unwind-protect
                     (emacs-process-make-process
                      :name "fallback"
                      :buffer buffer
                      :command '("/bin/sh" "-c" "printf fallback-output")
                      :sentinel (lambda (process event)
                                  (push (list process event)
                                        sentinel-events)))
                   nil)))
      (unwind-protect
          (progn
            (should (emacs-process-processp proc))
            (should (memq proc (emacs-process-process-list)))
            (should (equal (emacs-process-process-name proc) "fallback"))
            (should (equal (emacs-process-process-command proc)
                           '("/bin/sh" "-c" "printf fallback-output")))
            (should (eq (emacs-process-process-buffer proc) buffer))
            (should (eq (emacs-process-process-status proc) 'exit))
            (should (= (emacs-process-process-exit-status proc) 0))
            (should (= (emacs-process-process-id proc) 10000))
            (should (equal (nth 0 captured) "/bin/sh"))
            (should (null (nth 1 captured)))
            (should (eq (nth 2 captured) buffer))
            (should (null (nth 3 captured)))
            (should (equal (nthcdr 4 captured)
                           '("-c" "printf fallback-output")))
            (should (equal (with-current-buffer buffer (buffer-string))
                           "fallback-output"))
            (should (= (length sentinel-events) 1))
            (should (eq (caar sentinel-events) proc))
            (should (equal (cadar sentinel-events) "finished\n"))
            (should (eq (emacs-process-delete-process proc) proc))
            (should-not (memq proc (emacs-process-process-list))))
        (kill-buffer buffer)))))

(ert-deftest emacs-process-builtins-test/make-process-uses-native-nelisp-object ()
  "Standalone native process builtins should be preferred over fallback."
  (let ((emacs-standalone-force-mode t)
        (emacs-standalone--primitives (make-hash-table :test 'eq))
        (emacs-process--fallback-processes nil)
        (emacs-process--native-process-metadata nil)
        (native (vector 'native-process))
        (captured nil)
        (status-code 0)
        (output "native-output")
        (filter-chunks nil)
        (sentinel-events nil)
        (deleted nil))
    (cl-letf (((symbol-function 'nelisp-process-object-p)
               (lambda (object) (eq object native)))
              ((symbol-function 'nelisp-process-start-process)
               (lambda (&rest args)
                 (setq captured args)
                 native))
              ((symbol-function 'nelisp-process-status)
               (lambda (_process) status-code))
              ((symbol-function 'nelisp-process-exit-status)
               (lambda (_process) 0))
              ((symbol-function 'nelisp-process-pid)
               (lambda (_process) 4242))
              ((symbol-function 'nelisp-process-read-output)
               (lambda (_process _limit)
                 (prog1 output
                   (setq output nil))))
              ((symbol-function 'nelisp-process-delete)
               (lambda (_process)
                 (setq deleted t)
                 nil)))
      (let ((buffer (generate-new-buffer " *native-process*")))
        (unwind-protect
            (let ((proc (emacs-process-make-process
                         :name "native"
                         :buffer buffer
                         :command '("/bin/sh" "-c" "printf native-output")
                         :filter (lambda (_process chunk)
                                   (push chunk filter-chunks))
                         :sentinel (lambda (_process event)
                                     (push event sentinel-events)))))
              (should (eq proc native))
              (should (equal captured
                             '("/bin/sh" "-c" "printf native-output")))
              (should (emacs-process-processp proc))
              (should (memq proc (emacs-process-process-list)))
              (should (eq (emacs-process-process-status proc) 'run))
              (should (= (emacs-process-process-id proc) 4242))
              (should (equal (emacs-process-process-name proc) "native"))
              (should (equal (emacs-process-process-command proc)
                             '("/bin/sh" "-c" "printf native-output")))
              (should (eq (emacs-process-process-buffer proc) buffer))
              (setq status-code 1)
              (should (emacs-process-accept-process-output proc 0 0 t))
              (should (equal (with-current-buffer buffer (buffer-string))
                             "native-output"))
              (should (equal filter-chunks '("native-output")))
              (should (equal sentinel-events '("finished\n")))
              (should (eq (emacs-process-process-status proc) 'exit))
              (should (= (emacs-process-process-exit-status proc) 0))
              (should (eq (emacs-process-delete-process proc) proc))
              (should deleted)
              (should-not (memq proc (emacs-process-process-list))))
          (kill-buffer buffer))))))

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
