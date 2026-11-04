;;; emacs-standalone-test.el --- ERT for standalone scaffold  -*- lexical-binding: t; -*-

;;; Commentary:

;; Track M ERT.  Verifies the NeLisp standalone dispatch scaffold:
;; mode detection (= force-override + auto-detect), the primitive
;; registry, the dispatch core, the two-mode helper, the bootstrap
;; lifecycle, and integration with `emacs-process'-style delegation
;; (= when standalone primitives are registered, the substrate
;; dispatcher consults them in place of the
;; `emacs-process-not-implemented' signal).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-standalone)

;;;; --- fixtures ------------------------------------------------------

(defmacro emacs-standalone-test--fresh (&rest body)
  "Run BODY with a clean dispatcher / registry."
  (declare (indent 0) (debug (body)))
  `(let ((emacs-standalone-force-mode 'auto)
         (emacs-standalone--initialized nil)
         (emacs-standalone--detected nil))
     (let ((saved (let (out)
                    (maphash (lambda (k v) (push (cons k v) out))
                             emacs-standalone--primitives)
                    out)))
       (unwind-protect
           (progn
             (clrhash emacs-standalone--primitives)
             ,@body)
         (clrhash emacs-standalone--primitives)
         (dolist (cell saved)
           (puthash (car cell) (cdr cell) emacs-standalone--primitives))))))

;;;; A. Load + parity

(ert-deftest emacs-standalone-test/feature-loaded ()
  (should (featurep 'emacs-standalone))
  (dolist (sym '(emacs-standalone-mode-p emacs-standalone-active-p
                 emacs-standalone-init emacs-standalone-uninit
                 emacs-standalone-register-primitive
                 emacs-standalone-call-primitive
                 emacs-standalone-dispatch
                 emacs-standalone-status))
    (should (fboundp sym))))

(ert-deftest emacs-standalone-test/version-constant ()
  (should (integerp emacs-standalone-version))
  (should (>= emacs-standalone-version 1)))

;;;; B. mode detection — force-override

(ert-deftest emacs-standalone-test/force-mode-t ()
  (emacs-standalone-test--fresh
    (let ((emacs-standalone-force-mode t))
      (should (emacs-standalone-mode-p)))))

(ert-deftest emacs-standalone-test/force-mode-nil ()
  (emacs-standalone-test--fresh
    (let ((emacs-standalone-force-mode nil))
      (should-not (emacs-standalone-mode-p)))))

(ert-deftest emacs-standalone-test/force-mode-auto-host ()
  (emacs-standalone-test--fresh
    ;; Under host Emacs, `make-process' is a subr → mode-p returns nil.
    (let ((emacs-standalone-force-mode 'auto))
      (should-not (emacs-standalone-mode-p)))))

;;;; C. active-p requires init

(ert-deftest emacs-standalone-test/active-requires-init ()
  (emacs-standalone-test--fresh
    (let ((emacs-standalone-force-mode t))
      ;; Force standalone but skip init → active-p should be nil.
      (should-not (emacs-standalone-active-p))
      (emacs-standalone-init)
      (should (emacs-standalone-active-p)))))

(ert-deftest emacs-standalone-test/init-idempotent ()
  (emacs-standalone-test--fresh
    (should (emacs-standalone-init))
    (should (emacs-standalone-init))))

(ert-deftest emacs-standalone-test/uninit-clears ()
  (emacs-standalone-test--fresh
    (emacs-standalone-init)
    (let ((emacs-standalone-force-mode t))
      (should (emacs-standalone-active-p))
      (emacs-standalone-uninit)
      (should-not (emacs-standalone-active-p)))))

;;;; D. primitive registry

(ert-deftest emacs-standalone-test/register-and-lookup ()
  (emacs-standalone-test--fresh
    (should-not (emacs-standalone-has-primitive-p 'foo))
    (emacs-standalone-register-primitive 'foo (lambda (x) (* 2 x)))
    (should (emacs-standalone-has-primitive-p 'foo))
    (should (= 6 (emacs-standalone-call-primitive 'foo (list 3))))))

(ert-deftest emacs-standalone-test/register-rejects-bad-args ()
  (emacs-standalone-test--fresh
    (should-error (emacs-standalone-register-primitive "foo" #'identity)
                  :type 'wrong-type-argument)
    (should-error (emacs-standalone-register-primitive 'foo "not-a-fn")
                  :type 'wrong-type-argument)))

(ert-deftest emacs-standalone-test/unregister-and-clear ()
  (emacs-standalone-test--fresh
    (emacs-standalone-register-primitive 'a #'identity)
    (emacs-standalone-register-primitive 'b #'identity)
    (should (emacs-standalone-has-primitive-p 'a))
    (should (emacs-standalone-unregister-primitive 'a))
    (should-not (emacs-standalone-has-primitive-p 'a))
    (should (emacs-standalone-has-primitive-p 'b))
    (emacs-standalone-clear-registry)
    (should-not (emacs-standalone-has-primitive-p 'b))))

(ert-deftest emacs-standalone-test/registered-list ()
  (emacs-standalone-test--fresh
    (should (null (emacs-standalone-registered-primitives)))
    (emacs-standalone-register-primitive 'one #'ignore)
    (emacs-standalone-register-primitive 'two #'ignore)
    (let ((names (emacs-standalone-registered-primitives)))
      (should (memq 'one names))
      (should (memq 'two names))
      (should (= 2 (length names))))))

;;;; E. dispatch core fallback semantics

(ert-deftest emacs-standalone-test/call-primitive-dispatches ()
  (emacs-standalone-test--fresh
    (emacs-standalone-register-primitive 'sq (lambda (n) (* n n)))
    (should (= 25 (emacs-standalone-call-primitive 'sq '(5))))))

(ert-deftest emacs-standalone-test/call-primitive-fallback-fn ()
  (emacs-standalone-test--fresh
    (let ((calls 0))
      (let ((result (emacs-standalone-call-primitive
                     'never-registered '(1 2)
                     (lambda (a b) (cl-incf calls) (+ a b)))))
        (should (= 1 calls))
        (should (= 3 result))))))

(ert-deftest emacs-standalone-test/call-primitive-no-fallback-signals ()
  (emacs-standalone-test--fresh
    (should-error (emacs-standalone-call-primitive 'absent '(x))
                  :type 'emacs-standalone-no-primitive)))

;;;; F. two-mode dispatcher helper

(ert-deftest emacs-standalone-test/dispatch-host-mode-uses-host ()
  (emacs-standalone-test--fresh
    (let ((emacs-standalone-force-mode nil)
          (host-called nil))
      (emacs-standalone-dispatch 'whatever '(1 2)
                                 (lambda (a b) (setq host-called t) (+ a b))
                                 nil)
      (should host-called))))

(ert-deftest emacs-standalone-test/dispatch-standalone-uses-primitive ()
  (emacs-standalone-test--fresh
    (let ((emacs-standalone-force-mode t)
          (primitive-called nil))
      (emacs-standalone-register-primitive
       'foo (lambda (n) (setq primitive-called t) (* n 10)))
      (let ((result (emacs-standalone-dispatch
                     'foo '(7)
                     (lambda (_) (error "host should not run"))
                     nil)))
        (should primitive-called)
        (should (= 70 result))))))

(ert-deftest emacs-standalone-test/dispatch-falls-back-to-signal-fn ()
  (emacs-standalone-test--fresh
    (let ((emacs-standalone-force-mode t)
          (sig-called nil))
      (emacs-standalone-dispatch
       'no-impl '(x)
       nil
       (lambda () (setq sig-called t) 'signaled))
      (should sig-called))))

(ert-deftest emacs-standalone-test/dispatch-default-signal ()
  (emacs-standalone-test--fresh
    (let ((emacs-standalone-force-mode t))
      (should-error
       (emacs-standalone-dispatch 'no-impl '(x) nil nil)
       :type 'emacs-standalone-no-primitive))))

;;;; G. integration — emacs-process delegate

(ert-deftest emacs-standalone-test/process-delegate-uses-host-by-default ()
  (require 'emacs-process)
  (emacs-standalone-test--fresh
    (let ((emacs-standalone-force-mode nil))
      ;; Under host Emacs, `make-process' would normally run; here we
      ;; just verify the substrate routes to the host C primitive
      ;; rather than raising not-implemented.
      ;; Use `processp' as the witness — it's safe to call with nil.
      (should-not (emacs-process-processp nil)))))

(ert-deftest emacs-standalone-test/process-delegate-uses-standalone-primitive ()
  (require 'emacs-process)
  (emacs-standalone-test--fresh
    (let ((emacs-standalone-force-mode t))
      ;; No primitive registered → not-implemented.
      (should-error (emacs-process-process-status 'fake-process)
                    :type 'emacs-process-not-implemented)
      ;; Register a primitive → substrate dispatches to it.
      (emacs-standalone-register-primitive
       'process-status (lambda (_p) 'standalone-stopped))
      (should (eq 'standalone-stopped
                  (emacs-process-process-status 'fake-process))))))

;;;; H. status snapshot

(ert-deftest emacs-standalone-test/status-keys-present ()
  (emacs-standalone-test--fresh
    (let ((s (emacs-standalone-status)))
      (dolist (k '(:version :initialized :force-mode :detected
                   :mode-p :primitives :primitive-count))
        (should (plist-member s k))))))

(ert-deftest emacs-standalone-test/status-counts-registered ()
  (emacs-standalone-test--fresh
    (emacs-standalone-register-primitive 'one #'ignore)
    (emacs-standalone-register-primitive 'two #'ignore)
    (let ((s (emacs-standalone-status)))
      (should (= 2 (plist-get s :primitive-count))))))

(provide 'emacs-standalone-test)

;;; emacs-standalone-test.el ends here
