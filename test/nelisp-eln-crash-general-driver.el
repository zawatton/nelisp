;;; nelisp-eln-crash-general-driver.el --- S7.7 general crash-containment fixture -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Exercises `nelisp-eln-registration--containment-boundary' (the general
;; crash-containment boundary added for S7.7) through injection points that
;; are independent of the S7.6 pin-end fixture in
;; test/nelisp-eln-same-artifact-smoke.sh's cleanup-failure-driver.el, plus
;; non-local exit types (`throw', `quit') other than `error'.
;;
;; Selected via NELISP_ELN_CRASH_SCENARIO, one scenario per process so no
;; scenario's quarantined state (or disabled boundary) leaks into another:
;;
;;   cleanup-fail-early   -- root cause before any owner exists (mocked
;;                           `nelisp-eln-registration-objects-create-unit'
;;                           errors); cleanup itself fails while restoring
;;                           relocation cells, in
;;                           `nelisp-eln-registration--restore-data-relocations'
;;                           (independent of pin-end).
;;   cleanup-fail-owned   -- root cause after the owner is rooted in
;;                           `nelisp-eln-registration--owners' (mocked
;;                           `nelisp-eln-registration-objects-encode-word'
;;                           errors); cleanup itself fails in
;;                           `nelisp-eln-registration-objects-release-unit'
;;                           (independent of pin-end and of the scenario
;;                           above).
;;   throw-clean-rollback -- root cause is a `throw' (not a `error' signal);
;;                           cleanup succeeds normally, so nothing is
;;                           quarantined and a later attempt is not blocked.
;;   quit-cleanup-fail    -- root cause is a `quit' signal; cleanup fails
;;                           the same way as cleanup-fail-early.
;;   disabled-cleanup-fail-early -- negative control (a): same injection as
;;                           cleanup-fail-early, but with
;;                           `nelisp-eln-registration--boundary-enabled' set
;;                           to nil, so nothing gets recorded.
;;   inconsistent-registry -- negative control (b): no registration attempt
;;                           at all; directly corrupts
;;                           `nelisp-eln-registration--pending-cleanups' and
;;                           `nelisp-eln-registration--owners' and checks
;;                           that the reporter flags it.

(let ((root (getenv "NELISP_ROOT")))
  (when root
    (add-to-list 'load-path (expand-file-name "lisp" root))
    (add-to-list 'load-path (expand-file-name "packages/nl-ffi/src" root))))
(require 'nelisp-eln-metadata)
(require 'nl-ffi)
(require 'nl-ffi-memory)
(require 'nelisp-eln-system-loader)
(require 'nelisp-eln-native-subr)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-registration-objects)

(defun nelisp-eln-crash-general--report-line (label owners pending inconsistencies
                                               reentry-blocked)
  (format "CRASH_GENERAL_%s=owners:%d,pending:%d,inconsistencies:%d,reentry-blocked:%d\n"
          label owners pending inconsistencies (if reentry-blocked 1 0)))

(defun nelisp-eln-crash-general--reentry-blocked-p ()
  "Mirror the exact reentry guard at the top of `nelisp-eln-registration-load'."
  (not (not (or nelisp-eln-registration--active-owner
                nelisp-eln-registration--pending-cleanups))))

(defun nelisp-eln-crash-general--run-cleanup-fail
    (label root-symbol root-effect cleanup-symbol)
  "Inject ROOT-EFFECT (a 0-arg function raising the root cause) at
ROOT-SYMBOL, make CLEANUP-SYMBOL fail during `nelisp-eln-registration--cleanup',
run one registration attempt against the shared fixture, and report the
resulting pending-cleanup and reporter state under LABEL."
  (let ((base-root (symbol-function root-symbol))
        (base-cleanup (symbol-function cleanup-symbol))
        (path (getenv "NELISP_ELN_CRASH_GENERAL_ELN"))
        (attempt-error nil))
    (unwind-protect
        (progn
          (fset root-symbol (lambda (&rest _args) (funcall root-effect)))
          (fset cleanup-symbol
                (lambda (&rest _args)
                  (error "injected %s failure" cleanup-symbol)))
          ;; A `t' handler, not `error': the root cause may be a `quit'
          ;; signal, which `unwind-protect' unwinds through identically to
          ;; an `error' but which `(condition-case ... (error ...))' does
          ;; not catch (quit is not a subtype of error in Emacs's condition
          ;; hierarchy). Catching `t' here only widens what THIS test
          ;; observes; it has no bearing on what
          ;; `nelisp-eln-registration--containment-boundary' itself catches.
          (condition-case err
              (nelisp-eln-registration-load path)
            (t (setq attempt-error err)))
          (unless attempt-error
            (error "expected scenario %s to fail, it succeeded" label))
          (let ((report (nelisp-eln-registration-crash-boundary-report)))
            (princ
             (nelisp-eln-crash-general--report-line
              label (plist-get report :owners) (plist-get report :pending-cleanups)
              (length (plist-get report :inconsistencies))
              (nelisp-eln-crash-general--reentry-blocked-p)))))
      (fset root-symbol base-root)
      (fset cleanup-symbol base-cleanup))))

(defun nelisp-eln-crash-general--scenario-cleanup-fail-early ()
  (nelisp-eln-crash-general--run-cleanup-fail
   "cleanup_fail_early"
   'nelisp-eln-registration-objects-create-unit
   (lambda () (error "injected pre-unit root failure"))
   'nelisp-eln-registration--restore-data-relocations))

(defun nelisp-eln-crash-general--scenario-cleanup-fail-owned ()
  (nelisp-eln-crash-general--run-cleanup-fail
   "cleanup_fail_owned"
   'nelisp-eln-registration-objects-encode-word
   (lambda () (error "injected post-owner root failure"))
   'nelisp-eln-registration-objects-release-unit))

(defun nelisp-eln-crash-general--scenario-throw-clean-rollback ()
  (let ((base-root (symbol-function 'nelisp-eln-registration-objects-create-unit))
        (path (getenv "NELISP_ELN_CRASH_GENERAL_ELN"))
        (caught 'not-thrown))
    (unwind-protect
        (progn
          (fset 'nelisp-eln-registration-objects-create-unit
                (lambda (&rest _args)
                  (throw 'nelisp-eln-crash-general-throw 'thrown)))
          (setq caught
                (catch 'nelisp-eln-crash-general-throw
                  (nelisp-eln-registration-load path)
                  'not-thrown))
          (unless (eq caught 'thrown)
            (error "expected the throw to propagate through registration-load"))
          (let ((report (nelisp-eln-registration-crash-boundary-report)))
            (princ
             (nelisp-eln-crash-general--report-line
              "throw_clean_rollback"
              (plist-get report :owners) (plist-get report :pending-cleanups)
              (length (plist-get report :inconsistencies))
              (nelisp-eln-crash-general--reentry-blocked-p)))))
      (fset 'nelisp-eln-registration-objects-create-unit base-root))))

(defun nelisp-eln-crash-general--scenario-quit-cleanup-fail ()
  (nelisp-eln-crash-general--run-cleanup-fail
   "quit_cleanup_fail"
   'nelisp-eln-registration-objects-create-unit
   (lambda () (signal 'quit nil))
   'nelisp-eln-registration--restore-data-relocations))

(defun nelisp-eln-crash-general--scenario-disabled-cleanup-fail-early ()
  (let ((nelisp-eln-registration--boundary-enabled nil))
    (nelisp-eln-crash-general--run-cleanup-fail
     "disabled_cleanup_fail_early"
     'nelisp-eln-registration-objects-create-unit
     (lambda () (error "injected pre-unit root failure (boundary disabled)"))
     'nelisp-eln-registration--restore-data-relocations)))

(defun nelisp-eln-crash-general--scenario-inconsistent-registry ()
  "Negative control (b): corrupt the registry directly, no registration."
  (let ((fake-owner (make-vector nelisp-eln-registration--owner-size nil))
        (lost-owner (make-vector nelisp-eln-registration--owner-size nil)))
    (aset fake-owner 0 nelisp-eln-registration--owner-marker)
    (aset fake-owner 3 'fake-name)
    (aset lost-owner 0 nelisp-eln-registration--owner-marker)
    (aset lost-owner 3 'lost-name)
    ;; `fake-owner' is rooted normally; `lost-owner' is referenced by a
    ;; failed-cleanup pending entry but never added to --owners, which is
    ;; exactly the inconsistency the reporter must flag.
    (setq nelisp-eln-registration--owners (list fake-owner))
    (setq nelisp-eln-registration--pending-cleanups
          (list (list :phase 'failed-cleanup :owner lost-owner)))
    (let* ((report (nelisp-eln-registration-crash-boundary-report))
           (problems (plist-get report :inconsistencies))
           (found (seq-find (lambda (p) (eq (plist-get p :kind)
                                             'lost-failed-cleanup-owner))
                             problems)))
      (unless found
        (error "reporter did not flag the injected inconsistent registry: %S"
               report))
      (princ (format "CRASH_GENERAL_inconsistent_registry=inconsistencies:%d,found:1\n"
                      (length problems))))))

(let ((scenario (getenv "NELISP_ELN_CRASH_SCENARIO")))
  (cond
   ((equal scenario "cleanup-fail-early")
    (nelisp-eln-crash-general--scenario-cleanup-fail-early))
   ((equal scenario "cleanup-fail-owned")
    (nelisp-eln-crash-general--scenario-cleanup-fail-owned))
   ((equal scenario "throw-clean-rollback")
    (nelisp-eln-crash-general--scenario-throw-clean-rollback))
   ((equal scenario "quit-cleanup-fail")
    (nelisp-eln-crash-general--scenario-quit-cleanup-fail))
   ((equal scenario "disabled-cleanup-fail-early")
    (nelisp-eln-crash-general--scenario-disabled-cleanup-fail-early))
   ((equal scenario "inconsistent-registry")
    (nelisp-eln-crash-general--scenario-inconsistent-registry))
   (t (error "unknown NELISP_ELN_CRASH_SCENARIO: %S" scenario))))

;;; nelisp-eln-crash-general-driver.el ends here
