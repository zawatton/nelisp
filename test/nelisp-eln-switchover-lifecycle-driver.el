;;; nelisp-eln-switchover-lifecycle-driver.el --- S7.7 deferred release + quarantine -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Run by test/nelisp-eln-switchover-smoke.sh on the NeLisp binary,
;; concurrently with test/nelisp-eln-switchover-driver.el, on the pinned
;; genuine GNU identity artifact:
;;
;;   deferred   -- a still-referenced routed function defers the release:
;;                 the module stays mapped and callable, nothing is freed
;;                 under it; dropping the reference lets the next
;;                 finalize release it
;;   quarantine -- a registration whose cleanup fails quarantines the
;;                 registry; that item and every later one fall back,
;;                 recorded, and the later ones are never attempted
;;
;; Every phase is its own top-level form so finalize runs from a frame
;; that no longer holds the native callable.

(load (expand-file-name "nelisp-eln-switchover-common.el"
                        (getenv "NELISP_S77_TEST_DIR"))
      nil t)

(unless (and s77-gnu-identity (file-readable-p s77-gnu-identity))
  (s77-fail "env" s77-gnu-identity))

(defvar s77-previous '((nelisp-gnu-identity)))
(defconst s77-gnu-form '(defun nelisp-gnu-identity (value) value))
(defvar s77-maps-0 (s77-private-mappings))

;;; Phase: deferred release while a routed function is still referenced.

(defvar s77-route-1
  (nelisp-eln-switchover-load 'nelisp-gnu-identity s77-gnu-identity
                              (list :form s77-gnu-form)))
(defvar s77-held (symbol-function 'nelisp-gnu-identity))
(defvar s77-module (plist-get (nelisp-eln-switchover-entry 'nelisp-gnu-identity)
                              :module-id))
(s77-check "LIFECYCLE_ROUTED"
           (and s77-route-1
                (eq (plist-get (s77-route 'nelisp-gnu-identity) :route) 'eln)
                (equal (funcall s77-held "held") "held")
                (= (s77-private-mappings) (1+ s77-maps-0)))
           (s77-route 'nelisp-gnu-identity))
(nelisp-eln-switchover-unload 'nelisp-gnu-identity)
(defvar s77-fin-1 (nelisp-eln-switchover-finalize-unloads))
(s77-check "DEFERRED_WHILE_REFERENCED"
           (and (equal s77-fin-1 '(:released 0 :deferred 1 :failed 0))
                (not (fboundp 'nelisp-gnu-identity))
                (= (s77-report :owners) 1) (= (s77-report :open-handles) 1)
                (>= (nelisp--native-subr-live-count s77-module) 1)
                (= (funcall s77-held 9) 9)
                (= (s77-private-mappings) (1+ s77-maps-0)))
           s77-fin-1 (s77-report-all))
(setq s77-held nil)
(defvar s77-fin-2 (nelisp-eln-switchover-finalize-unloads))
(s77-check "DEFERRED_RELEASED"
           (and (equal s77-fin-2 '(:released 1 :deferred 0 :failed 0))
                (= (s77-report :owners) 0) (= (s77-report :open-handles) 0)
                (= (s77-report :live-units) 0)
                (= (nelisp--native-subr-live-count s77-module) 0)
                (= (s77-private-mappings) s77-maps-0))
           s77-fin-2 (s77-report-all))

;;; Phase: quarantine propagation.

(defvar s77-orig-create (symbol-function
                         'nelisp-eln-registration-objects-create-unit))
(defvar s77-orig-restore (symbol-function
                          'nelisp-eln-registration--restore-data-relocations))
(defun s77-gnu-spec ()
  (list 'nelisp-gnu-identity s77-gnu-identity (list :form s77-gnu-form)
        (lambda () (eval s77-gnu-form t))))
;; Root failure after open + a failing relocation restore during cleanup:
;; the registration layer must quarantine instead of guessing.
(fset 'nelisp-eln-registration-objects-create-unit
      (lambda (&rest _) (error "S77 injected root failure")))
(fset 'nelisp-eln-registration--restore-data-relocations
      (lambda (&rest _) (error "S77 injected cleanup failure")))
(defvar s77-q1 (nelisp-eln-switchover-load-batch (list (s77-gnu-spec))))
(fset 'nelisp-eln-registration-objects-create-unit s77-orig-create)
(fset 'nelisp-eln-registration--restore-data-relocations s77-orig-restore)
(defvar s77-q1-reason (plist-get (s77-route 'nelisp-gnu-identity) :reason))
(fmakunbound 'nelisp-gnu-identity)
(defvar s77-q2 (nelisp-eln-switchover-load-batch
                (list (s77-gnu-spec) (s77-gnu-spec))))
(s77-check "QUARANTINE"
           (and (equal s77-q1 '((nelisp-gnu-identity . fallback)))
                (eq s77-q1-reason 'registration-rejected-quarantined)
                (equal s77-q2 '((nelisp-gnu-identity . fallback)
                                (nelisp-gnu-identity . fallback)))
                (eq (plist-get (s77-route 'nelisp-gnu-identity) :reason)
                    'registry-quarantined)
                (eq (plist-get (s77-route 'nelisp-gnu-identity)
                               :fallback-installed)
                    t)
                (equal (nelisp-gnu-identity '(1 . 2)) '(1 . 2))
                (= (s77-report :pending-cleanups) 1)
                (null (s77-report :inconsistencies)))
           s77-q1 s77-q1-reason s77-q2 (s77-report-all))

(princ (format "NELISP-ELN-SWITCHOVER-LIFECYCLE-PASS decisions=%d\n"
               (length nelisp-eln-switchover-log)))
(kill-emacs 0)

;;; nelisp-eln-switchover-lifecycle-driver.el ends here
