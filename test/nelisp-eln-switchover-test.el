;;; nelisp-eln-switchover-test.el --- host tests for the Doc 208 router -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Host-only (no NeLisp binary) coverage of the routing decisions in
;; lisp/nelisp-eln-switchover.el and of the `.neln' loader hook in
;; lisp/nelisp-artifact.el.  Every path that needs the real dynamic
;; loader is exercised end to end by test/nelisp-eln-switchover-smoke.sh;
;; here `nelisp-eln-registration-load' is replaced with `cl-letf' fakes.

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-switchover)
(require 'nelisp-artifact)

(defmacro nelisp-eln-switchover-test--fresh (&rest body)
  "Run BODY with empty router and registration state."
  (declare (indent 0))
  `(let ((nelisp-eln-switchover-log nil)
         (nelisp-eln-switchover--entries nil)
         (nelisp-eln-switchover--pending-unloads nil)
         (nelisp-eln-registration--pending-cleanups nil)
         (nelisp-eln-registration--active-owner nil)
         (nelisp-eln-registration--owners nil))
     ,@body))

(defun nelisp-eln-switchover-test--file (content)
  (let ((path (make-temp-file "nelisp-s77-" nil ".eln")))
    (let ((coding-system-for-write 'no-conversion))
      (write-region content nil path nil 'silent))
    path))

(defun nelisp-eln-switchover-test--sha (path)
  (nelisp-eln-system-loader--file-sha256
   (nelisp-eln-system-loader--read-file path)))

(ert-deftest nelisp-eln-switchover/coverage-classification ()
  (let* ((path (nelisp-eln-switchover-test--file "not-really-an-eln"))
         (bytes (nelisp-eln-system-loader--read-file path))
         (sha (nelisp-eln-switchover-test--sha path)))
    (unwind-protect
        (progn
          (should (eq (cadr (nelisp-eln-switchover--coverage 'f bytes nil))
                      'outside-proven-coverage))
          (should (eq (cadr (nelisp-eln-switchover--coverage
                             'f bytes (list :sha256 "00" :kind 'constant)))
                      'artifact-hash-mismatch))
          (should (eq (cadr (nelisp-eln-switchover--coverage
                             'f bytes (list :sha256 sha :kind 'tail-import)))
                      'emitter-kind-not-proven))
          (let ((ok (nelisp-eln-switchover--coverage
                     'f bytes (list :sha256 sha :kind 'identity))))
            (should (car ok))
            (should (eq (cadr ok) 'self-emitted-migrated))
            (should (eq (plist-get (cddr ok) :publish) 'global)))
          ;; A pinned genuine artifact is admitted only for its own name.
          (let ((nelisp-eln-switchover-proven-genuine-artifacts
                 (list (list sha :name 'g :publish 'isolated
                             :evidence '("S0")))))
            (should (eq (cadr (nelisp-eln-switchover--coverage 'g bytes nil))
                        'genuine-pinned))
            (should (eq (cadr (nelisp-eln-switchover--coverage 'h bytes nil))
                        'genuine-name-mismatch))))
      (delete-file path))))

(ert-deftest nelisp-eln-switchover/fallbacks-are-recorded-not-silent ()
  (nelisp-eln-switchover-test--fresh
    (let ((installed 0)
          (path (nelisp-eln-switchover-test--file "x")))
      (unwind-protect
          (cl-letf (((symbol-function 'nelisp-eln-registration-load)
                     (lambda (&rest _) (error "must not be attempted"))))
            ;; Missing artifact, outside coverage, quarantined registry: never
            ;; attempted, each recorded once, fallback installed each time.
            (should-not (nelisp-eln-switchover-load
                         'a "/nonexistent.eln" nil (lambda () (cl-incf installed))))
            (should-not (nelisp-eln-switchover-load
                         'b path nil (lambda () (cl-incf installed))))
            (let ((nelisp-eln-registration--pending-cleanups '(quarantined)))
              (should-not (nelisp-eln-switchover-load
                           'c path (list :sha256 (nelisp-eln-switchover-test--sha path)
                                         :kind 'constant)
                           (lambda () (cl-incf installed)))))
            (should (= installed 3))
            (should (equal (mapcar (lambda (r) (list (plist-get r :symbol)
                                                     (plist-get r :route)
                                                     (plist-get r :reason)
                                                     (plist-get r :fallback-installed)))
                                   (reverse nelisp-eln-switchover-log))
                           '((a fallback missing-artifact t)
                             (b fallback outside-proven-coverage t)
                             (c fallback registry-quarantined t)))))
        (delete-file path)))))

(ert-deftest nelisp-eln-switchover/loader-rejection-falls-back ()
  (nelisp-eln-switchover-test--fresh
    (let* ((path (nelisp-eln-switchover-test--file "y"))
           (coverage (list :sha256 (nelisp-eln-switchover-test--sha path)
                           :kind 'constant))
           (calls 0) (installed nil))
      (unwind-protect
          (cl-letf (((symbol-function 'nelisp-eln-registration-load)
                     (lambda (p)
                       (cl-incf calls)
                       (should (equal p path))
                       ;; The router always registers into an isolated namespace.
                       (should (nelisp-eln-registration--namespace-p
                                nelisp-eln-registration-isolated-namespace))
                       (signal 'nelisp-eln-system-loader-error
                               (list 'invalid-elf-profile p)))))
            (should-not (nelisp-eln-switchover-load
                         'd path coverage (lambda () (setq installed t))))
            (should (= calls 1))
            (should installed)
            (let ((rec (car nelisp-eln-switchover-log)))
              (should (eq (plist-get rec :reason) 'registration-rejected))
              (should (eq (car (plist-get rec :detail))
                          'nelisp-eln-system-loader-error)))
            (should-not (nelisp-eln-switchover-entry 'd)))
        (delete-file path)))))

(ert-deftest nelisp-eln-switchover/batch-continues-after-router-error ()
  (nelisp-eln-switchover-test--fresh
    (let ((real (symbol-function 'nelisp-eln-switchover-load)))
      (cl-letf (((symbol-function 'nelisp-eln-switchover-load)
                 (lambda (symbol &rest args)
                   (if (eq symbol 'boom)
                       (error "router bug")
                     (apply real symbol args)))))
        (should (equal (nelisp-eln-switchover-load-batch
                        '((p "/nonexistent-1.eln" nil nil)
                          (boom "/nonexistent-2.eln" nil nil)
                          (q "/nonexistent-3.eln" nil nil)))
                       '((p . fallback) (boom . fallback) (q . fallback))))
        (should (equal (mapcar (lambda (r) (plist-get r :reason))
                               (reverse nelisp-eln-switchover-log))
                       '(missing-artifact router-error missing-artifact)))))))

(ert-deftest nelisp-eln-switchover/guard-falls-back-per-call-and-records-once ()
  (nelisp-eln-switchover-test--fresh
    (let* ((counters (vector 0 0))
           (native (lambda (x)
                     (if (symbolp x)
                         (signal 'nelisp-eln-objects-unsupported
                                 (list 'unsupported-symbol-state x))
                       x)))
           (guard (nelisp-eln-switchover--make-guard
                   'g native (lambda (x) (list 'vm x)) counters)))
      (should (equal (funcall guard 5) 5))
      (should (equal (funcall guard 'a) '(vm a)))
      (should (equal (funcall guard 'b) '(vm b)))
      (should (equal counters [1 2]))
      ;; One record per symbol and error kind, not one per call.
      (should (= 1 (length nelisp-eln-switchover-log)))
      (should (eq (plist-get (car nelisp-eln-switchover-log) :op) 'call-fallback))
      ;; Errors outside the coverage refusal propagate unchanged.
      (let ((g2 (nelisp-eln-switchover--make-guard
                 'g2 (lambda (_) (error "native error")) #'identity counters)))
        (should-error (funcall g2 1))))))

(ert-deftest nelisp-eln-switchover/retract-restores-previous-definition ()
  (nelisp-eln-switchover-test--fresh
    (let* ((sym (make-symbol "nelisp-s77-host"))
           (old (lambda () 'old))
           (published (lambda () 'native))
           (ns (nelisp-eln-registration-make-isolated-namespace))
           (owner (make-vector nelisp-eln-registration--owner-size nil)))
      (fset sym published)
      (put sym 'p 'new)
      (aset owner 9 'callable)
      (nelisp-eln-switchover--set-entry
       sym (list :symbol sym :state 'live :publish 'global :namespace ns
                 :callable 'callable :published published :owner owner
                 :previous (cons t old) :saved-props '((p nil))))
      (should (eq (nelisp-eln-switchover-unload sym) 'retracted))
      (should (eq (symbol-function sym) old))
      (should (null (get sym 'p)))
      (should (null (aref owner 9)))
      (should (eq (plist-get (nelisp-eln-switchover-entry sym) :state)
                  'retracted))
      (should (equal nelisp-eln-switchover--pending-unloads (list sym)))
      ;; A second unload of a non-live route is recorded, not an error.
      (should-not (nelisp-eln-switchover-unload sym))
      (should (eq (plist-get (car nelisp-eln-switchover-log) :result)
                  'not-routed)))))

(ert-deftest nelisp-eln-switchover/retract-leaves-superseded-definition ()
  (nelisp-eln-switchover-test--fresh
    (let ((sym (make-symbol "nelisp-s77-host2"))
          (user (lambda () 'user)))
      (fset sym user)
      (nelisp-eln-switchover--set-entry
       sym (list :symbol sym :state 'live :publish 'global
                 :published (lambda () 'native) :previous (cons nil nil)))
      (nelisp-eln-switchover-unload sym)
      (should (eq (symbol-function sym) user))
      (should (plist-get (nelisp-eln-switchover-entry sym) :superseded)))))

(ert-deftest nelisp-eln-switchover/neln-loader-hook-uses-router ()
  (let* ((routed nil)
        (installed nil)
        (nelisp-artifact-neln-legacy-native nil)
        (nelisp-artifact-neln-eln-table '((s77-a . (:eln "/a.eln"))))
        (nelisp-artifact-neln-eln-router
         (lambda (sym eln) (push (cons sym eln) routed))))
    (cl-letf (((symbol-function 'nelisp-eln-registration-load)
               (lambda (&rest _) (error "direct registration must not run")))
              ((symbol-function 'nelisp-artifact--install-native-functions)
               (lambda (_path native) (setq installed (plist-get native :symbols)))))
      (nelisp-artifact--maybe-install-native
       "/x.neln"
       (list :symbols '("s77-a" "s77-b")
             :defuns (list (list :name "s77-a" :arity 0 :rt-slot-count 0)
                           (list :name "s77-b" :arity 0 :rt-slot-count 0))))
      (should (equal routed '((s77-a . (:eln "/a.eln")))))
      (should (equal installed '("s77-b"))))))

(ert-deftest nelisp-eln-switchover/neln-eln-table-from-migration ()
  (should (equal (nelisp-eln-switchover-neln-eln-table
                  (list :entries (list (list :symbol 'a :route 'eln :eln "/a")
                                       (list :symbol 'b :route 'fallback))))
                 '((a :symbol a :route eln :eln "/a")))))

(provide 'nelisp-eln-switchover-test)

;;; nelisp-eln-switchover-test.el ends here
