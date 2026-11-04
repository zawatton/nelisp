;;; nelisp-cc-fxir-effects-test.el --- FXIR effect-class table -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc-fxir-effects)
(require 'nelisp-cc-fxir-analysis)

(defun nfe--analysis-keywords ()
  "Return op keywords used as `pcase' patterns in the analysis source."
  (let ((file (locate-library "nelisp-cc-fxir-analysis.el"))
        (found nil))
    (cl-labels
        ((pat (p)
           (cond ((keywordp p) (cl-pushnew p found))
                 ((and (consp p) (eq (car p) 'or)) (mapc #'pat (cdr p)))))
         (walk (f)
           (when (consp f)
             (when (eq (car f) 'pcase)
               (dolist (clause (cddr f))
                 (when (consp clause) (pat (car clause)))))
             (while (consp f)
               (walk (car f))
               (setq f (cdr f))))))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t (walk (read (current-buffer))))
          (end-of-file nil))))
    found))

(defun nfe--keywords-checked ()
  (let ((ops (nfe--analysis-keywords)))
    ;; Guard against a vacuous extraction.
    (should (>= (length ops) 20))
    (should (memq :condition-case ops))
    (should (memq :varbind ops))
    ops))

(ert-deftest nfe-table-covers-every-analysis-op ()
  (let ((ops (nfe--keywords-checked)))
    (dolist (op ops)
      (should (assq op nelisp-cc-fxir-effects-table)))))

;; Native lowering dispatches on its own `pcase'; every op it lowers must
;; also have an effect class so later passes never fall back silently.
(defun nfe--native-lower-keywords ()
  (let ((file (locate-library "nelisp-cc-fxir-native.el")) ops)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form) (eq (car form) 'defun)
                         (eq (nth 1 form) 'nelisp-cc-fxir-native--lower))
                (let (walk)
                  (setq walk
                        (lambda (x)
                          (when (consp x)
                            (when (and (eq (car x) 'pcase)
                                       (equal (nth 1 x) '(car n)))
                              (dolist (clause (nthcdr 2 x))
                                (let ((pat (car clause)))
                                  (cond ((keywordp pat) (push pat ops))
                                        ((eq (car-safe pat) 'or)
                                         (dolist (k (cdr pat))
                                           (when (keywordp k) (push k ops))))))))
                            (while (consp x)
                              (funcall walk (car x))
                              (setq x (cdr x))))))
                  (funcall walk form)))))
        (end-of-file nil)))
    (delete-dups ops)))

(ert-deftest nfe-table-covers-every-native-lower-op ()
  (let ((ops (nfe--native-lower-keywords)))
    (should (>= (length ops) 15))
    (should (memq :value ops))
    (dolist (op ops)
      (should (assq op nelisp-cc-fxir-effects-table)))))

(ert-deftest nfe-signal-or-reentry-is-conservative ()
  "Reentry may run Lisp, so it implies call, allocation and GC."
  (dolist (entry nelisp-cc-fxir-effects-table)
    (when (memq :signal-or-reentry (plist-get (cdr entry) :diagnostic))
      (should (plist-get (cdr entry) :may-call-lisp))
      (should (plist-get (cdr entry) :may-allocate))
      (should (plist-get (cdr entry) :may-gc)))))

;; An op with no diagnostic effect carries no flag.  This caught a
;; table edit that flipped `:nil' to may-call-lisp.
(ert-deftest nfe-diagnostic-free-ops-are-pure ()
  (dolist (entry nelisp-cc-fxir-effects-table)
    (unless (plist-get (cdr entry) :diagnostic)
      (should (nelisp-cc-fxir-effects-pure-p (car entry))))))

(ert-deftest nfe-table-entries-are-well-formed ()
  (dolist (entry nelisp-cc-fxir-effects-table)
    (dolist (k '(:may-allocate :may-gc :may-signal :may-call-lisp :nonlocal-exit))
      (should (memq (plist-get (cdr entry) k) '(nil t)))
      (should (plist-member (cdr entry) k)))
    (should (plist-member (cdr entry) :diagnostic))))

(ert-deftest nfe-unknown-op-is-conservative ()
  (let ((e (nelisp-cc-fxir-effects-of :no-such-op)))
    (dolist (k '(:may-allocate :may-gc :may-signal :may-call-lisp :nonlocal-exit :unknown))
      (should (eq t (plist-get e k))))
    (should (equal (plist-get e :diagnostic)
                   '(:unknown-call :allocation-or-gc :signal-or-reentry)))
    (should-not (nelisp-cc-fxir-effects-pure-p :no-such-op))
    ;; Mutating a result must not corrupt the shared default.
    (setf (plist-get e :may-gc) nil)
    (should (eq t (plist-get (nelisp-cc-fxir-effects-of :no-such-op) :may-gc)))))

(ert-deftest nfe-pure-p ()
  (should (nelisp-cc-fxir-effects-pure-p :const))
  (should (nelisp-cc-fxir-effects-pure-p :slot))
  ;; :varref reads a mutable cell and may signal, so it is not pure.
  (should-not (nelisp-cc-fxir-effects-pure-p :varref))
  (should-not (nelisp-cc-fxir-effects-pure-p :call))
  (should-not (nelisp-cc-fxir-effects-pure-p :catch)))

(defconst nfe--golden
  '(((:nil) nil
     (:schema 1 :valid t :row (fixture 17) :syntax-sha256 "d6ff9296bb59e7ffb2bfdf2de3e1ad5f3ece475a27e36905c89e0389a2030ecf" :digest-provenance :harvested-row-syntax :callee-proof :positional-only :constant-proof t :nodes 1 :sites nil :constants nil :effects nil :refusals (:m0-no-transform)))
    ((:slot 0) nil
     (:schema 1 :valid t :row (fixture 17) :syntax-sha256 "cfc2e78746db2604d0add4dd91ec0abc98b48e15930e84f8ac2a1b087904d740" :digest-provenance :harvested-row-syntax :callee-proof :positional-only :constant-proof t :nodes 1 :sites nil :constants nil :effects nil :refusals (:m0-no-transform)))
    ((:const 1) nil
     (:schema 1 :valid t :row (fixture 17) :syntax-sha256 "2021d9491facf0fd638a0db7ca183833939c48b0bc07bcbf194ee47cc67bffe0" :digest-provenance :harvested-row-syntax :callee-proof :positional-only :constant-proof nil :nodes 1 :sites nil :constants ((:origin (17 (1)) :owner 17 :index 1 :kind :unverified)) :effects nil :refusals (:m0-no-transform :unproved-constant)))
    ((:setslot 1 (:slot 0)) nil
     (:schema 1 :valid t :row (fixture 17) :syntax-sha256 "2bd753d0672b912403e22e94b3b524c822df38e2d234213d0a88e35cedb86164" :digest-provenance :harvested-row-syntax :callee-proof :positional-only :constant-proof t :nodes 2 :sites nil :constants nil :effects nil :refusals (:m0-no-transform)))
    ((:varref 3) nil
     (:schema 1 :valid t :row (fixture 17) :syntax-sha256 "abf0515dde3748abf28a3eb2cbc6964836a86864184fc970596bf7deff21e278" :digest-provenance :harvested-row-syntax :callee-proof :positional-only :constant-proof t :nodes 1 :sites nil :constants nil :effects (:mutable-cell-read :signal-or-reentry) :refusals (:m0-no-transform :mutable-global-or-alias)))
    ((:call 1 2 (:call 4 0) (:call 7 0)) (outer left right)
     (:schema 1 :valid t :row (fixture 17) :syntax-sha256 "e98cee2afe382a5a9a847cd96a84993c8fac82fb9e3d6a293e7ef8fb834ae3af" :digest-provenance :harvested-row-syntax :callee-proof :positional-only :constant-proof t :nodes 3 :sites ((:id (17 (1)) :origin (17 (1)) :callee outer :constant-owner 17 :cache-index 1 :argc 2 :identity-proof :absent) (:id (17 (1 3)) :origin (17 (1 3)) :callee left :constant-owner 17 :cache-index 4 :argc 0 :identity-proof :absent) (:id (17 (1 4)) :origin (17 (1 4)) :callee right :constant-owner 17 :cache-index 7 :argc 0 :identity-proof :absent)) :constants nil :effects (:unknown-call :allocation-or-gc :signal-or-reentry :call-observation) :refusals (:m0-no-transform :arbitrary-call)))
    ((:inline 4 5 (:slot 0) (:slot 1)) nil
     (:schema 1 :valid t :row (fixture 17) :syntax-sha256 "3f86d704421472bf020d27cbc3474317ceee7daedadb963d12a4d4f8f3dc5a9f" :digest-provenance :harvested-row-syntax :callee-proof :positional-only :constant-proof t :nodes 3 :sites nil :constants nil :effects (:unproved-primitive :signal-or-reentry) :refusals (:m0-no-transform :unproved-operation)))
    ((:let ((:varbind 1 (:varref 3))) (:while (:slot 0) (:inline 4 5 (:slot 0) (:slot 1))) 1) nil
     (:schema 1 :valid t :row (fixture 17) :syntax-sha256 "b39b458afe35da6f702be11a418024c8b6a8ba4249ff46142192eccdaccc615c" :digest-provenance :harvested-row-syntax :callee-proof :positional-only :constant-proof t :nodes 7 :sites nil :constants nil :effects (:dynamic-binding :signal-or-reentry :mutable-cell-read :poll-or-gc :unproved-primitive) :refusals (:m0-no-transform :dynamic-binding :mutable-global-or-alias :loop :unproved-operation)))
    ((:let ((:store 0 (:call 1 0))) (:condition-case 1 (:call 4 0) (:call 7 0) (10 (:catch (:call 11 0) (:throw (:call 14 0) (:call 17 0))))) 0) (init body success tag throwtag value)
     (:schema 1 :valid t :row (fixture 17) :syntax-sha256 "a0c505eb2b7e926becbd720152fcb3506c8e94ae5de3e1ca5c8f43adfd992b11" :digest-provenance :harvested-row-syntax :callee-proof :positional-only :constant-proof t :nodes 10 :sites ((:id (17 (1 1 0 2)) :origin (17 (1 1 0 2)) :callee init :constant-owner 17 :cache-index 1 :argc 0 :identity-proof :absent) (:id (17 (1 2 2)) :origin (17 (1 2 2)) :callee body :constant-owner 17 :cache-index 4 :argc 0 :identity-proof :absent) (:id (17 (1 2 3)) :origin (17 (1 2 3)) :callee success :constant-owner 17 :cache-index 7 :argc 0 :identity-proof :absent) (:id (17 (1 2 4 1 1)) :origin (17 (1 2 4 1 1)) :callee tag :constant-owner 17 :cache-index 11 :argc 0 :identity-proof :absent) (:id (17 (1 2 4 1 2 1)) :origin (17 (1 2 4 1 2 1)) :callee throwtag :constant-owner 17 :cache-index 14 :argc 0 :identity-proof :absent) (:id (17 (1 2 4 1 2 2)) :origin (17 (1 2 4 1 2 2)) :callee value :constant-owner 17 :cache-index 17 :argc 0 :identity-proof :absent)) :constants nil :effects (:unknown-call :allocation-or-gc :signal-or-reentry :call-observation :nonlocal-exit) :refusals (:m0-no-transform :arbitrary-call :handler-or-nonlocal-exit)))
    ((:seq (:if (:slot 0) (:alias 18) (:nil)) (:prog1 (:and (:slot 0) (:or (:slot 1) (:nil))) (:unwind-protect (:slot 0) (:slot 1)))) nil
     (:schema 1 :valid t :row (fixture 17) :syntax-sha256 "c4df85464bb49caf7155bfbfa0d3ef3cdae8a2114df08bd3f45dd79edb90689b" :digest-provenance :harvested-row-syntax :callee-proof :positional-only :constant-proof t :nodes 14 :sites nil :constants nil :effects (:mutable-cell-read :signal-or-reentry :nonlocal-exit) :refusals (:m0-no-transform :mutable-global-or-alias :handler-or-nonlocal-exit))))
  "Triples (IR CALLEES EXPECTED-REPORT) captured before the table refactor.")

(ert-deftest nfe-golden-analysis-rows-unchanged ()
  (dolist (g nfe--golden)
    (should (equal (nelisp-cc-fxir-analysis-row
                    (list 'fixture 17 2 (nth 1 g) (list :fn (nth 0 g)) '(2 2 -1)))
                   (nth 2 g)))))

;; Doc 231 stage 2: callee-refined :call effects.
(ert-deftest nfe-callee-table-entries ()
  (dolist (e nelisp-cc-fxir-effects-callee-table)
    (should (symbolp (car e)))
    (should (integerp (plist-get (cdr e) :arity)))
    (should (plist-get (cdr e) :fast-path-pure))))

(ert-deftest nfe-call-of-eq-is-non-fallible ()
  (let ((e (nelisp-cc-fxir-effects-call-of 'eq)))
    (dolist (k '(:may-allocate :may-gc :may-signal :may-call-lisp :nonlocal-exit))
      (should-not (plist-get e k)))
    (should (nelisp-cc-fxir-effects-pure-p :nil))))

(ert-deftest nfe-call-of-guard-fallback-stays-conservative ()
  (let ((e (nelisp-cc-fxir-effects-call-of 'car)))
    (should (plist-get e :fast-path-pure))
    (dolist (k '(:may-allocate :may-gc :may-signal :may-call-lisp))
      (should (plist-get e k)))))

(ert-deftest nfe-call-of-unknown-is-conservative ()
  (should (equal (nelisp-cc-fxir-effects-call-of 'no-such-fn)
                 (nelisp-cc-fxir-effects-of :call)))
  (should (equal (nelisp-cc-fxir-effects-call-of nil)
                 (nelisp-cc-fxir-effects-of :call)))
  (should-not (plist-get (nelisp-cc-fxir-effects-call-of 'no-such-fn) :fast-path-pure)))

(ert-deftest nfe-region-ops-are-may-signal-only ()
  "Doc 237: nine region operations may only signal; stats may allocate."
  (dolist (op '(nelisp--region-open nelisp--region-close nelisp--region-move
                nelisp--region-live-p nelisp--region-size nelisp--region-u8-ref
                nelisp--region-u8-set nelisp--region-i64-ref nelisp--region-i64-set))
    (let ((e (nelisp-cc-fxir-effects-call-of op)))
      (should (plist-get e :may-signal))
      (dolist (k '(:may-allocate :may-gc :may-call-lisp))
        (should-not (plist-get e k)))))
  (let ((e (nelisp-cc-fxir-effects-call-of 'nelisp--region-stats)))
    (should (plist-get e :may-allocate))
    (should-not (plist-get e :may-call-lisp)))
  (should (= 10 (length nelisp-cc-fxir-effects-region-table))))

(provide 'nelisp-cc-fxir-effects-test)
;;; nelisp-cc-fxir-effects-test.el ends here
