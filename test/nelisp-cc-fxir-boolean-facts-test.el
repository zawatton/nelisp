;;; nelisp-cc-fxir-boolean-facts-test.el --- Boolean proof producer -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'nelisp-cc-fxir-facts)

(defun nfb-row (name slots body &optional callees)
  (list name 44001 slots callees (list :fn body) '(1 1 -1)))

(defun nfb-condition-proof (result path)
  (cadr (assoc path (plist-get result :boolean-truth-tests))))

(ert-deftest nfb-comparison-flows-through-lexical-store-copy-reassign-and-join ()
  (let* ((row (nfb-row 'unrelated_renamed_predicate 5
                '(:seq
                  (:setslot 0 (:inline 6 10 (:slot 3) (:slot 4)))
                  (:setslot 1 (:slot 0))
                  (:setslot 0 (:const 9))
                  (:if (:slot 2)
                       (:setslot 1 (:inline 13 12 (:slot 3) (:slot 4)))
                    (:setslot 1 (:inline 14 14 (:slot 4) (:slot 3))))
                  (:if (:slot 1) (:nil) (:nil)))))
         (facts (nelisp-cc-fxir-facts-analyze row))
         (proof (nfb-condition-proof facts '(1 5 1))))
    (should (plist-get facts :valid))
    (should (= 6 (plist-get (cadr (assoc '(1 5 1) (plist-get facts :facts))) :type)))
    (should (eq (plist-get proof :kind) :boolean-truth-test))
    (should (equal (plist-get proof :path) '(1 5 1)))
    (should (eq (plist-get proof :status0) t))
    (should (eq (plist-get proof :representation) :word))
    (should (nelisp-cc-fxir-facts-verify row nil facts))))

(ert-deftest nfb-comparison-opcodes-prove-only-on-success ()
  (dolist (opcode '(6 13 14))
    (let* ((row (nfb-row 'renamed 2 `(:if (:inline ,opcode 8 (:slot 0) (:slot 1)) (:nil) (:nil))))
           (facts (nelisp-cc-fxir-facts-analyze row))
           (proof (nfb-condition-proof facts '(1 1))))
      (should (plist-get facts :valid))
      (should (= 6 (plist-get (cadr (assoc '(1 1) (plist-get facts :facts))) :type)))
      (should (= opcode (nelisp-cc-fxir-facts--origin-get
                         (plist-get proof :source) :opcode)))
      (should (= 2 (nelisp-cc-fxir-facts--origin-get
                    (plist-get proof :source) :arity)))))
  (let* ((row (nfb-row 'ordinary_predicate 1 '(:if (:call 4 1 (:slot 0)) (:nil) (:nil)) '(fixnump)))
         (facts (nelisp-cc-fxir-facts-analyze row)))
    (should (plist-get facts :valid))
    (should-not (plist-get facts :boolean-truth-tests))
    (should (= 15 (plist-get (cadr (assoc '(1 1) (plist-get facts :facts))) :type)))))

;; Captured NeLisp FXIR; source rows.el SHA-256:
;; a7ae21edf87564863038e3f95981877d907d9877b650593a5dd55afa1e27251d.
;; The captured program body is analyzed as data and is never executed.
(ert-deftest nfb-if-value-origin-propagates-captured-maple-merge ()
  (let* ((row '(maple-merge 30016 5 (aset aref aset aset aref aset)
                (:fn (:let ((:store 4 (:if (:inline 6 7 (:slot 0) (:const 5))
                                             (:inline 6 15 (:slot 1) (:slot 2))
                                           (:inline 14 23 (:slot 1) (:slot 2)))))
                      (:if (:slot 4)
                           (:seq (:call 46 3 (:slot 3) (:const 33)
                                        (:inline 4 44 (:call 39 2 (:slot 3) (:const 37)) (:const 42)))
                                 (:call 56 3 (:slot 3) (:const 52) (:const 54)) (:const 60))
                         (:seq (:call 79 3 (:slot 3) (:const 66)
                                      (:inline 4 77 (:call 72 2 (:slot 3) (:const 70)) (:const 75)))
                               (:call 89 3 (:slot 3) (:const 85) (:const 87)) (:const 93))) 0))
                (4 4 -1)))
         (facts (nelisp-cc-fxir-facts-analyze row))
         (proof (nfb-condition-proof facts '(1 2 1)))
         (use-origin (plist-get proof :source))
         (store-origin (nelisp-cc-fxir-facts--origin-get use-origin :source))
         (if-origin (nelisp-cc-fxir-facts--origin-get store-origin :source)))
    (should (plist-get facts :valid))
    (should (= 6 (plist-get (cadr (assoc '(1 2 1) (plist-get facts :facts))) :type)))
    (should (eq :if-value (nelisp-cc-fxir-facts--origin-get if-origin :kind)))
    (should (equal (nelisp-cc-fxir-facts--origin-get if-origin :path)
                   '(1 1 0 2)))
    (should (equal (plist-get proof :path) '(1 2 1)))
    (should (nelisp-cc-fxir-origin-decode (plist-get proof :source)))
    (should (nelisp-cc-fxir-facts-verify row nil facts))))

(ert-deftest nfb-if-value-origin-needs-two-proven-boolean-arms ()
  (dolist (selector '((:slot 0) (:nil)))
    (let* ((row (nfb-row 'renamed_if_value 2
                         `(:let ((:store 1 (:if ,selector
                                                 (:inline 13 8 (:slot 0) (:slot 1))
                                               (:inline 14 12 (:slot 0) (:slot 1)))))
                           (:if (:slot 1) (:nil) (:nil)) 0)))
           (facts (nelisp-cc-fxir-facts-analyze row))
           (proof (nfb-condition-proof facts '(1 2 1)))
           (store-origin (nelisp-cc-fxir-facts--origin-get
                          (plist-get proof :source) :source))
           (if-origin (nelisp-cc-fxir-facts--origin-get store-origin :source)))
      (should (eq :if-value (nelisp-cc-fxir-facts--origin-get if-origin :kind)))
      (should (nelisp-cc-fxir-facts-verify row nil facts))))
  (dolist (arms '(((:inline 13 8 (:slot 0) (:slot 1)) (:const 10))
                  ((:const 10) (:inline 14 12 (:slot 0) (:slot 1)))))
    (let* ((row (nfb-row 'renamed_unknown_arm 2
                         `(:let ((:store 1 (:if (:slot 0) ,(car arms) ,(cadr arms))))
                           (:if (:slot 1) (:nil) (:nil)) 0)))
           (facts (nelisp-cc-fxir-facts-analyze row)))
      (should (= 15 (plist-get (cadr (assoc '(1 1 0 2) (plist-get facts :facts))) :type)))
      (should-not (nfb-condition-proof facts '(1 2 1))))))

(ert-deftest nfb-if-value-origin-captured-ash-any-mask-does-not-prove ()
  ;; Same staged capture and provenance as the maple-merge row above.
  (let* ((row '(ash-choice 30018 4 (aref aset aref aset aset aref aset)
                (:fn (:let ((:store 3 (:call 7 2 (:slot 0) (:slot 1))))
                      (:if (:slot 3)
                           (:seq (:call 31 3 (:slot 2) (:const 18)
                                        (:inline 4 29 (:call 24 2 (:slot 2) (:const 22)) (:const 27)))
                                 (:call 41 3 (:slot 2) (:const 37) (:const 39)) (:const 45))
                         (:seq (:call 64 3 (:slot 2) (:const 51)
                                      (:inline 4 62 (:call 57 2 (:slot 2) (:const 55)) (:const 60)))
                               (:call 74 3 (:slot 2) (:const 70) (:const 72)) (:const 78))) 0))
                (3 3 -1)))
         (facts (nelisp-cc-fxir-facts-analyze row)))
    (should (= 15 (plist-get (cadr (assoc '(1 1 0 2) (plist-get facts :facts))) :type)))
    (should-not (nfb-condition-proof facts '(1 2 1)))
    (should (nelisp-cc-fxir-facts-verify row nil facts))))

(ert-deftest nfb-if-value-origin-verifier-invalidates-condition-or-either-arm ()
  (let* ((row (nfb-row 'stale_if_value 2
                       '(:let ((:store 1 (:if (:slot 0)
                                              (:inline 13 8 (:slot 0) (:slot 1))
                                              (:inline 14 12 (:slot 0) (:slot 1)))))
                         (:if (:slot 1) (:nil) (:nil)) 0)))
         (facts (nelisp-cc-fxir-facts-analyze row)))
    (should (nelisp-cc-fxir-facts-verify row nil facts))
    (dolist (index '(1 2 3))
      (let* ((changed (copy-tree row))
             (body (cadr (nth 4 changed)))
             (if-value (nth 2 (car (nth 1 body)))))
        (setf (nth index if-value)
              (pcase index
                (1 '(:slot 1))
                (2 '(:inline 6 20 (:slot 0) (:slot 1)))
                (_ '(:inline 4 25 (:slot 0) (:slot 1)))))
        (should-not (nelisp-cc-fxir-facts-verify changed nil facts))))))

(ert-deftest nfb-calls-polls-scope-and-unknown-overwrite-kill-slot-proof ()
  (let* ((call-row (nfb-row 'caller 3
                     '(:seq (:setslot 0 (:inline 6 8 (:slot 1) (:slot 2)))
                            (:call 5 0)
                            (:if (:slot 0) (:nil) (:nil))) '(unknown)))
         (call-facts (nelisp-cc-fxir-facts-analyze call-row))
         (overwrite-row (nfb-row 'overwrite 3
                          '(:seq (:setslot 0 (:inline 13 8 (:slot 1) (:slot 2)))
                                 (:setslot 0 (:const 9))
                                 (:if (:slot 0) (:nil) (:nil)))))
         (overwrite-facts (nelisp-cc-fxir-facts-analyze overwrite-row))
         (let-row (nfb-row 'scope 3
                    '(:seq (:let ((:store 0 (:inline 14 8 (:slot 1) (:slot 2)))) (:nil) 0)
                           (:if (:slot 0) (:nil) (:nil)))))
         (let-facts (nelisp-cc-fxir-facts-analyze let-row)))
    (should (plist-get call-facts :valid))
    (should-not (plist-get call-facts :boolean-truth-tests))
    (should (assoc '(:call (1 2)) (plist-get call-facts :boundaries)))
    (should (plist-get overwrite-facts :valid))
    (should-not (plist-get overwrite-facts :boolean-truth-tests))
    (should (plist-get let-facts :valid))
    (should-not (plist-get let-facts :boolean-truth-tests))))

(ert-deftest nfb-unsupported-bindings-and-widened-loop-refuse-or-kill ()
  (let* ((unsupported (nfb-row 'unsupported 1 '(:let ((:varbind 3 (:nil))) (:nil) 1)))
         (widened (nfb-row 'looping 3
                    '(:seq (:setslot 0 (:inline 6 8 (:slot 1) (:slot 2)))
                           (:while (:nil) (:setslot 0 (:const 9)))
                           (:if (:slot 0) (:nil) (:nil)))))
         (bad (let ((nelisp-cc-fxir-facts--loop-iterations 1))
                (nelisp-cc-fxir-facts-analyze widened))))
    (should-not (plist-get (nelisp-cc-fxir-facts-analyze unsupported) :valid))
    (should (plist-get bad :valid))
    (should-not (plist-get bad :boolean-truth-tests))))

(ert-deftest nfb-revisited-widened-condition-has-no-stale-proof ()
  (let* ((row (nfb-row 'renamed_loop 3
                '(:seq (:setslot 2 (:inline 6 8 (:slot 0) (:slot 1)))
                       (:while (:slot 2) (:setslot 2 (:const 9))))))
         (facts (nelisp-cc-fxir-facts-analyze row))
         (condition (cadr (assoc '(1 2 1) (plist-get facts :facts))))
         (proof (nfb-condition-proof facts '(1 2 1))))
    (should (plist-get facts :valid))
    (should (= 15 (plist-get condition :type)))
    (should-not proof)
    (should (nelisp-cc-fxir-facts-verify row nil facts))))

(ert-deftest nfb-comparisons-retain-signal-barriers-and-success-facts ()
  (dolist (opcode '(6 13 14))
    (let* ((row (nfb-row 'renamed_comparison 2
                  `(:if (:inline ,opcode 8 (:slot 0) (:slot 1)) (:nil) (:nil))))
           (facts (nelisp-cc-fxir-facts-analyze row)))
      (should (plist-get facts :valid))
      (should (nfb-condition-proof facts '(1 1)))
      (should (assoc (list :signal '(1 1)) (plist-get facts :boundaries)))
      (should (nelisp-cc-fxir-facts-verify row nil facts)))))

(ert-deftest nfb-origin-schema-roundtrips-every-variant-and-proof-dag ()
  (let* ((comparison (nelisp-cc-fxir-facts--origin-make
                      :comparison :path '(1 1) :opcode 6 :arity 2
                      :status0 t :representation :word))
         (assignment (nelisp-cc-fxir-facts--origin-make
                      :assignment :path '(1 2) :slot 0 :version '(:assignment (1 2) 0)
                      :source comparison))
         (let-store (nelisp-cc-fxir-facts--origin-make
                     :let-store :path '(1 3 1 0 2) :slot 1
                     :version '(:assignment (1 3 1 0) 1) :source comparison))
         (slot-use (nelisp-cc-fxir-facts--origin-make
                    :slot-use :path '(1 4) :slot 0 :version '(:assignment (1 2) 0)
                    :source assignment))
         (join (nelisp-cc-fxir-facts--origin-make
                :join :path '(1 5) :slot 0 :version '(:join (1 5) 0)
                :sources (list assignment let-store)))
         (origins (list comparison assignment let-store slot-use join)))
    (dolist (origin origins)
      (should origin)
      (should (equal origin (nelisp-cc-fxir-facts--origin-decode origin))))
    (should (= 6 (nelisp-cc-fxir-facts--origin-get comparison :opcode)))
    (should (equal '(1 5) (nelisp-cc-fxir-facts--origin-get join :path)))
    (let* ((row (nfb-row 'renamed_origin_dag 5
                  '(:seq
                    (:setslot 0 (:inline 6 10 (:slot 3) (:slot 4)))
                    (:setslot 1 (:slot 0))
                    (:if (:slot 2)
                         (:setslot 1 (:inline 13 12 (:slot 3) (:slot 4)))
                      (:setslot 1 (:inline 14 14 (:slot 4) (:slot 3))))
                    (:if (:slot 1) (:nil) (:nil)))))
           (facts (nelisp-cc-fxir-facts-analyze row))
           (proof (nfb-condition-proof facts '(1 4 1)))
           (source (plist-get proof :source)))
      (should (nelisp-cc-fxir-facts-verify row nil facts))
      (should (eq :slot-use (nelisp-cc-fxir-facts--origin-get source :kind)))
      (should (eq :join (nelisp-cc-fxir-facts--origin-get
                         (nelisp-cc-fxir-facts--origin-get source :source) :kind))))))

(ert-deftest nfb-origin-schema-rejects-malformed-and-unbounded-data ()
  (let* ((comparison (nelisp-cc-fxir-facts--origin-make
                      :comparison :path '(1) :opcode 13 :arity 2
                      :status0 t :representation :word))
         (odd (butlast comparison))
         (duplicate '(:kind :comparison :kind :comparison :path (1)
                      :opcode 13 :arity 2 :status0 t :representation :word))
         (unknown-key '(:kind :comparison :path (1) :opcode 13 :arity 2
                        :status0 t :representation :word :extra nil))
         (unknown-kind '(:kind :future :path (1)))
         (wrong-slot (nelisp-cc-fxir-facts--origin-make
                      :slot-use :path '(1) :slot 512 :version '(:entry 0)
                      :source comparison))
         (wrong-version (nelisp-cc-fxir-facts--origin-make
                         :slot-use :path '(1) :slot 0 :version nil :source comparison))
         (wrong-path (nelisp-cc-fxir-facts--origin-make
                      :assignment :path '(1 . 2) :slot 0 :version '(:entry 0)
                      :source comparison))
         (bad-join (nelisp-cc-fxir-facts--origin-make
                    :join :path '(1) :slot 0 :version '(:join (1) 0)
                    :sources (list comparison)))
         (cyclic (list :kind :assignment :path '(1) :slot 0
                       :version '(:entry 0) :source nil))
         (deep comparison))
    (setf (plist-get cyclic :source) cyclic)
    (dotimes (i 30)
      (setq deep (list :kind :assignment :path (list i) :slot 0
                       :version (list :assignment (list i) 0) :source deep)))
    (dolist (bad (list odd duplicate unknown-key unknown-kind wrong-slot wrong-version
                       wrong-path bad-join cyclic deep))
      (should-not (nelisp-cc-fxir-facts--origin-decode bad)))))

(ert-deftest nfb-verifier-recomputes-proof-and-rejects-status-or-origin-tampering ()
  (let* ((row (nfb-row 'proof_row 2 '(:if (:inline 13 8 (:slot 0) (:slot 1)) (:nil) (:nil))))
         (facts (nelisp-cc-fxir-facts-analyze row))
         (mutated (copy-tree facts)))
    (should (nelisp-cc-fxir-facts-verify row nil facts))
    (setcar (cddr (cadr (assoc '(1 1) (plist-get mutated :boolean-truth-tests))) ) nil)
    (should-not (nelisp-cc-fxir-facts-verify row nil mutated))
    (let* ((mutated (copy-tree facts))
           (proof (cadr (assoc '(1 1) (plist-get mutated :boolean-truth-tests)))))
      (setf (plist-get proof :status0) nil)
      (should-not (nelisp-cc-fxir-facts-verify row nil mutated)))
    (let* ((mutated (copy-tree facts))
           (proof (cadr (assoc '(1 1) (plist-get mutated :boolean-truth-tests))))
           (source (plist-get proof :source)))
      (setf (plist-get source :path) '(99))
      (should-not (nelisp-cc-fxir-facts-verify row nil mutated)))
    (should-not (nelisp-cc-fxir-facts-verify
                 (nfb-row 'proof_row 2 '(:if (:inline 6 8 (:slot 0) (:slot 1)) (:nil) (:nil)))
                 nil facts))
    (let ((forced (copy-tree (nelisp-cc-fxir-facts-analyze
                              (nfb-row 'unknown 1 '(:if (:slot 0) (:nil) (:nil)))))))
      (setf (plist-get forced :boolean-truth-tests)
            '(((1 1) (:kind :boolean-truth-test :result-mask 6 :status0 t
                            :representation :word :optimization-authorized t)))
            (plist-get forced :optimization-authorized) t)
      (should-not (nelisp-cc-fxir-facts-verify
                   (nfb-row 'unknown 1 '(:if (:slot 0) (:nil) (:nil))) nil forced)))))

(ert-deftest nfb-verifier-refuses-malformed-candidate-without-signaling ()
  (let* ((row (nfb-row 'malformed_candidate 2
                '(:if (:inline 13 8 (:slot 0) (:slot 1)) (:nil) (:nil))))
         (bad-table (copy-tree (nelisp-cc-fxir-facts-analyze row)))
         (cycle (list :origin-schema 1)))
    (setf (plist-get bad-table :boolean-truth-tests) '(17))
    (setcdr (last cycle) cycle)
    (dolist (bad (list bad-table 17 "invalid" [1 2] '(:origin-schema . 1) cycle))
      (should-not (nelisp-cc-fxir-facts-verify row nil bad)))))

(ert-deftest nfb-verifier-authenticates-slot-version-path-opcode-and-hash ()
  (let* ((row (nfb-row 'versioned_use 3
                '(:seq (:setslot 2 (:inline 6 8 (:slot 0) (:slot 1)))
                       (:if (:slot 2) (:nil) (:nil)))))
         (facts (nelisp-cc-fxir-facts-analyze row)))
    (should (nelisp-cc-fxir-facts-verify row nil facts))
    (dolist (change '((:slot 1) (:version (:entry 2)) (:path (9))))
      (let* ((forged (copy-tree facts))
             (source (plist-get (nfb-condition-proof forged '(1 2 1)) :source)))
        (setf (plist-get source (car change)) (cadr change))
        ;; Each forgery remains a valid leaf schema, but is not this use.
        (should (nelisp-cc-fxir-origin-decode source))
        (should-not (nelisp-cc-fxir-facts-verify row nil forged))))
    (let* ((forged (copy-tree facts))
           (source (plist-get (nfb-condition-proof forged '(1 2 1)) :source))
           (comparison (plist-get (plist-get source :source) :source)))
      (setf (plist-get comparison :opcode) 14)
      (should (nelisp-cc-fxir-origin-decode source))
      (should-not (nelisp-cc-fxir-facts-verify row nil forged)))
    (dolist (change '((:analysis-hash "forged") (:origin-schema 99)
                       (:schema 99) (:optimization-authorized t)))
      (let ((forged (copy-tree facts)))
        (setf (plist-get forged (car change)) (cadr change))
        (should-not (nelisp-cc-fxir-facts-verify row nil forged))))))

(ert-deftest nfb-join-with-one-unknown-arm-cannot-prove-boolean ()
  (dolist (unknown '((:const 9) (:inline 4 8 (:slot 0) (:slot 1))
                     (:call 8 0)))
    (let* ((row (nfb-row 'mixed_join 4
                  `(:seq (:if (:slot 2)
                               (:setslot 3 (:inline 13 8 (:slot 0) (:slot 1)))
                             (:setslot 3 ,unknown))
                         (:if (:slot 3) (:nil) (:nil)))
                  (and (eq (car unknown) :call) '(arbitrary_result))))
           (facts (nelisp-cc-fxir-facts-analyze row)))
      (should (plist-get facts :valid))
      (should (= 15 (plist-get (cadr (assoc '(1 2 1) (plist-get facts :facts))) :type)))
      (should-not (nfb-condition-proof facts '(1 2 1)))
      (should (nelisp-cc-fxir-facts-verify row nil facts)))))

(ert-deftest nfb-poll-and-fixed-point-caps-kill-proofs ()
  (dolist (cap '(0 1 2 5))
    (let* ((nelisp-cc-fxir-facts--loop-iterations cap)
           (row (nfb-row 'renamed_poll 3
                  '(:seq (:setslot 2 (:inline 14 8 (:slot 0) (:slot 1)))
                         (:while (:slot 2) (:nil))
                         (:if (:slot 2) (:nil) (:nil)))))
           (facts (nelisp-cc-fxir-facts-analyze row)))
      (should (plist-get facts :valid))
      (should-not (nfb-condition-proof facts '(1 2 1)))
      (should-not (nfb-condition-proof facts '(1 3 1)))
      (should (assoc '(:poll (1 2)) (plist-get facts :boundaries)))
      (should (cl-every (lambda (entry) (eq (cadr entry) :word))
                        (plist-get facts :boundaries)))
      (should (nelisp-cc-fxir-facts-verify row nil facts)))))

(ert-deftest nfb-ordinary-predicate-names-never-refine ()
  (dolist (name '(fixnump numberp integerp < = > unrelated_predicate))
    (let* ((row (nfb-row 'arbitrary_definition 1
                  '(:if (:call 8 1 (:slot 0)) (:nil) (:nil)) (list name)))
           (facts (nelisp-cc-fxir-facts-analyze row)))
      (should (plist-get facts :valid))
      (should-not (plist-get facts :boolean-truth-tests))
      (should (= 15 (plist-get (cadr (assoc '(1 1) (plist-get facts :facts))) :type)))
      (should (nelisp-cc-fxir-facts-verify row nil facts)))))

(ert-deftest nfb-renamed-store-copy-join-and-reassignment-versions ()
  (dolist (name '(alpha_variant unrelated_beta))
    (let* ((row (nfb-row name 5
                  '(:seq
                    (:let ((:store 3 (:inline 14 8 (:slot 0) (:slot 1))))
                       (:seq (:setslot 4 (:slot 3))
                             (:setslot 3 (:const 9))
                             (:if (:slot 4) (:nil) (:nil))
                             (:if (:slot 3) (:nil) (:nil))) 0)
                    (:if (:slot 3) (:nil) (:nil)))))
           (facts (nelisp-cc-fxir-facts-analyze row))
           (proof (nfb-condition-proof facts '(1 1 2 3 1))))
      (should (plist-get facts :valid))
      (should proof)
      (should-not (nfb-condition-proof facts '(1 1 2 4 1)))
      (should-not (nfb-condition-proof facts '(1 2 1)))
      (should (nelisp-cc-fxir-facts-verify row nil facts)))))

(ert-deftest nfb-arithmetic-and-unbound-constants-remain-any ()
  (dolist (form '((:const 1) (:inline 4 8 (:slot 0) (:slot 1))
                  (:inline 5 8 (:slot 0) (:slot 1))))
    (let* ((row (nfb-row 'unknown_numeric_result 2 `(:if ,form (:nil) (:nil))))
           (facts (nelisp-cc-fxir-facts-analyze row)))
      (should (plist-get facts :valid))
      (should (= 15 (plist-get (cadr (assoc '(1 1) (plist-get facts :facts))) :type)))
      (should-not (plist-get facts :boolean-truth-tests))))
  (let* ((row (nfb-row 'code_binding 2
                '(:if (:inline 6 1 (:slot 0) (:slot 1)) (:nil) (:nil))))
         (facts (nelisp-cc-fxir-facts-analyze row [6 0])))
    (should (nelisp-cc-fxir-facts-verify row [6 0] facts))
    (should-not (plist-get (nelisp-cc-fxir-facts-analyze row [14 0]) :valid))
    (should-not (nelisp-cc-fxir-facts-verify row [14 0] facts))))

(ert-deftest nfb-oversized-origin-chain-falls-back-without-stale-proof ()
  (let ((body '((:setslot 2 (:inline 6 8 (:slot 0) (:slot 1))))))
    (dotimes (_ 30) (setq body (append body '((:setslot 2 (:slot 2))))))
    (let* ((row (nfb-row 'bounded_origin 3
                  (cons :seq (append body '((:if (:slot 2) (:nil) (:nil)))))))
           (facts (nelisp-cc-fxir-facts-analyze row)))
      (should (plist-get facts :valid))
      (should-not (nfb-condition-proof facts '(1 32 1)))
      (should (nelisp-cc-fxir-facts-verify row nil facts)))))

(ert-deftest nfb-shape-valid-boolean-forgery-on-unknown-value-is-rejected ()
  (let* ((row (nfb-row 'unknown_heap_result 1 '(:if (:slot 0) (:nil) (:nil))))
         (forged (copy-tree (nelisp-cc-fxir-facts-analyze row)))
         (path '(1 1))
         (fact (cadr (assoc path (plist-get forged :facts))))
         (origin (nelisp-cc-fxir-origin-make
                  :comparison :path path :opcode 13 :arity 2
                  :status0 t :representation :word)))
    (setf (plist-get fact :type) 6
          (plist-get forged :boolean-truth-tests)
          (list (list path (list :kind :boolean-truth-test
                                 :syntax-sha256 (plist-get forged :analysis-hash)
                                 :path path :source origin :result-mask 6
                                 :representation :word :status0 t))))
    ;; A shape-only verifier would accept this forged unknown-value proof.
    (should (nelisp-cc-fxir-facts--truth-proofs-consistent-p forged))
    (should-not (nelisp-cc-fxir-facts-verify row nil forged))))

(provide 'nelisp-cc-fxir-boolean-facts-test)
;;; nelisp-cc-fxir-boolean-facts-test.el ends here
