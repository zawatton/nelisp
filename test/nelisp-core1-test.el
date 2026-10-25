;;; nelisp-core1-test.el --- native equal / sxhash / list search (core1) -*- lexical-binding: t; -*-

;; Static checks of the generated word forms.  Behaviour is covered by the
;; parity corpora run on the built binary (target/core1/corpus-*.el).

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defun core1-test--defun (name)
  (cl-some (lambda (group)
             (cl-find-if (lambda (f) (and (eq (car-safe f) 'defun) (eq (cadr f) name)))
                         (and (boundp group) (symbol-value group))))
           '(nelisp-standalone--applyfn-ht-helpers nelisp-standalone--applyfn-search-helpers
             nelisp-standalone--applyfn-fast-list-helpers nelisp-standalone--applyfn-bf-helpers)))

(defun core1-test--mentions (symbol form)
  (or (eq form symbol)
      (and (consp form) (or (core1-test--mentions symbol (car form))
                            (core1-test--mentions symbol (cdr form))))))

(defun core1-test--find (needle form)
  (cond ((equal form needle) form)
        ((consp form) (or (core1-test--find needle (car form)) (core1-test--find needle (cdr form))))))

(defun core1-test--arm (name)
  (cl-find-if (lambda (e) (equal (cadar e) name)) (nelisp-standalone--applyfn-reader-table)))

(ert-deftest core1-key-eq-is-allocation-free-for-common-shapes ()
  ;; `wf_key_eq' (member/assoc/rassoc/equal-keyed tables) must decide through
  ;; `wf_eqd' first; `wf_key_eq_depth' (a 1.6 KB scratch stack per call) is the
  ;; fallback for undecided shapes only.
  (let ((f (core1-test--defun 'wf_key_eqn)))
    (should f)
    (should (core1-test--mentions 'wf_eqd f))
    (dolist (n '(wf_eqd wf_eqd_cons wf_eqd_vec wf_sxh wf_sxh_list wf_sxh_vec wf_sxh_rec))
      (let ((g (core1-test--defun n)))
        (should g)
        (should-not (core1-test--mentions 'alloc-bytes g))
        (should-not (core1-test--mentions 'wf_key_eq_depth g))))))

(ert-deftest core1-public-equal-decides-natively ()
  (let ((arm (core1-test--arm "equal")))
    (should arm)
    (should (core1-test--mentions 'bf_equal_dispatch_w arm))
    (let ((d (core1-test--defun 'bf_equal_dispatch_w)))
      (should d)
      (should (core1-test--mentions 'wf_eqd d))
      ;; Only the undecided case delegates to the Lisp reference.
      (should (core1-test--mentions 'bf_n2_call_lisp_equal_w d))
      ;; GNU `equal' never signals circular-list (FOR_EACH_TAIL_BASIC).
      (should-not (core1-test--mentions 'bf_signal_circular_list d)))))

(ert-deftest core1-cyclic-lists-delegate-to-the-gnu-port ()
  ;; A cdr cycle makes `wf_eqd_cons' answer 2 (undecided), never 3 or an error;
  ;; the Lisp reference ports internal_equal_1/internal_equal_cycle.
  (let ((f (core1-test--defun 'wf_eqd_cons)))
    (should f)
    (should-not (equal (core1-test--find '(setq res 3) f) '(setq res 3))))
  (let ((text (with-temp-buffer
                (insert-file-contents (expand-file-name "scripts/nelisp-stdlib-prelude.el"
                                                        nelisp-standalone--repo-root))
                (buffer-string))))
    (dolist (n '("(defun nelisp--equal-cycle " "(defun nelisp--equal-seen "
                 "Stack overflow in equal"))
      (should (string-search n text)))))

(ert-deftest core1-alist-get-testfn-uses-static-symbol-ids ()
  (let ((f (core1-test--defun 'wf_testfn_mode)))
    (should f)
    (should (core1-test--mentions 'nl_sym_sid f))
    (should (= (nelisp-standalone--sid "eq") 66))
    (should (= (nelisp-standalone--sid "equal") 182)))
  (should-not (core1-test--defun 'wf_symbol_is_equal)))

(ert-deftest core1-native-arms-registered ()
  (dolist (n '("rassq" "alist-get" "assoc-string" "sxhash-equal" "sxhash-eql"))
    (should (core1-test--arm n))
    (should (assoc n (nelisp-standalone--builtin-arities)))
    (should (member n nelisp-standalone--reader-builtins)))
  (should (equal (cdr (assoc "alist-get" (nelisp-standalone--builtin-arities))) '(2 5)))
  (should (nelisp-standalone--validate-reader-registrations)))

(ert-deftest core1-slow-delegates-exist-in-prelude ()
  (let ((text (with-temp-buffer
                (insert-file-contents (expand-file-name "scripts/nelisp-stdlib-prelude.el"
                                                        nelisp-standalone--repo-root))
                (buffer-string))))
    (dolist (n '("(defun nelisp--alist-get-slow " "(defun nelisp--assoc-string-slow "))
      (should (string-search n text)))
    ;; `sxhash' must not sit inside the `unless (fboundp (quote sxhash-equal))' block.
    (should (string-search "(unless (fboundp 'sxhash)" text))))

(provide 'nelisp-core1-test)
