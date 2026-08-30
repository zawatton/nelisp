;;; nelisp-aot-tco-test.el --- ERT for the Doc 171 self-tail-call pass -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 171 G1 coverage for the transparent self-tail-call optimization
;; in `nelisp-aot-compiler.el': eligibility gating, the allowlist tail
;; walker (sec 5), parallel-reassignment rewrite shape (sec 8.3), the
;; opt-out declare, and semantic equivalence of the rewritten source
;; (executed on host Emacs through a tiny dialect shim: `seq' ->
;; `progn').  Flag-off inertness is asserted structurally: with
;; `nelisp-aot-compiler-tco-enabled' nil the preprocessed defun is
;; identical to the pre-pass output.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defun nelisp-aot-tco-test--pre (form &optional enabled)
  "Preprocess FORM with the TCO flag set to ENABLED."
  (let ((nelisp-aot-compiler-tco-enabled enabled)
        (nelisp-aot-compiler--label-counter 0)
        (nelisp-aot-compiler--tco-log nil))
    (nelisp-aot-compiler--preprocess-source form)))

(defun nelisp-aot-tco-test--calls-to (name tree)
  "Count direct (NAME ...) call forms inside TREE."
  (cond
   ((not (consp tree)) 0)
   ((eq (car tree) name)
    (let ((n 1))
      (dolist (sub (cdr tree))
        (setq n (+ n (nelisp-aot-tco-test--calls-to name sub))))
      n))
   (t (let ((n 0))
        (dolist (sub tree)
          (when (consp sub)
            (setq n (+ n (nelisp-aot-tco-test--calls-to name sub)))))
        n))))

(defun nelisp-aot-tco-test--run (params body &rest args)
  "Evaluate the preprocessed-dialect BODY as Elisp and apply to ARGS.
PARAMS is the lambda list.  `seq' is shimmed to `progn'."
  (apply (eval `(cl-macrolet ((seq (&rest fs) (cons 'progn fs)))
                  (lambda ,params ,body))
               t)
         args))

(defconst nelisp-aot-tco-test--sum-defun
  '(defun tco-sum (n acc)
     (if (= n 0) acc (tco-sum (- n 1) (+ acc n)))))

(defconst nelisp-aot-tco-test--fact-defun
  '(defun tco-fact (n)
     (if (= n 0) 1 (* n (tco-fact (- n 1))))))

;;; Flag off = inert ---------------------------------------------------

(ert-deftest nelisp-aot-tco-flag-off-is-identity ()
  "With the flag nil the defun arm output has the pre-pass shape."
  (let ((off (nelisp-aot-tco-test--pre nelisp-aot-tco-test--sum-defun nil)))
    ;; Direct recursion survives untouched, no loop wrapper appears.
    (should (= (nelisp-aot-tco-test--calls-to 'tco-sum off) 1))
    (should (= (nelisp-aot-tco-test--calls-to 'while off) 0))))

;;; Basic rewrite shape ------------------------------------------------

(ert-deftest nelisp-aot-tco-rewrites-self-tail-call ()
  (let* ((on (nelisp-aot-tco-test--pre nelisp-aot-tco-test--sum-defun t))
         (body (nth 3 on)))
    ;; Loop wrapper: (let ((cont 1) (ret 0)) (seq (while ...) ret))
    (should (eq (car body) 'let))
    (should (= (nelisp-aot-tco-test--calls-to 'while body) 1))
    ;; The tail self-call is gone; no direct call remains.
    (should (= (nelisp-aot-tco-test--calls-to 'tco-sum body) 0))))

(ert-deftest nelisp-aot-tco-semantic-equivalence-sum ()
  (let* ((on (nelisp-aot-tco-test--pre nelisp-aot-tco-test--sum-defun t))
         (body (nth 3 on)))
    (should (equal (nelisp-aot-tco-test--run '(n acc) body 100 0) 5050))
    ;; Constant stack: a depth far beyond max-lisp-eval-depth.
    (should (equal (nelisp-aot-tco-test--run '(n acc) body 200000 0)
                   20000100000))))

(ert-deftest nelisp-aot-tco-swap-arguments-parallel-assignment ()
  "gcd-style swapped recursion needs the temp-let parallel assignment."
  (let* ((on (nelisp-aot-tco-test--pre
              '(defun tco-gcd (a b)
                 (if (= b 0) a (tco-gcd b (% a b))))
              t))
         (body (nth 3 on)))
    (should (= (nelisp-aot-tco-test--calls-to 'tco-gcd body) 0))
    (should (equal (nelisp-aot-tco-test--run '(a b) body 48 18) 6))
    (should (equal (nelisp-aot-tco-test--run '(a b) body 1071 462) 21))))

;; Doc 171 sec 11.1 route 2: a temporary is only load-bearing when a later
;; argument still has to read the parameter it would overwrite.  The rest go
;; direct, which is one less assignment per iteration for the G4 sum and two
;; for a three-way rotation.
(defun nelisp-aot-tco-test--back-edge-setq (body)
  "Return the innermost back-edge `setq' inside BODY, or nil."
  (cond
   ((not (consp body)) nil)
   ((and (eq (car body) 'setq)
         (memq 1 (cdr body))
         (let ((tail (cdr body)) (hit nil))
           (while (consp tail)
             (when (string-prefix-p "nelisp-tco-cont"
                                    (format "%s" (car tail)))
               (setq hit t))
             (setq tail (cdr (cdr tail))))
           hit))
    body)
   (t (let ((tail body) (found nil))
        (while (and (consp tail) (not found))
          (setq found (nelisp-aot-tco-test--back-edge-setq (car tail)))
          (setq tail (cdr tail)))
        found))))

(ert-deftest nelisp-aot-tco-skips-the-temp-nothing-reads ()
  "acc is not read by any later argument, so it is assigned in place."
  (let* ((on (nelisp-aot-tco-test--pre
              '(defun tco-sum (n acc)
                 (if (= n 0) acc (tco-sum (- n 1) (+ acc n))))
              t))
         (edge (nelisp-aot-tco-test--back-edge-setq (nth 3 on))))
    (should edge)
    ;; n -> temp (the second argument still reads it), acc -> direct.
    (should (memq 'acc (cdr edge)))
    (should (= (/ (length (cdr edge)) 2) 4))))

(ert-deftest nelisp-aot-tco-keeps-the-temp-a-later-argument-needs ()
  "gcd's second argument reads a, so a cannot be assigned in place."
  (let* ((on (nelisp-aot-tco-test--pre
              '(defun tco-gcd2 (a b)
                 (if (= b 0) a (tco-gcd2 b (% a b))))
              t))
         (edge (nelisp-aot-tco-test--back-edge-setq (nth 3 on)))
         (targets (let ((tail (cdr edge)) (out nil))
                    (while (consp tail)
                      (push (car tail) out)
                      (setq tail (cdr (cdr tail))))
                    (nreverse out))))
    (should edge)
    ;; First write goes to a temp, not to a.
    (should (string-prefix-p "nelisp-tco-a" (format "%s" (car targets))))
    (should (equal (nelisp-aot-tco-test--run '(a b) (nth 3 on) 1071 462) 21))))

(ert-deftest nelisp-aot-tco-three-way-rotation-is-correct ()
  "x is read by the third argument; y and z are not read after their turn."
  (let* ((on (nelisp-aot-tco-test--pre
              '(defun tco-rot (x y z)
                 (if (= x 0) z (tco-rot y z x)))
              t))
         (body (nth 3 on))
         (edge (nelisp-aot-tco-test--back-edge-setq body)))
    (should (= (/ (length (cdr edge)) 2) 5))
    ;; 3 -> 1 -> 0 stops with z holding what x held two rotations back.
    (should (equal (nelisp-aot-tco-test--run '(x y z) body 3 1 0) 1))))

(ert-deftest nelisp-aot-tco-non-tail-recursion-untouched ()
  "fact's recursion is an argument of *, not a tail call."
  (let* ((off (nelisp-aot-tco-test--pre nelisp-aot-tco-test--fact-defun nil))
         (on (nelisp-aot-tco-test--pre nelisp-aot-tco-test--fact-defun t)))
    (should (equal on off))))

(ert-deftest nelisp-aot-tco-mixed-tail-and-non-tail ()
  "Only the tail-position self-call becomes a back-edge."
  (let* ((on (nelisp-aot-tco-test--pre
              '(defun tco-mix (n acc)
                 (if (= n 0)
                     acc
                   (if (= n 1)
                       (+ 1 (tco-mix 0 acc))   ; non-tail: stays a call
                     (tco-mix (- n 1) (+ acc n)))))
              t))
         (body (nth 3 on)))
    (should (= (nelisp-aot-tco-test--calls-to 'tco-mix body) 1))
    (should (= (nelisp-aot-tco-test--calls-to 'while body) 1))))

;;; Tail-position table (Doc 171 sec 5) --------------------------------

(ert-deftest nelisp-aot-tco-non-tail-forms-are-opaque ()
  "Self-calls inside while / catch / condition-case / unwind-protect
bodies and setq values are never rewritten."
  (dolist (form '((defun tco-w (n) (while (tco-w n) 0))
                  (defun tco-c (n) (catch 'tag (tco-c n)))
                  (defun tco-cc (n) (condition-case nil (tco-cc n)
                                      (error 0)))
                  (defun tco-u (n) (unwind-protect (tco-u n) 0))
                  (defun tco-s (n) (setq n (tco-s n)))))
    (let ((off (nelisp-aot-tco-test--pre form nil))
          (on (nelisp-aot-tco-test--pre form t)))
      (should (equal on off)))))

(ert-deftest nelisp-aot-tco-and-or-tail-positions ()
  (let* ((on (nelisp-aot-tco-test--pre
              '(defun tco-a (n) (and (> n 0) (tco-a (- n 1))))
              t)))
    (should (= (nelisp-aot-tco-test--calls-to 'tco-a (nth 3 on)) 0))
    (should (= (nelisp-aot-tco-test--calls-to 'while (nth 3 on)) 1))))

(ert-deftest nelisp-aot-tco-cond-desugars-to-if-tails ()
  (let* ((on (nelisp-aot-tco-test--pre
              '(defun tco-cd (n acc)
                 (cond ((= n 0) acc)
                       (t (tco-cd (- n 1) (+ acc 1)))))
              t))
         (body (nth 3 on)))
    (should (= (nelisp-aot-tco-test--calls-to 'tco-cd body) 0))
    (should (equal (nelisp-aot-tco-test--run '(n acc) body 100000 0)
                   100000))))

(ert-deftest nelisp-aot-tco-special-let-blocks-rewrite ()
  (let ((nelisp-aot-compiler--special-vars '(tco-dyn)))
    (let ((off (nelisp-aot-tco-test--pre
                '(defun tco-sp (n) (let ((tco-dyn 5)) (tco-sp n)))
                nil))
          (on (nelisp-aot-tco-test--pre
               '(defun tco-sp (n) (let ((tco-dyn 5)) (tco-sp n)))
               t)))
      (should (equal on off))))
  ;; Control: the same shape with a lexical binding IS rewritten.
  (let ((on (nelisp-aot-tco-test--pre
             '(defun tco-lx (n) (let ((m 5)) (if (= n m) n (tco-lx m))))
             t)))
    (should (= (nelisp-aot-tco-test--calls-to 'tco-lx (nth 3 on)) 0))))

;;; Eligibility gates ---------------------------------------------------

(ert-deftest nelisp-aot-tco-opt-out-declare ()
  (let* ((form '(defun tco-opt (n)
                  (declare (nelisp-no-tco))
                  (if (= n 0) 0 (tco-opt (- n 1)))))
         (off (nelisp-aot-tco-test--pre form nil))
         (on (nelisp-aot-tco-test--pre form t)))
    (should (equal on off))))

(ert-deftest nelisp-aot-tco-rest-params-skip ()
  (let* ((form '(defun tco-r (n &rest xs)
                  (if (= n 0) xs (tco-r (- n 1)))))
         (off (nelisp-aot-tco-test--pre form nil))
         (on (nelisp-aot-tco-test--pre form t)))
    (should (equal on off))))

(ert-deftest nelisp-aot-tco-f64-suspect-skip ()
  (let* ((form '(defun tco-f (x)
                  (if (= x 0) 0 (tco-f (f64-to-bits x)))))
         (off (nelisp-aot-tco-test--pre form nil))
         (on (nelisp-aot-tco-test--pre form t)))
    (should (equal on off))))

(ert-deftest nelisp-aot-tco-optional-pad-with-zero ()
  "Omitted &optional args are padded with raw 0, like the call arm."
  (let* ((on (nelisp-aot-tco-test--pre
              '(defun tco-o (n &optional acc)
                 (if (= n 0) acc (tco-o (- n 1))))
              t))
         (body (nth 3 on)))
    (should (= (nelisp-aot-tco-test--calls-to 'tco-o body) 0))
    ;; After one back-edge acc is re-bound to the pad value 0.
    (should (equal (nelisp-aot-tco-test--run '(n acc) body 3 'seed) 0))))

(ert-deftest nelisp-aot-tco-zero-param-defun-skip ()
  (let* ((form '(defun tco-z () (tco-z)))
         (off (nelisp-aot-tco-test--pre form nil))
         (on (nelisp-aot-tco-test--pre form t)))
    (should (equal on off))))

(ert-deftest nelisp-aot-tco-log-records-rewrites ()
  (let ((nelisp-aot-compiler-tco-enabled t)
        (nelisp-aot-compiler--label-counter 0)
        (nelisp-aot-compiler--tco-log nil))
    (nelisp-aot-compiler--preprocess-source nelisp-aot-tco-test--sum-defun)
    (should (equal nelisp-aot-compiler--tco-log '(tco-sum)))
    (nelisp-aot-compiler--preprocess-source nelisp-aot-tco-test--fact-defun)
    (should (equal nelisp-aot-compiler--tco-log '(tco-sum)))))

;;; End-to-end codegen evidence (Doc 171 G3) --------------------------

(ert-deftest nelisp-aot-tco-emitted-code-differs-with-flag ()
  "The pass reaches the real backend, not just the preprocessor.
Compiling the same self-tail-recursive defun to a link unit with the
flag off and on must emit different machine code (the loop form
carries the continue/result slots and the back-edge instead of a
`call').  This is the codegen half of the user-code path: the
artifact pipeline binds the same flag around its compile calls."
  (let* ((form '(seq (defun nelisp-aot-tco-test--sum (n acc)
                       (if (= n 0)
                           acc
                         (nelisp-aot-tco-test--sum (- n 1) (+ acc n))))))
         (text (lambda (flag)
                 (let ((nelisp-aot-compiler-tco-enabled flag))
                   (plist-get (nelisp-aot-compile-to-link-unit
                               form :arch 'x86_64 :format 'elf)
                              :text)))))
    (let ((off (funcall text nil))
          (on (funcall text t)))
      (should (> (length off) 0))
      (should (> (length on) 0))
      (should-not (equal off on)))))

(provide 'nelisp-aot-tco-test)

;;; nelisp-aot-tco-test.el ends here