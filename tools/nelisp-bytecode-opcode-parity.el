;;; nelisp-bytecode-opcode-parity.el --- Ordered opcode parity cases -*- lexical-binding: t; -*-

(require 'nelisp-bytecode-corpus-parity)

(defconst nelisp-bytecode-opcode-parity-cases
  '((call (192 193 194 34 135) [list 1 2] 4)
    ;; Bcall family (raw 32-39, all decode to base 32): embedded operand
    ;; 0-5 (raw 32-37), explicit 1-byte operand (raw 38, any count that
    ;; fits a byte), explicit 2-byte operand (raw 39). Callees are lambda
    ;; literals pushed straight from the constants vector, so these
    ;; exercise the call gateway itself rather than any dedicated opcode.
    (call-arity-0 (192 32 135) [(lambda () 7)] 2)
    (call-arity-1 (192 193 33 135) [(lambda (x) (* x 3)) 5] 3)
    (call-arity-3 (192 193 194 195 35 135)
                  [(lambda (a b c) (list a b c)) 1 2 3] 5)
    (call-arity-4 (192 193 194 195 196 36 135)
                  [(lambda (a b c d) (list a b c d)) 1 2 3 4] 6)
    (call-arity-5 (192 193 194 195 196 197 37 135)
                  [(lambda (a b c d e) (list a b c d e)) 1 2 3 4 5] 7)
    (call-arity-6-explicit-1-byte
     (192 193 194 195 196 197 198 38 6 135)
     [(lambda (a b c d e f) (list a b c d e f)) 1 2 3 4 5 6] 8)
    (call-arity-7-explicit-1-byte
     (192 193 194 195 196 197 198 199 38 7 135)
     [(lambda (a b c d e f g) (list a b c d e f g)) 1 2 3 4 5 6 7] 9)
    (call-arity-2-explicit-2-byte
     ;; Raw 39 (2-byte operand) is never emitted by byte-compile in
     ;; practice (only for 256+ direct args), but the encoding is valid
     ;; and `byte-code' on host accepts it, so it is ground-truthed the
     ;; same way as `constant2-index-256' below.
     (192 193 194 39 2 0 135) [(lambda (a b) (+ a b)) 10 20] 4)
    (call-builtin-via-symbol (192 193 33 135) [car (1 . 2)] 3)
    (call-builtin-fset-override nil nil nil
     (let ((old (symbol-function 'car)))
       (unwind-protect
           (progn (fset 'car (lambda (&rest _) 'overridden))
                  (byte-code (unibyte-string 192 193 33 135)
                             (vector 'car '(1 . 2)) 3))
         (fset 'car old))))
    (call-symbol-lambda-cell nil nil nil
     (progn (fset 'nl-parity-lambda-sym (lambda (x) (+ x 100)))
            (byte-code (unibyte-string 192 193 33 135)
                       (vector 'nl-parity-lambda-sym 5) 3)))
    (call-symbol-bytecode-cell nil nil nil
     (progn (fset 'nl-parity-bc-sym (make-byte-code 257 "T\207" [] 2))
            (byte-code (unibyte-string 192 193 33 135)
                       (vector 'nl-parity-bc-sym 41) 3)))
    (call-wrong-type-from-callee nil nil nil
     (condition-case e
         (byte-code (unibyte-string 192 193 33 135) (vector 'car 5) 3)
       (error (list (car e) (cadr e)))))
    (call-wrong-number-of-arguments nil nil nil
     (condition-case e
         (byte-code (unibyte-string 192 193 194 34 135) (vector 'car 1 2) 4)
       (error (list (car e) (caddr e)))))
    (call-void-function nil nil nil
     (condition-case e
         (byte-code (unibyte-string 192 32 135)
                    (vector 'nl-parity-totally-undefined-fn-xyz) 2)
       (error (list (car e) (cadr e)))))
    (stack-ref-embedded (192 193 1 92 135) [10 20] 4)
    (stack-ref-wide-1-byte (192 193 194 195 196 197 6 5 92 135) [1 2 3 4 5 100] 8)
    (pushconditioncase-caught
     (193 49 9 0 194 195 33 48 135 24 196 8 41 68 135)
     [e (error) error "boom" caught] 2)
    (pushconditioncase-no-error
     (192 49 7 0 193 194 92 48 135) [error 1 2] 4)
    (car (192 64 135) [(1 . 2)] 2)
    (cdr (192 65 135) [(1 . 2)] 2)
    (varref (193 24 8 41 135) [bytecode-x 7] 2)
    (goto-if-not-nil (192 132 8 0 193 130 9 0 194 135) [t no yes] 2)
    (goto-if-nil-else-pop (192 133 5 0 193 135) [nil bad] 2)
    (cons (192 193 66 135) [1 2] 3)
    (memq (192 193 62 135) [b (a b c)] 3)
    (eq (192 193 61 135) [same same] 3)
    (goto-if-not-nil-else-pop (192 134 5 0 193 135) [t bad] 2)
    (car-safe (192 162 135) [4] 2)
    (not (192 63 135) [nil] 2)
    (discardN (192 193 182 1 135) [1 2] 3)
    (discardN-distinct-count-one (192 193 194 182 1 135) [11 22 33] 3)
    (discardN-preserve-zero (192 193 182 128 135) [1 2] 2)
    (discardN-preserve-one (192 193 194 182 129 135) [1 2 3] 3)
    (discardN-preserve-distinct-count-one
     (192 193 194 182 129 135) [11 22 33] 3)
    (constant2-index-256 nil nil nil
     (let ((constants (make-vector 257 nil)))
       (aset constants 256 'constant2-target)
       (byte-code (unibyte-string 129 0 1 135) constants 1)))
    (equal-list (192 193 154 135) [(a b) (a b)] 3)
    (equal-not-list (192 193 154 135) [(a b) (a c)] 3)
    (nthcdr-list (192 193 155 135) [2 (a b c)] 3)
    (nthcdr-negative (192 193 155 135) [-1 (a b c)] 3)
    (member-return-tail (192 193 157 135) [b (a b c)] 3)
    ;; Member must return the original list tail, shared with nthcdr.
    (member-tail-alias (192 193 157 194 193 155 61 135) [b (a b c) 1] 5)
    (equal-fset-override nil nil nil
     (let ((old (symbol-function 'equal)))
       (unwind-protect
           (progn (fset 'equal (lambda (&rest _) nil))
                  (byte-code (unibyte-string 192 193 154 135) '[(a) (a)] 3))
         (fset 'equal old))))
    (nthcdr-fset-override nil nil nil
     (let ((old (symbol-function 'nthcdr)))
       (unwind-protect
           (progn (fset 'nthcdr (lambda (&rest _) nil))
                  (byte-code (unibyte-string 192 193 155 135) '[1 (a b)] 3))
         (fset 'nthcdr old))))
    (member-fset-override nil nil nil
     (let ((old (symbol-function 'member)))
       (unwind-protect
           (progn (fset 'member (lambda (&rest _) nil))
                  (byte-code (unibyte-string 192 193 157 135) '[b (a b)] 3))
         (fset 'member old))))
    (nthcdr-wrong-type nil nil nil
     (condition-case e (byte-code (unibyte-string 192 193 155 135) '[x (a b)] 3)
       (error (list (car e) (cadr e)))))
    (nthcdr-improper-list nil nil nil
     (condition-case e (byte-code (unibyte-string 192 193 155 135) '[2 (a . b)] 3)
       (error (list (car e) (cadr e)))))
    (member-wrong-type nil nil nil
     (condition-case e (byte-code (unibyte-string 192 193 157 135) '[x 42] 3)
       (error (list (car e) (cadr e)))))
    (member-improper-list nil nil nil
     (condition-case e (byte-code (unibyte-string 192 193 157 135) '[z (a . b)] 3)
       (error (list (car e) (cadr e)))))
    (list1 (192 67 135) [1] 2)
    (stringp-true (192 59 135) ["x"] 2)
    (stringp-false (192 59 135) [1] 2)
    (integerp-true (192 168 135) [1] 2)
    (integerp-false (192 168 135) ["x"] 2)
    (numberp-fixnum (192 167 135) [1] 2)
    (numberp-float (192 167 135) [1.5] 2)
    (numberp-bignum nil nil nil
     (let ((large (* 1208925819614629174706176 8193)))
       (byte-code (unibyte-string 192 167 135) (vector large) 2)))
    (numberp-string-false (192 167 135) ["1"] 2)
    (numberp-fset-override nil nil nil
     (let ((old (symbol-function 'numberp)))
       (unwind-protect
           (progn (fset 'numberp (lambda (&rest _) nil))
                  (byte-code (unibyte-string 192 167 135) '[1] 2))
         (fset 'numberp old))))
    (concat2-strings (192 193 80 135) ["ab" "cd"] 3)
    (concat2-unibyte nil nil nil
     (let ((a (string-as-unibyte "ab"))
           (b (string-as-unibyte "cd"))
           (result nil))
       (setq result (byte-code (unibyte-string 192 193 80 135)
                               (vector a b) 3))
       (list (multibyte-string-p result) (string-bytes result))))
    (concat2-fset-override nil nil nil
     (let ((old (symbol-function 'concat)))
       (unwind-protect
           (progn (fset 'concat (lambda (&rest _) "shadow"))
                  (byte-code (unibyte-string 192 193 80 135) '["a" "b"] 3))
         (fset 'concat old))))
    (concat2-wrong-type nil nil nil
     (condition-case e (byte-code (unibyte-string 192 193 80 135) '[1 "b"] 3)
       (error (list (car e) (cadr e)))))
    (concat3-strings (192 193 194 81 135) ["a" "b" "c"] 4)
    (concat3-unibyte nil nil nil
     (let ((a (string-as-unibyte "a"))
           (b (string-as-unibyte "b"))
           (c (string-as-unibyte "c"))
           (result nil))
       (setq result (byte-code (unibyte-string 192 193 194 81 135)
                               (vector a b c) 4))
       (list (multibyte-string-p result) (string-bytes result))))
    (concat3-fset-override nil nil nil
     (let ((old (symbol-function 'concat)))
       (unwind-protect
           (progn (fset 'concat (lambda (&rest _) "shadow"))
                  (byte-code (unibyte-string 192 193 194 81 135)
                             '["a" "b" "c"] 4))
         (fset 'concat old))))
    (concat3-wrong-type nil nil nil
     (condition-case e
         (byte-code (unibyte-string 192 193 194 81 135) '["a" 1 "c"] 4)
       (error (list (car e) (cadr e)))))
    (min-integer (192 193 94 135) [8 -3] 3)
    (max-integer (192 193 93 135) [8 -3] 3)
    (min-float-int (192 193 94 135) [3 1.5] 3)
    (max-float-int (192 193 93 135) [3 1.5] 3)
    (min-bignum nil nil nil
     (let ((big (* 1208925819614629174706176 8193)))
       (byte-code (unibyte-string 192 193 94 135) (vector big 9) 3)))
    (max-bignum nil nil nil
     (let ((big (* 1208925819614629174706176 8193)))
       (byte-code (unibyte-string 192 193 93 135) (vector 9 big) 3)))
    (plus-float (192 193 92 135) [2.5 1.25] 3)
    (plus-bignum nil nil nil
     (let ((big (* 1208925819614629174706176 8193)))
       (byte-code (unibyte-string 192 193 92 135) (vector big 9) 3)))
    (plus-fixnum-overflow nil nil nil
     (byte-code (unibyte-string 192 193 92 135)
                (vector most-positive-fixnum 1) 3))
    (plus-fset-override nil nil nil
     (let ((old (symbol-function '+)))
       (unwind-protect
           (progn (fset '+ (lambda (&rest _) 999))
                  (byte-code (unibyte-string 192 193 92 135) '[4 7] 3))
         (fset '+ old))))
    (plus-argument-float nil nil nil
     (funcall (make-byte-code 514 (unibyte-string 1 1 92 135) [] 4)
              2.5 1.25))
    (plus-argument-bignum nil nil nil
     (funcall (make-byte-code 514 (unibyte-string 1 1 92 135) [] 4)
              (* 1208925819614629174706176 8193) 1))
    (plus-argument-fixnum-overflow nil nil nil
     (funcall (make-byte-code 514 (unibyte-string 1 1 92 135) [] 4)
              most-positive-fixnum 1))
    (plus-argument-fset-override nil nil nil
     (let ((old (symbol-function '+)))
       (unwind-protect
           (progn (fset '+ (lambda (&rest _) 999))
                  (funcall
                   (make-byte-code 514 (unibyte-string 1 1 92 135) [] 4)
                   2 3))
         (fset '+ old))))
    (mult-integer (192 193 95 135) [6 7] 3)
    (mult-float (192 193 95 135) [2.5 4] 3)
    (mult-bignum nil nil nil
     (byte-code (unibyte-string 192 193 95 135)
                (vector most-positive-fixnum 2) 3))
    (mult-fset-override nil nil nil
     (let ((old (symbol-function '*)))
       (unwind-protect
           (progn (fset '* (lambda (&rest _) 'overridden))
                  (byte-code (unibyte-string 192 193 95 135) '[6 7] 3))
         (fset '* old))))
    (mult-wrong-type nil nil nil
     (condition-case e
         (byte-code (unibyte-string 192 193 95 135) ["x" 2] 3)
       (error (list (car e) (cadr e)))))
    ;; Regression guard for the opcode-95 core-mode investigation: a loop
    ;; using `*' to compute a byte offset per iteration (the exact shape
    ;; nelisp-eln-system-loader--file-u/nelisp-eln-raw-call-word use,
    ;; `(* WIDTH i)' with i incremented by the real `1+') under lexical
    ;; binding, matching every core module's own declared binding mode.
    ;; This was never actually about opcode 95 -- the historical `(ash 1)'
    ;; regression traced back to core-mode compiling under the WRONG
    ;; (forced dynamic) binding mode, fixed separately -- but this pins
    ;; the loop shape down at the opcode-parity level too.
    (mult-write-loop-index-matches-interpreted nil nil nil
     (funcall
      (make-byte-code
       257 (unibyte-string 192 193 2 131 26 0 1 137 194 95 66 1 66 178 1 1
                            84 178 2 2 65 178 3 130 2 0 159 135)
       [0 nil 8] 6)
      '(10 20 30 40 50 60)))
    ;; Bswitch (183): an `eq'-test jump table with 7 symbol keys,
    ;; including the literal symbol `nil' itself (distinct from every
    ;; other key, tag-wise, on this runtime) and a miss that must fall
    ;; through rather than jump, built the ordinary way
    ;; (`make-hash-table'/`puthash') and driving the exact byte-code
    ;; nelisp-eln-leaf-code-valid-p's compiled switch uses.  The
    ;; switch-reader-* cases below cover jump tables that arrive as
    ;; `#s(hash-table ...)' reader literals, the way baked core source
    ;; delivers them.
    (switch-eq-symbol-keys-including-nil-and-miss nil nil nil
     (let ((ht (make-hash-table :test 'eq)))
       (puthash 'immediate 6 ht) (puthash 'move 8 ht) (puthash nil 10 ht)
       (puthash 'test 12 ht) (puthash 'jz 14 ht) (puthash 'jmp 16 ht)
       (puthash 'ret 18 ht)
       (let ((probe
              (make-byte-code
               257
               (unibyte-string 137 192 183 130 20 0 193 135 194 135 195 135
                                196 135 197 135 198 135 199 135 200 135)
               (vector ht 1 2 3 4 5 6 7 'default)
               3)))
         (list (funcall probe 'immediate) (funcall probe 'move)
               (funcall probe nil) (funcall probe 'test)
               (funcall probe 'jz) (funcall probe 'jmp)
               (funcall probe 'ret) (funcall probe 'xyz)))))
    ;; A jump table read from a `#s(hash-table ...)' literal that follows a
    ;; 600-element list in the same form.  The native single-form reader
    ;; used to decline any form needing more than 2048 parser slots and
    ;; fall back to the Elisp reader, which read `#s(...)' as the symbol
    ;; `#s' followed by a list; every baked core defun whose
    ;; (unibyte-string ...) literal was that long lost its jump table.
    (switch-reader-literal-after-long-list nil nil nil
     (let* ((text (concat "(x (" (mapconcat #'identity (make-list 600 "1") " ")
                          ") #s(hash-table test eq data (a 1 b 2)))"))
            (form (car (read-from-string text)))
            (table (nth 2 form)))
       (list (length (nth 1 form)) (hash-table-p table)
             (and (hash-table-p table) (gethash 'b table)))))
    ;; Fixnum keys, `eq' test, table from the reader.
    (switch-reader-eq-fixnum-keys-and-miss nil nil nil
     (let* ((table (car (read-from-string
                         "#s(hash-table test eq data (1 6 2 8 3 10 4 12))")))
            (probe (make-byte-code
                    257
                    (unibyte-string 137 192 183 130 14 0 193 135 194 135
                                    195 135 196 135 197 135)
                    (vector table 'one 'two 'three 'four 'other)
                    3)))
       (mapcar probe '(1 2 3 4 9 nil))))
    ;; String keys, `equal' test: GNU's compiled `(cond ((equal x "a") ...))'.
    (switch-reader-equal-string-keys-and-miss nil nil nil
     (let* ((table (car (read-from-string
                         "#s(hash-table test equal data (\"a\" 6 \"b\" 8 \"c\" 10 \"d\" 12))")))
            (probe (make-byte-code
                    257
                    (unibyte-string 137 192 183 130 14 0 193 135 194 135
                                    195 135 196 135 197 135)
                    (vector table 1 2 3 4 5)
                    3)))
       (list (funcall probe "a") (funcall probe (concat "b" ""))
             (funcall probe "c") (funcall probe "d") (funcall probe "zz")
             (funcall probe 'a))))
    ;; A whole byte-code function literal with a jump table, read by
    ;; `read-from-string' as core bytecode installation does.
    (switch-reader-bytecode-literal nil nil nil
     (let ((f (car (read-from-string
                    "(make-byte-code 257 (unibyte-string 137 192 183 130 14 0 193 135 194 135 195 135 196 135 197 135) [#s(hash-table test eq purecopy t data (a 6 b 8 c 10 nil 12)) 1 2 3 4 5] 3)"))))
       (mapcar (eval f t) '(a b c nil zz))))
    (min-equal-identity nil nil nil
     (let ((a 1) (b 1.0))
       (eq a (byte-code (unibyte-string 192 193 94 135) (vector a b) 3))))
    (max-fset-override nil nil nil
     (let ((old (symbol-function 'max)))
       (unwind-protect
           (progn (fset 'max (lambda (&rest _) -999))
                  (byte-code (unibyte-string 192 193 93 135) '[4 7] 3))
         (fset 'max old))))
    (min-fset-override nil nil nil
     (let ((old (symbol-function 'min)))
       (unwind-protect
           (progn (fset 'min (lambda (&rest _) 999))
                  (byte-code (unibyte-string 192 193 94 135) '[4 7] 3))
         (fset 'min old))))
    (min-wrong-type nil nil nil
     (condition-case e (byte-code (unibyte-string 192 193 94 135) '[4 bad] 3)
       (error (list (car e) (cadr e)))))
    (listp-true (192 60 135) [nil] 2)
    (listp-false (192 60 135) [1] 2)
    (symbolp-true (192 57 135) [predicate-symbol] 2)
    (symbolp-false (192 57 135) [1] 2)
    (stringp-branch-true (192 59 131 7 0 193 135 194 135)
                         ["x" yes no] 2)
    (stringp-branch-false (192 59 131 7 0 193 135 194 135)
                          [1 yes no] 2)
    ;; Host Emacs 31.1 byte-compiles vendor `minusp' to: argument slot,
    ;; dup, constant[0], byte-lss, return. Prefix the argument as a constant.
    (minusp-negative (193 137 192 87 135) [0 -1] 3)
    (minusp-zero (193 137 192 87 135) [0 0] 3)
    (minusp-positive (193 137 192 87 135) [0 1] 3)
    (length (192 71 135) [(a b c)] 2)
    (consp (192 58 135) [(a)] 2)
    (unbind (193 24 192 41 135) [bytecode-x 7] 2)
    (eqlsign (192 193 85 135) [2 2] 3)
    (varbind (193 24 8 41 135) [bytecode-x 7] 2)
    (sub1 (192 83 135) [3] 2)
    (gtr (192 193 86 135) [3 2] 3)
    (nth (192 193 56 135) [1 (a b)] 3)
    (nth-out-of-range-proper-list-nil (192 193 56 135) [5 (a b c)] 3)
    (nth-negative-n-returns-car (192 193 56 135) [-1 (a b c)] 3)
    ;; Opcode 56 (Bnth) has its own inline loop in bytecode.c, distinct
    ;; from `Fnth'/interpreted `nth' (both are `Fcar (Fnthcdr (N,
    ;; LIST))'): for a fixnum N in [0, 127] it walks cdr N times and, if
    ;; what it lands on is neither a cons nor nil, signals wrong-type-
    ;; argument listp against THAT REACHED TAIL -- not the whole list,
    ;; unlike `Fnthcdr's `CHECK_LIST_END', which deliberately names the
    ;; whole list. Host byte-compiled (lambda (x) (nth 7 x)) on
    ;; '(1 . 2) reports (wrong-type-argument listp 2); interpreted
    ;; (nth 7 '(1 . 2)) reports (wrong-type-argument listp (1 . 2)) --
    ;; both verified on host GNU 31.1, see nth-interpreted-vs-bytecode-
    ;; error-datum-known-gap below.
    (nth-improper-tail-reports-reached-cdr nil nil nil
     (condition-case e (byte-code (unibyte-string 192 193 56 135) '[7 (1 . 2)] 3)
       (error e)))
    (nth-improper-tail-longer-reports-reached-cdr nil nil nil
     (condition-case e (byte-code (unibyte-string 192 193 56 135) '[7 (1 2 . 3)] 3)
       (error e)))
    (nth-bignum-n-improper-tail-reports-reached-cdr nil nil nil
     (condition-case e
         (byte-code (unibyte-string 192 193 56 135)
                    (vector (+ most-positive-fixnum 1000) '(a . b)) 3)
       (error e)))
    (setcar (192 193 160 135) [(1 . 2) 9] 3)
    (diff (192 193 90 135) [7 2] 3)
    (list2 (192 193 68 135) [left right] 3)
    (leq-true (192 193 88 135) [2 2] 3)
    (leq-false (192 193 88 135) [2 1] 3)
    (geq-true (192 193 89 135) [2 2] 3)
    (geq-false (192 193 89 135) [1 2] 3)
    (aref-vector (192 193 72 135) [[alpha beta] 1] 3)
    (aref-type-error nil nil nil
     (condition-case e (byte-code (unibyte-string 192 193 72 135) '[[a b] bad] 3)
       (error (list (car e) (cadr e)))))
    ;; make-closure (GNU alloc.c Fmake_closure): not a byte-code opcode
    ;; itself, but the real subr Bcall(32) reaches for whenever compiled
    ;; lexical code closes over an enclosing binding -- see the
    ;; needs-make-closure-not-yet-implemented guard this now lets adopt.
    (make-closure-counter-independent-instances nil nil nil
     (let* ((make-counter (lambda ()
                            (let ((n 0))
                              (lambda () (setq n (1+ n))))))
            (c1 (funcall make-counter))
            (c2 (funcall make-counter)))
       (list (funcall c1) (funcall c1) (funcall c1) (funcall c2))))
    (make-closure-dolist-capture nil nil nil
     (let ((fns nil))
       (dolist (x '(1 2 3))
         (push (lambda () x) fns))
       (mapcar #'funcall (nreverse fns))))
    (make-closure-shared-mutation nil nil nil
     (let* ((n 0)
            (inc (lambda () (setq n (1+ n))))
            (get (lambda () n)))
       (funcall inc) (funcall inc)
       (funcall get)))
    (make-closure-direct nil nil nil
     (funcall (make-closure (make-byte-code 257 "\300\1\\\207" [nil] 3) 41) 1))
    (make-closure-wrong-type nil nil nil
     (condition-case e (make-closure 5 1 2)
       (error (list (car e) (cadr e)))))
    (make-closure-too-many-vars nil nil nil
     (condition-case e
         (make-closure (make-byte-code 257 "\207" [] 1) 1 2 3)
       (error (error-message-string e))))
    (aset-vector nil nil nil
     (let ((v (vector 1 2 3)))
       (byte-code (unibyte-string 192 193 194 73 135) (vector v 1 99) 4)
       v))
    (aset-string nil nil nil
     (let ((s (copy-sequence "abc")))
       (byte-code (unibyte-string 192 193 194 73 135) (vector s 1 ?X) 4)
       s))
    (aset-fset-override nil nil nil
     (let ((old (symbol-function 'aset)))
       (unwind-protect
           (progn (fset 'aset (lambda (&rest _) 'overridden))
                  (byte-code (unibyte-string 192 193 194 73 135)
                             (vector (vector 1 2 3) 1 99) 4))
         (fset 'aset old))))
    ;; aset's error paths are a KNOWN, PRE-EXISTING gap in the native
    ;; `aset' builtin itself, not in this opcode: host signals
    ;; wrong-type-argument/args-out-of-range for a non-array or an
    ;; out-of-bounds index, but the standalone native `aset' silently
    ;; returns the value with no validation at all -- reproduces
    ;; identically calling `aset' directly, with no byte-code involved.
    ;; Opcode 73 correctly delegates via the same (builtin aset)
    ;; OPCODE-STATIC dispatch every sibling opcode uses; fixing the
    ;; validation itself is native-ABI work (bf_aset), out of this
    ;; lane's file scope. See these two known-gap cases in
    ;; nelisp-bytecode-opcode-parity-edge-forms below.
    (substring (192 193 194 79 135) ["abcdef" 1 4] 4)
    (substring-type-error nil nil nil
     (condition-case e (byte-code (unibyte-string 192 193 194 79 135) '[7 0 1] 4)
       (error (list (car e) (cadr e)))))
    (assq-hit (192 193 158 135) [key ((key . value))] 3)
    (assq-type-error nil nil nil
     (condition-case e (byte-code (unibyte-string 192 193 158 135) '[key 1] 3)
       (error (list (car e) (cadr e)))))
    (aref-fset-override nil nil nil
     (let ((old (symbol-function 'aref)))
       (unwind-protect
           (progn (fset 'aref (lambda (&rest _) 'shadow))
                  (byte-code (unibyte-string 192 193 72 135) '[[alpha beta] 1] 3))
         (fset 'aref old))))
    (substring-fset-override nil nil nil
     (let ((old (symbol-function 'substring)))
       (unwind-protect
           (progn (fset 'substring (lambda (&rest _) 'shadow))
                  (byte-code (unibyte-string 192 193 194 79 135) '["abcdef" 1 4] 4))
         (fset 'substring old))))
    (assq-fset-override nil nil nil
     (let ((old (symbol-function 'assq)))
       (unwind-protect
           (progn (fset 'assq (lambda (&rest _) 'shadow))
                  (byte-code (unibyte-string 192 193 158 135) '[key ((key . value))] 3))
         (fset 'assq old))))
    ;; Opcode 75 (Bsymbol_function): plain CHECK_SYMBOL then return the
    ;; symbol's raw function-cell contents (nil, an alias symbol, an
    ;; autoload list, or a real function -- never resolved further, never
    ;; a void-function signal). OPCODE-STATIC dispatch via (builtin
    ;; symbol-function) means the opcode reads whatever the ARGUMENT
    ;; symbol's own cell currently holds while staying immune to a later
    ;; fset of the symbol symbol-function itself.
    (symbol-function-normal (192 75 135) [car] 2)
    (symbol-function-void (192 75 135) [nl-parity-void-fn-xyz] 2)
    (symbol-function-non-symbol-error nil nil nil
     (condition-case e (byte-code (unibyte-string 192 75 135) '[5] 2)
       (error (list (car e) (cadr e)))))
    (symbol-function-fset-override nil nil nil
     (let ((old (symbol-function 'symbol-function)))
       (unwind-protect
           (progn (fset 'symbol-function (lambda (&rest _) 'overridden))
                  (byte-code (unibyte-string 192 75 135) '[car] 2))
         (fset 'symbol-function old))))
    (nreverse (192 159 135) [(1 2 3)] 2)
    (setcdr (192 193 161 135) [(old . nil) new] 3)
    (rem (192 193 166 135) [-7 3] 3)
    (list3 (192 193 194 69 135) [a b c] 4)
    (list3-alias nil nil nil
     (let ((cell (cons 'inside 'tail)))
       (eq (nth 1 (byte-code (unibyte-string 192 193 194 69 135)
                             (vector 'left cell 'right) 4)) cell)))
    (list4 (192 193 194 195 70 135) [a b c d] 5)
    (list4-alias nil nil nil
     (let ((cell (cons 'inside 'tail)))
       (eq (nth 2 (byte-code (unibyte-string 192 193 194 195 70 135)
                             (vector 'one 'two cell 'four) 5)) cell)))
    (listN (192 193 194 195 196 175 5 135) [a b c d e] 6)
    (listN-alias nil nil nil
     (let ((cell (cons 'inside 'tail)))
       (eq (nth 3 (byte-code (unibyte-string 192 193 194 195 196 175 5 135)
                             (vector 'one 'two 'three cell 'five) 6))
           cell)))
    (listN-zero nil nil nil
     (byte-code (unibyte-string 175 0 135) [] 1))
    (string-equal-true (192 193 152 135) ["same" "same"] 3)
    (string-equal-false (192 193 152 135) ["same" "other"] 3)
    (string-equal-symbol (192 193 152 135) [name "name"] 3)
    (string-equal-type-error nil nil nil
     (condition-case e
         (byte-code (unibyte-string 192 193 152 135) '[7 "s"] 3)
       (error (list (car e) (cadr e)))))
    (string-less-true (192 193 153 135) ["a" "b"] 3)
    (string-less-false (192 193 153 135) ["b" "a"] 3)
    (string-less-type-error nil nil nil
     (condition-case e
         (byte-code (unibyte-string 192 193 153 135) '["s" 7] 3)
       (error (list (car e) (cadr e)))))
    (string-equal-fset-override nil nil nil
     (let ((old (symbol-function 'string=)))
       (unwind-protect
           (progn (fset 'string= (lambda (&rest _) nil))
                  (byte-code (unibyte-string 192 193 152 135) '["same" "same"] 3))
         (fset 'string= old))))
    (string-less-fset-override nil nil nil
     (let ((old (symbol-function 'string<)))
       (unwind-protect
           (progn (fset 'string< (lambda (&rest _) nil))
                  (byte-code (unibyte-string 192 193 153 135) '["a" "b"] 3))
         (fset 'string< old))))
    (nconc-proper (192 193 164 135) [(a b) (c d)] 3)
    (nconc-nil-first (192 193 164 135) [nil (c d)] 3)
    (nconc-dotted-first (192 193 164 135) [(a . old) (c)] 3)
    (nconc-type-error nil nil nil
     (condition-case e
         (byte-code (unibyte-string 192 193 164 135) '[7 (c)] 3)
       (error (list (car e) (cadr e)))))
    (nconc-alias nil nil nil
     (let ((cell (list 'a)))
       (let ((result (byte-code (unibyte-string 192 192 164 135)
                                (vector cell) 3)))
         (and (eq result cell) (eq (cdr cell) cell)))))
    (nconc-fset-override nil nil nil
     (let ((old (symbol-function 'nconc)))
       (unwind-protect
           (progn (fset 'nconc (lambda (&rest _) 'shadow))
                  (byte-code (unibyte-string 192 193 164 135) [(a) (b)] 3))
         (fset 'nconc old))))
    (varset (193 24 194 16 8 41 135) [bytecode-x 1 2] 2))
  "One focused case per requested opcode, in implementation order.")

(defvar nelisp-catch-parity-dynamic nil
  "Special variable used to check binding restoration across bytecode THROW.")

(defconst nelisp-bytecode-regression-forms
  '((plus . (+ 1 2))
    (if . (if (< 3 2) 'yes 'no))
    (string . "abc")
    (negative-plus . (+ -5 3))
    (loop-five . (let ((i 0))
                   (while (< i 5) (setq i (1+ i))) i))
    (loop-sum . (let ((s 0) (i 0))
                  (while (< i 10)
                    (setq s (+ s i)) (setq i (1+ i))) s))
    (carry-symbol . (let ((i 0) (x 'carried-symbol))
                      (while (< i 2) (setq i (1+ i))) x))
    (carry-string . (let ((i 0) (x "carried-string"))
                      (while (< i 2) (setq i (1+ i))) x))
    (carry-cons . (let ((i 0) (x '(left . right)))
                    (while (< i 2) (setq i (1+ i))) x))
    (carry-list . (let ((i 0) (x '(left right)))
                    (while (< i 2) (setq i (1+ i))) x))
    (catch-normal . (catch 'k 17))
    (catch-throw . (catch 'k (throw 'k 17)))
    (catch-inner . (catch 'outer (catch 'inner (throw 'inner 19))))
    (catch-outer . (catch 'outer (catch 'inner (throw 'outer 23))))
    (catch-error-propagates . (catch 'k (car 1)))
    (catch-unbinds-special .
     (let ((nelisp-catch-parity-dynamic 1))
       (list (catch 'k
               (let ((nelisp-catch-parity-dynamic 2))
                 (throw 'k nelisp-catch-parity-dynamic)))
             nelisp-catch-parity-dynamic))))
  "Pre-existing byte-code values and loop-root regressions.")

(defconst nelisp-bytecode-opcode-parity-edge-forms
  '((nreverse-nil
     (byte-code (unibyte-string 192 159 135) [nil] 2))
    (nreverse-list-alias
     (let* ((xs (list 1 2 3))
            (result (byte-code (unibyte-string 192 159 135) (vector xs) 2)))
       (list result xs)))
    (nreverse-vector-alias
     (let* ((xs (vector 1 2 3))
            (result (byte-code (unibyte-string 192 159 135) (vector xs) 2)))
       (list result xs)))
    (nreverse-string-copy
     (let* ((xs "ab")
            (result (byte-code (unibyte-string 192 159 135) (vector xs) 2)))
       (list result xs)))
    (nreverse-multibyte-string
     (let* ((xs "あbc")
            (result (byte-code (unibyte-string 192 159 135) (vector xs) 2)))
       (list result xs)))
    (nreverse-empty-string
     (byte-code (unibyte-string 192 159 135) [""] 2))
    (nreverse-bool-vector
     (let* ((xs (bool-vector t nil t nil))
            (result (byte-code (unibyte-string 192 159 135) (vector xs) 2)))
       (list (eq result xs) (aref result 0) (aref result 1)
             (aref result 2) (aref result 3))))
    (setcdr-alias
     (let* ((cell (cons 'old nil))
            (result (byte-code (unibyte-string 192 193 161 135)
                               (vector cell 'new) 3)))
       (list result cell)))
    (nreverse-circular-list
     (let ((xs (list 1 2)))
       (setcdr (cdr xs) xs)
       (condition-case err
           (byte-code (unibyte-string 192 159 135) (vector xs) 2)
         (error (car err)))))
    (nreverse-circular-tail
     (let ((xs (list 1 2 3)))
       (setcdr (last xs) (cdr xs))
       (condition-case err
           (byte-code (unibyte-string 192 159 135) (vector xs) 2)
         (error (car err)))))
    (nreverse-wrong-type
     (condition-case err
         (byte-code (unibyte-string 192 159 135) [42] 2)
       (error (list (car err) (cadr err) (caddr err)))))
    (nreverse-improper-list
     (let ((xs (cons 1 2)))
       (condition-case err
           (byte-code (unibyte-string 192 159 135) (vector xs) 2)
         (error (list (car err) (cadr err) (caddr err))))))
    (nreverse-function-cell-override
     (let ((original (symbol-function 'nreverse)))
       (unwind-protect
           (progn
             (fset 'nreverse (lambda (_value) 'overridden))
             (byte-code (unibyte-string 192 159 135)
                        (vector (list 'a 'b)) 2))
         (fset 'nreverse original))))
    (concat2-properties-known-gap
     (let ((text (propertize "a" 'face 'bold)))
       (byte-code (unibyte-string 192 193 80 135) (vector text "b") 3)))
    (concat3-properties-known-gap
     (let ((text (propertize "a" 'face 'bold)))
       (byte-code (unibyte-string 192 193 194 81 135)
                  (vector text "b" "c") 4)))
    (max-bignum-float-known-gap
     (condition-case e
         (byte-code (unibyte-string 192 193 93 135)
                    [2305843009213693952 1.0e50] 3)
       (error (list (car e) (cadr e)))))
    (min-bignum-float-known-gap
     (condition-case e
         (byte-code (unibyte-string 192 193 94 135)
                    [2305843009213693952 1.0e50] 3)
       (error (list (car e) (cadr e)))))
    ;; Bcall (opcode 32/base) invoking a callee that THROWs to a tag whose
    ;; `catch' is OUTSIDE this top-level `byte-code' call, not inside it: the
    ;; throw crosses the byte-code frame the same way it would cross any
    ;; other function-call frame. FIXED (this session): the top-level
    ;; "byte-code" native entry point's unhandled-exit path used to force-
    ;; write the pending-exit-kind flag to 1 (plain signal) even when it was
    ;; 2 (throw) before re-signalling, losing the tag/value pair the outer
    ;; `catch' needed and surfacing an uncaught top-level error instead. Now
    ;; a pending flag of 2 is left untouched (see scripts/nelisp-standalone-
    ;; build.el's "byte-code" native-ABI entry, ~18493); every other pending
    ;; kind still defaults to 1, unchanged. This is a normal case, not a
    ;; known-gap, as of that fix -- both sides return 77.
    (call-throw-escapes-bytecode
     (catch 'nl-parity-tag
       (byte-code (unibyte-string 192 32 135)
                  (vector (lambda () (throw 'nl-parity-tag 77))) 2)
       'not-reached))
    ;; Bunwind-protect (opcode 142) registers the popped cleanup form/
    ;; function via the same specpdl-style unwind stack a dynamic `let'
    ;; binding uses; base 40 (Bunbind) later pops it either way. A THROW
    ;; from the protected body must still run the cleanup, then keep
    ;; propagating -- the result is the throw's value, not the cleanup's.
    (unwind-protect-cleanup-runs-on-throw
     (catch 'nl-parity-unwind-tag
       (byte-code (unibyte-string 192 142 193 194 195 34 41 135)
                  (vector (lambda () 99) 'throw 'nl-parity-unwind-tag 42) 3)))
    ;; Falling through normally: real GNU Emacs 31.1 byte-compiles this
    ;; exact `(unwind-protect BODY (setq X t))' shape (dynamic-binding
    ;; source, matching how this whole codebase compiles the general
    ;; prelude) to running the cleanup's SIDE EFFECT via the Bunbind that
    ;; follows Bunwind-protect on the fall-through path, same as it does
    ;; for the throw case above -- verified equal on host and standalone.
    (unwind-protect-cleanup-runs-on-fallthrough
     (let (nl-parity-unwind-marker)
       (byte-code (unibyte-string 192 142 41 193 135)
                  (vector (lambda () (setq nl-parity-unwind-marker t)) 7) 2)
       nl-parity-unwind-marker))
    ;; Bcall of a symbol whose function cell is the low-level autoload
    ;; placeholder `(autoload FILE ...)`, pointing at a file that does not
    ;; exist. Host resolves the placeholder, attempts to load the file, and
    ;; signals `file-missing'. Standalone's generic call path does not
    ;; recognise the `(autoload ...)' list-headed-by-that-symbol shape as a
    ;; load trigger and instead tries to invoke it as a function value,
    ;; signalling `invalid-function' instead. Both sides succeed in
    ;; signalling AN error (status 0 under this condition-case), just a
    ;; different one -- tracked here, not in wf_bytecode_call_gateway's
    ;; scope (autoload resolution is native-ABI function-cell dispatch).
    (call-autoload-missing-file-known-gap
     (let ((sym (make-symbol "nl-parity-autoload-fn")))
       (fset sym '(autoload "nl-parity-nonexistent-file-xyz" nil nil nil))
       (condition-case e
           (byte-code (unibyte-string 192 32 135) (vector sym) 2)
         (error (car e)))))
    ;; Native `aset' validation gap (see the comment beside aset-vector
    ;; above): host signals, standalone silently returns the value.
    (aset-wrong-type-known-gap
     (condition-case e
         (byte-code (unibyte-string 192 193 194 73 135) (vector 5 1 9) 4)
       (error (list (car e) (cadr e)))))
    (aset-out-of-range-known-gap
     (condition-case e
         (byte-code (unibyte-string 192 193 194 73 135)
                    (vector (vector 1 2 3) 9 5) 4)
       (error (list (car e) (cadr e)))))
    (setcdr-wrong-type
     (condition-case err
         (byte-code (unibyte-string 192 193 161 135) [42 new] 3)
       (error (list (car err) (cadr err) (caddr err)))))
    (rem-wrong-type
     (condition-case err
         (byte-code (unibyte-string 192 193 166 135) [1.5 2] 3)
       (error (list (car err) (cadr err) (caddr err)))))
    (rem-zero
     (condition-case err
         (byte-code (unibyte-string 192 193 166 135) [7 0] 3)
       (error (list (car err) (cadr err) (caddr err)))))
    (rem-signs
     (list (byte-code (unibyte-string 192 193 166 135) [-7 3] 3)
           (byte-code (unibyte-string 192 193 166 135) [7 -3] 3))))
  "Host differential cases for bytecode mutation and argument failures.")

(defun nelisp-bytecode-opcode-parity-run ()
  (let* ((root (nelisp-bytecode-corpus--root))
         (default-directory root)
         (output (expand-file-name "target/bytecode-opcode-parity/" root))
         (standalone (expand-file-name (or (getenv "NELISP_BIN")
                                           "target/nelisp") root))
         (host (or (getenv "NELISP_EMACS")
                   (expand-file-name invocation-name invocation-directory)))
         (passed 0) (edge-passed 0) (known-gap-passed 0)
         (regression-passed 0) failures)
    (make-directory output t)
    (cl-loop
     for case in nelisp-bytecode-opcode-parity-cases
     for index from 1
     for name = (nth 0 case)
     for bytes = (nth 1 case)
     for constants = (nth 2 case)
     for depth = (nth 3 case)
     for form = (or (nth 4 case)
                    `(byte-code (unibyte-string ,@bytes) ',constants ,depth))
     for printed = (prin1-to-string form)
     for stem = (expand-file-name (format "%02d-%s" index name) output)
     for host-out = (concat stem ".host.out")
     for host-err = (concat stem ".host.err")
     for standalone-out = (concat stem ".standalone.out")
     for standalone-err = (concat stem ".standalone.err")
     for host-status = (nelisp-bytecode-corpus--run
                        host (list "--batch" "-Q" "--eval"
                                   (format "(prin1 %s)" printed))
                        host-out host-err)
     for standalone-status = (nelisp-bytecode-corpus--run
                              standalone (list "--eval" printed)
                              standalone-out standalone-err)
     for host-value = (nelisp-bytecode-corpus--result-text host-out)
     for standalone-value = (nelisp-bytecode-corpus--result-text
                              standalone-out)
     do
     (nelisp-bytecode-corpus--write (concat stem ".expr") printed)
     (nelisp-bytecode-corpus--write
      (concat stem ".status")
      (format "host=%s\nstandalone=%s\n" host-status standalone-status))
     (if (and (equal host-status 0) (equal standalone-status 0)
              (equal host-value standalone-value))
         (cl-incf passed)
       (push (list name host-status standalone-status
                   host-value standalone-value)
             failures)))
    (cl-loop
     for case in nelisp-bytecode-opcode-parity-edge-forms
     for index from 1
     for name = (car case)
     for form = (cadr case)
     for printed = (prin1-to-string form)
     for stem = (expand-file-name (format "edge-%02d-%s" index name) output)
     for host-out = (concat stem ".host.out")
     for host-err = (concat stem ".host.err")
     for standalone-out = (concat stem ".standalone.out")
     for standalone-err = (concat stem ".standalone.err")
     for host-status = (nelisp-bytecode-corpus--run
                        host (list "--batch" "-Q" "--eval"
                                   (format "(prin1 %s)" printed))
                        host-out host-err)
     for standalone-status = (nelisp-bytecode-corpus--run
                              standalone (list "--eval" printed)
                              standalone-out standalone-err)
     for host-value = (nelisp-bytecode-corpus--result-text host-out)
     for standalone-value = (nelisp-bytecode-corpus--result-text standalone-out)
     do
     (nelisp-bytecode-corpus--write (concat stem ".expr") printed)
     (nelisp-bytecode-corpus--write
      (concat stem ".status")
      (format "host=%s\nstandalone=%s\n" host-status standalone-status))
     (if (string-match-p "-known-gap\\'" (symbol-name name))
         (if (cond
              ((memq name '(concat2-properties-known-gap
                            concat3-properties-known-gap
                            max-bignum-float-known-gap
                            min-bignum-float-known-gap
                            call-autoload-missing-file-known-gap
                            aset-wrong-type-known-gap
                            aset-out-of-range-known-gap))
               (and (equal host-status 0) (equal standalone-status 0)
                    (not (equal host-value standalone-value))))
              (t
               (and (equal host-status 0) (equal standalone-status 1)
                    (with-temp-buffer
                      (insert-file-contents standalone-err)
                      (search-forward "circular-list" nil t)))))
             (cl-incf known-gap-passed)
           (push (list name host-status standalone-status
                       host-value standalone-value)
                 failures))
       (if (and (equal host-status 0) (equal standalone-status 0)
                (equal host-value standalone-value))
           (cl-incf edge-passed)
         (push (list name host-status standalone-status
                     host-value standalone-value)
               failures))))
    (cl-loop
     for case in nelisp-bytecode-regression-forms
     for index from 1
     for name = (car case)
     for object = (byte-compile `(lambda () ,(cdr case)))
     for form = (nelisp-bytecode-corpus--call-form object)
     for printed = (let ((print-escape-newlines t)
                         (print-escape-control-characters t)
                         (print-escape-nonascii t))
                     (prin1-to-string form))
     for stem = (expand-file-name (format "reg-%02d-%s" index name) output)
     for host-out = (concat stem ".host.out")
     for host-err = (concat stem ".host.err")
     for standalone-out = (concat stem ".standalone.out")
     for standalone-err = (concat stem ".standalone.err")
     for host-status = (nelisp-bytecode-corpus--run
                        host (list "--batch" "-Q" "--eval"
                                   (format "(prin1 %s)" printed))
                        host-out host-err)
     for standalone-status = (nelisp-bytecode-corpus--run
                              standalone (list "--eval" printed)
                              standalone-out standalone-err)
     for host-value = (nelisp-bytecode-corpus--result-text host-out)
     for standalone-value = (nelisp-bytecode-corpus--result-text
                              standalone-out)
     do
     (nelisp-bytecode-corpus--write (concat stem ".expr") printed)
     (nelisp-bytecode-corpus--write
      (concat stem ".status")
      (format "host=%s\nstandalone=%s\n" host-status standalone-status))
     (if (or (and (equal host-status 0) (equal standalone-status 0)
                  (equal host-value standalone-value))
             (and (eq name 'catch-error-propagates)
                  (not (equal host-status 0))
                  (not (equal standalone-status 0))
                  (let ((host-error
                         (concat (nelisp-bytecode-corpus--result-text host-out)
                                 (nelisp-bytecode-corpus--result-text host-err)))
                        (standalone-error
                         (concat
                          (nelisp-bytecode-corpus--result-text standalone-out)
                          (nelisp-bytecode-corpus--result-text standalone-err))))
                    (and (string-match-p "wrong-type-argument" host-error)
                         (string-match-p "wrong-type-argument" standalone-error)
                         (string-match-p "(listp 1)" host-error)
                         (string-match-p "(listp 1)" standalone-error)))))
         (cl-incf regression-passed)
       (push (list name host-status standalone-status
                   host-value standalone-value)
             failures)))
    (setq failures (nreverse failures))
    (let ((report (expand-file-name "report.txt" output)))
      (with-temp-file report
        (insert (format "opcode-cases=%d\nopcode-passed=%d\n"
                        (length nelisp-bytecode-opcode-parity-cases)
                        passed))
        (insert (format "edge-cases=%d\nedge-passed=%d\n"
                        (- (length nelisp-bytecode-opcode-parity-edge-forms) 7)
                        edge-passed))
        (insert (format "known-gap-cases=7\nknown-gap-confirmed=%d\n"
                        known-gap-passed))
        (insert (format "regression-cases=%d\nregression-passed=%d\nfailed=%d\n"
                        (length nelisp-bytecode-regression-forms)
                        regression-passed (length failures)))
        (dolist (row failures)
          (insert (format "%S host-status=%S standalone-status=%S host=%S standalone=%S\n"
                          (nth 0 row) (nth 1 row) (nth 2 row)
                          (nth 3 row) (nth 4 row)))))
      (princ (with-temp-buffer
               (insert-file-contents report)
               (buffer-string))))))

(provide 'nelisp-bytecode-opcode-parity)
;;; nelisp-bytecode-opcode-parity.el ends here
