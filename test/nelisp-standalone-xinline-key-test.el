;;; nelisp-standalone-xinline-key-test.el --- xinline used-entries digest and CG7 on hits -*- lexical-binding: t; -*-

;; Run: emacs --batch -Q -L scripts -L lisp -L src -l test/nelisp-standalone-xinline-key-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defmacro nl-xk--with-table (&rest body)
  `(let ((nelisp-standalone--xinline-table (make-hash-table :test #'eq))
         (nelisp-standalone--xinline-used (make-hash-table :test #'eq)))
     (nelisp-standalone--xinline-register
      '(seq (defun h_used (a) (+ a 1)) (defun h_unused (a) (+ a 2)) (defun h_two (a b) (+ a b))))
     ,@body))

(defconst nl-xk--src '(seq (defun caller (x) (h_used (h_two x 1))) (defun own_fn (x) x)))

(ert-deftest nl-xk-digest-covers-used-entries-only ()
  (nl-xk--with-table
   (should (equal '(h_two h_used) (nelisp-standalone--xinline-used-entries nl-xk--src)))
   (let ((base (nelisp-standalone--xinline-digest nl-xk--src))
         (whole (nelisp-standalone--xinline-digest)))
     ;; An entry the unit never calls: no rekey (negative control: the whole-table digest moves).
     (puthash 'h_unused '((a) . (+ a 99)) nelisp-standalone--xinline-table)
     (should (equal base (nelisp-standalone--xinline-digest nl-xk--src)))
     (should-not (equal whole (nelisp-standalone--xinline-digest)))
     ;; A used entry changes: rekey.
     (puthash 'h_used '((a) . (+ a 7)) nelisp-standalone--xinline-table)
     (should-not (equal base (nelisp-standalone--xinline-digest nl-xk--src)))
     ;; A new entry that the unit calls (absent before): rekey.
     (let ((src2 '(seq (defun caller (x) (h_used (h_new x))))))
       (let ((b2 (nelisp-standalone--xinline-digest src2)))
         (puthash 'h_new '((a) . (+ a 5)) nelisp-standalone--xinline-table)
         (should-not (equal b2 (nelisp-standalone--xinline-digest src2))))))))

(ert-deftest nl-xk-used-entries-match-the-expander ()
  "The key's used set is exactly what a compile marks (same walk)."
  (nl-xk--with-table
   (dolist (src (list nl-xk--src
                      '(seq (defun c (x) (h_used '(h_two 1 2)) (h_two x)))   ; quote and arity mismatch
                      '(seq (defun h_used (a) a) (defun c (x) (h_used x)))   ; own definition wins
                      '(seq (defun c (x) (h_two (data-addr h_used) x)))))
     (clrhash nelisp-standalone--xinline-used)
     (nelisp-standalone--xinline-expand src)
     (let (marked)
       (maphash (lambda (k _) (push k marked)) nelisp-standalone--xinline-used)
       (should (equal (sort marked (lambda (a b) (string< a b)))
                      (nelisp-standalone--xinline-used-entries src)))))))

(ert-deftest nl-xk-cg7-fires-after-a-cache-hit ()
  (nl-xk--with-table
   ;; A hit marks the entries its unit would have inlined; no expansion happens.
   (nelisp-standalone--xinline-mark-used nl-xk--src)
   (should (gethash 'h_used nelisp-standalone--xinline-used))
   (should-error (nelisp-standalone--xinline-register '(seq (defun h_used (a) (+ a 100))))
                 :type 'error)))

(ert-deftest nl-xk-cg7-hit-negative-control ()
  (nl-xk--with-table
   (nelisp-standalone--xinline-mark-used nl-xk--src)
   ;; Redefining a helper the unit does NOT use is not an error.
   (nelisp-standalone--xinline-register '(seq (defun h_unused (a) (+ a 100))))
   (should (eq 'conflict (gethash 'h_unused nelisp-standalone--xinline-table)))
   ;; Without the hit marking, the redefinition of h_used is silent (old behaviour).
   (clrhash nelisp-standalone--xinline-used)
   (nelisp-standalone--xinline-register '(seq (defun h_used (a) (+ a 100))))
   (should (eq 'conflict (gethash 'h_used nelisp-standalone--xinline-table)))))

(ert-deftest nl-xk-cached-unit-hit-marks-used ()
  "The hit branch of `--cached-unit' calls the marker (and a miss does not need to)."
  (let* ((dir (make-temp-file "nl-xk" t)) (marks 0) (compiled 0)
         (nelisp-standalone--toolchain-digest "fixed")
         (nelisp-standalone--recompiled nil))
    (unwind-protect
        (nl-xk--with-table
         (cl-letf (((symbol-function 'nelisp-standalone--target-cache-dir) (lambda () dir))
                   ((symbol-function 'nelisp-standalone--target-object-name) #'identity)
                   ((symbol-function 'nelisp-standalone--prewarm-wait) #'ignore)
                   ((symbol-function 'nelisp-standalone--unit-inline-imports) #'ignore)
                   ((symbol-function 'nelisp-standalone--xinline-prepare) #'ignore)
                   ((symbol-function 'nelisp-standalone--compile-to-unit)
                    (lambda (name source) (cl-incf compiled) (list name source)))
                   ((symbol-function 'nelisp-standalone--unit-cache-encode) #'identity)
                   ((symbol-function 'nelisp-standalone--unit-cache-decode) #'identity)
                   ((symbol-function 'nelisp-standalone--xinline-enabled-p) (lambda () t)))
           (advice-add 'nelisp-standalone--xinline-mark-used :before (lambda (&rest _) (cl-incf marks)) '((name . nl-xk)))
           (unwind-protect
               (progn
                 (nelisp-standalone--cached-unit "t.o" nl-xk--src)
                 (should (= compiled 1)) (should (= marks 0))
                 (nelisp-standalone--cached-unit "t.o" nl-xk--src)
                 (should (= compiled 1)) (should (= marks 1)))
             (advice-remove 'nelisp-standalone--xinline-mark-used 'nl-xk))))
      (delete-directory dir t))))

(provide 'nelisp-standalone-xinline-key-test)
