;;; eqhash-semantics.el --- GNU 31.1 hash-table oracle -*- lexical-binding: t; -*-

;; Run with -Q --batch -l tools/ai/eqhash-semantics.el in GNU Emacs, or
;; load the same file in NeLisp.  Stdout is the comparison protocol:
;; one (eqhash LABEL VALUE) form per case and a final count marker.
;; The adjacent .expected file is real GNU 31.1 output, not hand-written.
;; Regenerate from the repository root with the GNU 31.1 executable:
;;   (ulimit -v 8388608; emacs -Q --batch -l tools/ai/eqhash-semantics.el) \
;;     > tools/ai/eqhash-semantics.expected
;;   cp tools/ai/eqhash-semantics.expected test/standalone-eqhash-check.expected
;; Same-valued float `eq' identity is a pre-existing scalar-representation
;; limitation outside hash-table scope.  The eq numeric case uses distinct
;; float values; eql/equal still exercise duplicate floating-point values.

(defvar eqhash--count 0)
(defvar eqhash--weak-state nil)
(defvar eqhash--ordinary-only nil)

(defun eqhash--emit (label value)
  (setq eqhash--count (1+ eqhash--count))
  (prin1 (list 'eqhash label value))
  (terpri))

(defmacro eqhash--case (label &rest body)
  `(eqhash--emit ',label
                 (condition-case err
                     (progn ,@body)
                   (error (list 'unexpected-error err)))))

(defun eqhash--caught (function)
  (condition-case err (list 'returned (funcall function))
    (error err)))

(defun eqhash--entries (table)
  (let (entries)
    (maphash (lambda (key value)
               (setq entries (cons (list key value) entries)))
             table)
    (nreverse entries)))

(defun eqhash--collect ()
  ;; A second cycle also checks that the first sweep left no stale entry.
  (garbage-collect)
  (garbage-collect))

(dolist (test '(eq eql equal))
  (eqhash--emit
   (intern (concat "strings-" (symbol-name test)))
   (let* ((a (copy-sequence "same"))
          (b (copy-sequence "same"))
          (probe (copy-sequence "same"))
          (table (make-hash-table :test test :size 1)))
     (puthash a 'first table)
     (puthash b 'second table)
     (list (eq a b) (equal a b) (hash-table-test table)
           (hash-table-count table) (gethash a table 'missing)
           (gethash b table 'missing) (gethash probe table 'missing)))))

(dolist (test '(eq eql equal))
  (eqhash--emit
   (intern (concat "numbers-" (symbol-name test)))
   (let* ((f1 (string-to-number "1.25"))
          (f2 (string-to-number (if (eq test 'eq) "2.5" "1.25")))
          (b1 (string-to-number "100000000000000000000"))
          (b2 (string-to-number "100000000000000000000"))
          (table (make-hash-table :test test)))
     (puthash f1 'float-first table)
     (puthash f2 'float-second table)
     (puthash b1 'big-first table)
     (puthash b2 'big-second table)
     (list (eql f1 f2) (eq b1 b2) (hash-table-count table)
           (gethash f1 table) (gethash f2 table)
           (gethash b1 table) (gethash b2 table)))))

(eqhash--case identity-types
  (let* ((keys (vector (copy-sequence "key") (list 'key)
                       (make-symbol "key") (vector 'key)
                       (string-to-number "1.25")
                       (string-to-number "100000000000000000000")
                       (make-marker)))
         (table (make-hash-table :test 'eq :size 1))
         (i 0) values)
    (while (< i (length keys))
      (puthash (aref keys i) i table)
      (setq i (1+ i)))
    (eqhash--collect)
    (setq i 0)
    (while (< i (length keys))
      (setq values (cons (gethash (aref keys i) table 'missing) values))
      (setq i (1+ i)))
    (list (hash-table-count table) (nreverse values))))

(dolist (test '(eq eql))
  (eqhash--emit
   (intern (concat "growth-" (symbol-name test)))
   (let ((keys (make-vector 257 nil))
         (table (make-hash-table :test test :size 1))
         (i 0) (hits 0) (sum 0) (visited 0) (mapped-sum 0))
     (while (< i 257)
       (let ((key (copy-sequence "identical-text")))
         (aset keys i key)
         (puthash key i table))
       (setq i (1+ i)))
     (eqhash--collect)
     (setq i 0)
     (while (< i 257)
       (let ((value (gethash (aref keys i) table 'missing)))
         (when (equal value i) (setq hits (1+ hits)))
         (when (numberp value) (setq sum (+ sum value))))
       (setq i (1+ i)))
     (maphash (lambda (_key value)
                (setq visited (1+ visited) mapped-sum (+ mapped-sum value)))
              table)
     (list (hash-table-count table) hits sum visited mapped-sum
           (gethash (copy-sequence "identical-text") table 'missing)))))

(let ((table (make-hash-table :test 'eq :size 1)))
  (dolist (pair '((alpha . 1) (beta . 2) (gamma . 3) (delta . 4)))
    (puthash (car pair) (cdr pair) table))
  (eqhash--case maphash-initial (eqhash--entries table))
  (remhash 'beta table)
  (puthash 'epsilon 5 table)
  (eqhash--case maphash-reused-slot (eqhash--entries table))
  (puthash 'gamma 30 table)
  (eqhash--case maphash-update (eqhash--entries table))
  (eqhash--case maphash-return
    (maphash (lambda (_key _value) 'ignored) table))
  (eqhash--case copy-metadata
    (let ((copy (copy-hash-table table)))
      (list (eq copy table) (hash-table-test copy)
            (hash-table-weakness copy) (hash-table-count copy)
            (eqhash--entries copy))))
  (eqhash--case copy-independent
    (let ((copy (copy-hash-table table)))
      (puthash 'alpha 'changed copy)
      (remhash 'delta copy)
      (list (gethash 'alpha table) (gethash 'alpha copy)
            (hash-table-count table) (hash-table-count copy))))
  (eqhash--case remove-clear
    (let ((removed (remhash 'alpha table)))
      (clrhash table)
      (list removed (hash-table-count table) (eqhash--entries table)))))

(eqhash--case copy-shallow
  (let* ((key (copy-sequence "key")) (value (list 'value))
         (table (make-hash-table :test 'eq :weakness 'key)))
    (puthash key value table)
    (let ((copy (copy-hash-table table)))
      (list (hash-table-test copy) (hash-table-weakness copy)
            (eq value (gethash key copy))
            (gethash (copy-sequence "key") copy 'missing)))))

(unless eqhash--ordinary-only
(defun eqhash--setup-weak (kind)
  (let ((table (make-hash-table :test 'eq :weakness kind :size 1))
        (roots (make-vector 3 nil)) (i 0))
    (while (< i 4)
      (let ((key (list i)) (value (vector i)))
        (puthash key value table)
        (cond ((= i 0) (aset roots 0 (cons key value)))
              ((= i 1) (aset roots 1 key))
              ((= i 2) (aset roots 2 value))))
      (setq i (1+ i)))
    (vector table roots)))

(defun eqhash--weak-result ()
  (let ((table (aref eqhash--weak-state 0)) entries)
    (maphash (lambda (key value)
               (setq entries (cons (list (car key) (aref value 0)) entries)))
             table)
    (list (hash-table-weakness table) (hash-table-count table)
          (nreverse entries))))

(dolist (kind '(key value key-or-value key-and-value))
  (setq eqhash--weak-state (eqhash--setup-weak kind))
  (eqhash--collect)
  (eqhash--emit (intern (concat "weak-" (symbol-name kind)))
                (eqhash--weak-result))
  (aset eqhash--weak-state 1 nil)
  (eqhash--collect)
  (eqhash--emit (intern (concat "weak-unrooted-" (symbol-name kind)))
                (eqhash--weak-result)))

(defun eqhash--setup-weak-strings ()
  (let ((table (make-hash-table :test 'eq :weakness 'key :size 1))
        (a (copy-sequence "a")) (b (copy-sequence "b")))
    (puthash a (list 1) table)
    (puthash b (list 2) table)
    (vector table a)))

(setq eqhash--weak-state (eqhash--setup-weak-strings))
(eqhash--collect)
(eqhash--case weak-string-retained
  (let ((table (aref eqhash--weak-state 0)))
    (list (hash-table-count table)
          (gethash (aref eqhash--weak-state 1) table 'missing)
          (gethash (copy-sequence "a") table 'missing))))
(eqhash--case weak-string-order-reuse
  (let ((table (aref eqhash--weak-state 0)) (c (copy-sequence "c")))
    (puthash c (list 3) table)
    (list (hash-table-count table) (eqhash--entries table))))
(aset eqhash--weak-state 1 nil)
(eqhash--collect)
(eqhash--case weak-string-unrooted
  (hash-table-count (aref eqhash--weak-state 0)))

(eqhash--case weakness-t-alias
  (hash-table-weakness (make-hash-table :weakness t)))

(defun eqhash--setup-cycle (kind)
  (let* ((table (make-hash-table :test 'eq :weakness kind))
         (key (list nil)) (value (vector key)))
    (setcar key value)
    (puthash key value table)
    (vector table)))

(dolist (kind '(key value key-or-value key-and-value))
  (setq eqhash--weak-state (eqhash--setup-cycle kind))
  (eqhash--collect)
  (eqhash--emit (intern (concat "weak-cycle-" (symbol-name kind)))
                (hash-table-count (aref eqhash--weak-state 0))))

(defun eqhash--setup-chain (reverse mixed)
  (let* ((first (make-hash-table :test 'eq :weakness 'key))
         (second (make-hash-table :test 'eq
                                  :weakness (if mixed 'value 'key)))
         (a (list 'a)) (b (list 'b)) (c (list 'c)))
    (when reverse
      (let ((swap first)) (setq first second second swap)))
    (if mixed
        (progn
          (puthash a c (if reverse second first))
          (puthash b c (if reverse first second)))
      (puthash a b first)
      (puthash b c second))
    (vector first second a)))

(dolist (mixed '(nil t))
  (dolist (reverse '(nil t))
    (setq eqhash--weak-state (eqhash--setup-chain reverse mixed))
    (eqhash--collect)
    (eqhash--emit
     (if mixed (if reverse 'weak-mixed-reverse 'weak-mixed-forward)
       (if reverse 'weak-chain-reverse 'weak-chain-forward))
     (list (hash-table-count (aref eqhash--weak-state 0))
           (hash-table-count (aref eqhash--weak-state 1))))
    (aset eqhash--weak-state 2 nil)
    (eqhash--collect)
    (eqhash--emit
     (if mixed (if reverse 'weak-mixed-reverse-unrooted
                 'weak-mixed-forward-unrooted)
       (if reverse 'weak-chain-reverse-unrooted
         'weak-chain-forward-unrooted))
     (list (hash-table-count (aref eqhash--weak-state 0))
           (hash-table-count (aref eqhash--weak-state 1))))))
(setq eqhash--weak-state nil)
)

(define-hash-table-test 'eqhash-length
  (lambda (a b) (= (length a) (length b))) #'length)
(eqhash--case custom-test
  (let ((table (make-hash-table :test 'eqhash-length :size 1)))
    (puthash "ab" 'first table)
    (puthash "cd" 'second table)
    (puthash "xyz" 'third table)
    (list (hash-table-test table) (hash-table-count table)
          (gethash "ef" table 'missing) (gethash "123" table 'missing)
          (gethash "q" table 'missing))))

(define-hash-table-test 'eqhash-hash-error #'equal
  (lambda (key) (signal 'error (list "eqhash hash callback" key))))
(eqhash--case error-custom-hash
  (eqhash--caught
   (lambda () (gethash "probe" (make-hash-table :test 'eqhash-hash-error)))))
(define-hash-table-test 'eqhash-compare-error
  (lambda (a b) (signal 'error (list "eqhash compare callback" a b)))
  (lambda (_key) 0))
(eqhash--case error-custom-compare
  (let ((table (make-hash-table :test 'eqhash-compare-error)))
    (puthash "stored" 1 table)
    (eqhash--caught (lambda () (gethash "probe" table)))))

(eqhash--case error-invalid-test
  (eqhash--caught (lambda () (make-hash-table :test 'eqhash-undefined))))
(eqhash--case error-invalid-size
  (eqhash--caught (lambda () (make-hash-table :size -1))))
(eqhash--case error-invalid-weakness
  (eqhash--caught (lambda () (make-hash-table :weakness 'invalid))))
(eqhash--case error-invalid-keyword
  (eqhash--caught (lambda () (make-hash-table :invalid 1))))
(eqhash--case error-odd-keywords
  (eqhash--caught (lambda () (make-hash-table :size))))
(eqhash--case obsolete-keywords-ignored
  (let ((table (make-hash-table :rehash-size 'ignored
                               :rehash-threshold 'ignored :purecopy 'ignored)))
    (list (hash-table-test table) (hash-table-count table))))
(eqhash--case error-gethash-table
  (eqhash--caught (lambda () (gethash 'key 7))))
(eqhash--case error-puthash-table
  (eqhash--caught (lambda () (puthash 'key 'value 7))))
(eqhash--case error-remhash-table
  (eqhash--caught (lambda () (remhash 'key 7))))
(eqhash--case error-copy-table
  (eqhash--caught (lambda () (copy-hash-table 7))))
(eqhash--case error-maphash-table
  (eqhash--caught (lambda () (maphash #'ignore 7))))
(eqhash--case error-table-test
  (eqhash--caught (lambda () (hash-table-test 7))))

(prin1 (list 'eqhash-complete eqhash--count))
(terpri)
;;; eqhash-semantics.el ends here
