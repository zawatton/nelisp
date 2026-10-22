;;; standalone-weak-symbol-mark-check.el --- a symbol reached first through a weak table keeps its slots -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-weak-symbol-mark-check.el
;;
;; A symbol object's name, value, function and plist are its children.  The
;; collector reaches a symbol cell through `nl_gc_mark_symbol', which marks
;; the cell and then traces those four slots; the cell's mark is the dedup, so
;; a cell marked by any other path keeps its slots untraced for the rest of
;; the collection.  The weak-table collector (`nl_gc_weak_mark_word') used to
;; mark the class-110 words it met with `nl_gc_mark_vectorlike', which has no
;; symbol arm: it set the cell's mark (or a typed cell's bit) and stopped.
;; Two arms of the collector go through that function: the metadata of a
;; weak table (the test symbol `eq' of a table is one; marked eagerly, when the
;; table's cons is reached) and, at the end of the mark phase, the key and
;; value of every entry that survives.
;;
;; Facets 1 and 2 build the order directly with the entry arm.  An uninterned
;; symbol (a typed cell; it is in no obarray, and a plist or a property would
;; give it another path, so it has none) is the value of a live key in a weak-key
;; table, or the key of a live value in a weak-value table, and nothing else
;; refers to it.  The old collector marked its cell and traced none of its
;; slots, so the list in its value cell, the list in its function cell and its
;; name were swept and reused (the churn between the collections takes the
;; freed cells).  Facet 3 is the symptom this caused in a fresh-prelude boot,
;; where the test symbol of a weak table was the builtin `eq' whose name box
;; had been made lazily and was reachable from its cell alone: the name of an
;; interned symbol was empty or garbage, and so in the cold image dumped from
;; that boot.  Every interned symbol must give its name back through `intern'.
;;
;; The conservative stack scan is switched off so that only the precise roots
;; decide what lives.  Prints one WEAK-SYMBOL-MARK line and exits non-zero on
;; failure.  The binary built from the same tree without the fix dies reading
;; a swept cell or reports damage (tools/ai/integ-wh-progress.org, S2.16).

(nelisp--debug-switch 28)

(defvar wsm-failures nil)
(defvar wsm-count 24)

(defun wsm-value (i) (list i (+ i 1) (+ i 2) (number-to-string (* i 4711))))
(defun wsm-function (i) (list 'lambda nil (list 'quote (list i (number-to-string i)))))
(defun wsm-name (i) (concat "weakmark-symbol-" (number-to-string i)))

(defun wsm-symbol (i)
  (let ((sym (make-symbol (wsm-name i))))
    (set sym (wsm-value i))
    (fset sym (wsm-function i))
    sym))

;; The symbols are not kept: each one leaves these functions referenced by the
;; weak table alone.
(defun wsm-build-weak-key ()
  (let ((out nil) (i 0))
    (while (< i wsm-count)
      (let ((tbl (make-hash-table :test 'eq :weakness 'key)) (k (list 'live-key i)))
        (puthash k (wsm-symbol i) tbl)
        (push (cons k tbl) out))
      (setq i (1+ i)))
    (nreverse out)))

(defun wsm-build-weak-value ()
  (let ((out nil) (i 0))
    (while (< i wsm-count)
      (let ((tbl (make-hash-table :test 'eq :weakness 'value)) (v (list 'live-value i)))
        (puthash (wsm-symbol i) v tbl)
        (push (cons v tbl) out))
      (setq i (1+ i)))
    (nreverse out)))

(defun wsm-churn ()
  (dotimes (i 20000) (list i i i) (make-string 40 ?x) (vector i i)))

(defun wsm-judge (facet i sym)
  (let ((got (condition-case e (symbol-name sym) (error (list 'error e)))))
    (unless (equal got (wsm-name i)) (push (list facet i 'name got) wsm-failures)))
  (let ((got (condition-case e (symbol-value sym) (error (list 'error e)))))
    (unless (equal got (wsm-value i)) (push (list facet i 'value got) wsm-failures)))
  (let ((got (condition-case e (symbol-function sym) (error (list 'error e)))))
    (unless (equal got (wsm-function i)) (push (list facet i 'function got) wsm-failures))))

(defun wsm-only (tbl pick)
  (let (r) (maphash (lambda (k v) (setq r (if (eq pick 'key) k v))) tbl) r))

;; Facets 1 and 2.  The tables are locals of the form that runs the
;; collections, so the evaluator's environment keeps them alive.
(let ((weak-key (wsm-build-weak-key)) (weak-value (wsm-build-weak-value)))
  (garbage-collect)
  (garbage-collect)
  (wsm-churn)
  (garbage-collect)
  (wsm-churn)
  (princ "WEAK-SYMBOL-MARK facet 1: value of a live key in a weak-key table\n")
  (let ((i 0))
    (dolist (e weak-key)
      (wsm-judge 'weak-key-table-value i (gethash (car e) (cdr e)))
      (setq i (1+ i))))
  (princ "WEAK-SYMBOL-MARK facet 2: key of a live value in a weak-value table\n")
  (let ((i 0))
    (dolist (e weak-value)
      (wsm-judge 'weak-value-table-key i (wsm-only (cdr e) 'key))
      (setq i (1+ i)))))

;; Facet 3.  Every interned symbol answers its own name, and `intern' of that
;; name answers the symbol.
(princ "WEAK-SYMBOL-MARK facet 3: every interned symbol gives its name back\n")
(let ((n 0) (bad 0))
  (mapatoms
   (lambda (s)
     (setq n (1+ n))
     (let ((nm (condition-case _e (symbol-name s) (error nil))))
       (unless (and (stringp nm) (> (length nm) 0) (eq s (intern nm)))
         (setq bad (1+ bad))
         (when (< bad 6) (push (list 'atom 'name nm) wsm-failures))))))
  (unless (> n 1000) (push (list 'atom 'count n) wsm-failures))
  (unless (= bad 0) (push (list 'atom 'damaged-symbols bad) wsm-failures)))

(nelisp--debug-switch 29)

(dolist (f (reverse wsm-failures))
  (princ (format "  DAMAGED %S\n" f)))
(princ (format "WEAK-SYMBOL-MARK symbols=%d failures=%d %s\n"
               (* 2 wsm-count) (length wsm-failures) (if wsm-failures "FAIL" "PASS")))
(kill-emacs (if wsm-failures 1 0))

;;; standalone-weak-symbol-mark-check.el ends here
