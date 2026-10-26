;;; nelisp-record-aref-test.el --- Baref/Baset and the fx aref/aset ids on records -*- lexical-binding: t; -*-

;; corecall: `wf_bytecode_vector_primitive' (the byte-code VM's Baref/Baset) and
;; `m5_fx_primitive_run' ids -14/-15 (the fx VM and the interpreter) read and write a
;; record's slots directly, as GNU data.c Faref/Faset do for RECORDP (PVSIZE):
;; index 0 is the type word, K > 0 is data slot K-1, K may equal the slot count.

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)
(require 'nelisp-cc-fx-call-primitives)
(require 'nelisp-directcall-test)

(defconst nra--rec 40006)          ; record word, header at 40000
(defconst nra--vec 50006)          ; vector word, header at 50000
(defconst nra--mem 4000)           ; VM stack words
(defconst nra--stk 4000)

(defun nra--model ()
  "Memory: the record has 3 slots (from slot 0: 2000 2001 2002), type word 1111."
  (let ((m (make-hash-table :test #'eql)))
    (puthash 40000 (+ 12 (ash 3 8)) m) (puthash 40008 1111 m)
    (dotimes (i 3) (puthash (+ 40016 (* 8 i)) (+ 2000 i) m))
    (puthash 50000 (+ 8 (ash 4 8)) m)
    (dotimes (i 4) (puthash (+ 50008 (* 8 i)) (+ 3000 i) m))
    m))

(defun nra--fix (n) (+ (ash n 2) 1))

(defvar nra--dirty 0)
(defvar nra--sets nil)

(defmacro nra--with (m &rest body)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'ptr-read-u64) (lambda (p i) (gethash (+ p i) ,m 0)))
             ((symbol-function 'heap-read-u64) (lambda (p i) (gethash (+ p i) ,m 0)))
             ((symbol-function 'ptr-write-u64) (lambda (p i v) (puthash (+ p i) v ,m)))
             ((symbol-function 'heap-sexp-tag-word)
              (lambda (w) (cond ((= (logand w 7) 6) (logand (gethash (- w 6) ,m 0) 63)) (t 0))))
             ((symbol-function 'nl_word_tag64) (lambda (_) 0))
             ((symbol-function 'wf_bytecode_w_tag)
              (lambda (w) (if (= (logand w 3) 1) 2 (if (= w 3) 0 (if (= w 7) 1 99)))))
             ((symbol-function 'wf_bytecode_w_int) (lambda (w) (ash w -2)))
             ((symbol-function 'vector-len) (lambda (w) (ash (gethash (- w 6) ,m 0) -8)))
             ((symbol-function 'nl_record_length) (lambda (w) (ash (gethash (- w 6) ,m 0) -8)))
             ((symbol-function 'nl_word_payload) (lambda (w) (- w (logand w 6))))
             ((symbol-function 'nl_record_type_word) (lambda (w) (gethash (+ (- w 6) 8) ,m 0)))
             ((symbol-function 'nl_record_slot_word)
              (lambda (w i) (gethash (+ (- w 6) 16 (* 8 i)) ,m 0)))
             ((symbol-function 'nl_vector_slot_word)
              (lambda (w i) (gethash (+ (- w 6) 8 (* 8 i)) ,m 0)))
             ((symbol-function 'nl_word_canon) (lambda (w) w))
             ((symbol-function 'bf_nemacs_char_table_vector_p) (lambda (_) 0))
             ((symbol-function 'wf_dirty) (lambda () (cl-incf nra--dirty)))
             ((symbol-function 'vector-slot-set)
              (lambda (w i v) (puthash (+ (- w 6) 8 (* 8 i)) v ,m))))
     (setq nra--dirty 0)
     ,@body))

(defun nra--bc-fn (&optional mutate)
  (let* ((f (ndc--find 'wf_bytecode_vector_primitive nelisp-standalone--applyfn-bytecode-helpers))
         (form (cons 'lambda (cddr f))))
    (when mutate (setq form (funcall mutate form)))
    (eval (ndc--host form) t)))

(defun nra--fx-fn (&optional mutate)
  (let* ((f (ndc--find 'm5_fx_primitive_run (cdr nelisp-cc-fx-call-primitives--source)))
         (form (cons 'lambda (cddr f))))
    (when mutate (setq form (funcall mutate form)))
    (eval (ndc--host form) t)))

;; ---- byte-code VM: operands at words 0..2 of the stack at MEM, SP counts them ----

(defun nra--bc-aref (run m arr idx)
  (puthash nra--mem arr m) (puthash (+ nra--mem 8) idx m)
  (let ((rc (funcall run nra--mem 2 72)))
    (list rc (gethash nra--mem m))))

(ert-deftest nra-bc-aref-record-slots ()
  (let ((run (nra--bc-fn)))
    (dolist (c '((0 1111) (1 2000) (2 2001) (3 2002)))
      (let ((m (nra--model)))
        (nra--with m
          (should (equal (nra--bc-aref run m nra--rec (nra--fix (car c))) (list 0 (cadr c)))))))
    ;; out of range and negative index decline with the operands untouched
    (dolist (i '(4 -1))
      (let ((m (nra--model)))
        (nra--with m
          (should (equal (nra--bc-aref run m nra--rec (nra--fix i)) (list 2 nra--rec))))))
    ;; non-fixnum index declines
    (let ((m (nra--model)))
      (nra--with m
        (should (equal (nra--bc-aref run m nra--rec 77) (list 2 nra--rec)))))))

(ert-deftest nra-bc-aref-vector-unchanged ()
  (let ((run (nra--bc-fn)))
    (dotimes (i 4)
      (let ((m (nra--model)))
        (nra--with m
          (should (equal (nra--bc-aref run m nra--vec (nra--fix i)) (list 0 (+ 3000 i)))))))
    (let ((m (nra--model)))
      (nra--with m
        (should (equal (nra--bc-aref run m nra--vec (nra--fix 4)) (list 2 nra--vec)))))))

(ert-deftest nra-bc-aref-seven-slot-vector-asks-the-char-table-test ()
  (let ((run (nra--bc-fn)) (m (nra--model)) (asked 0))
    (puthash 50000 (+ 8 (ash 7 8)) m)
    (nra--with m
      (cl-letf (((symbol-function 'bf_nemacs_char_table_vector_p) (lambda (_) (cl-incf asked) 1)))
        (should (equal (nra--bc-aref run m nra--vec (nra--fix 1)) (list 2 nra--vec)))
        (should (= asked 1))))
    (puthash 50000 (+ 8 (ash 4 8)) m)
    (nra--with m
      (cl-letf (((symbol-function 'bf_nemacs_char_table_vector_p) (lambda (_) (cl-incf asked) 1)))
        (nra--bc-aref run m nra--vec (nra--fix 1))
        (should (= asked 1))))))

(ert-deftest nra-bc-aset-record-slots ()
  (let ((run (nra--bc-fn)))
    ;; slots 1..3 are written (slot K-1), the value is the result, the epoch is bumped
    (dolist (k '(1 2 3))
      (let ((m (nra--model)))
        (nra--with m
          (puthash nra--mem nra--rec m) (puthash (+ nra--mem 8) (nra--fix k) m)
          (puthash (+ nra--mem 16) (nra--fix 9) m)
          (should (= (funcall run nra--mem 3 73) 0))
          (should (= (gethash nra--mem m) (nra--fix 9)))
          (should (= (gethash (+ 40016 (* 8 (1- k))) m) (nra--fix 9)))
          (should (= nra--dirty 1))
          ;; the neighbours are untouched
          (dotimes (i 3)
            (unless (= i (1- k)) (should (= (gethash (+ 40016 (* 8 i)) m) (+ 2000 i))))))))
    ;; index 0 (the type word) and out of range decline with nothing written
    (dolist (i '(0 4 -1))
      (let ((m (nra--model)))
        (nra--with m
          (puthash nra--mem nra--rec m) (puthash (+ nra--mem 8) (nra--fix i) m)
          (puthash (+ nra--mem 16) (nra--fix 9) m)
          (let ((before (copy-hash-table m)))
            (should (= (funcall run nra--mem 3 73) 2))
            (should (= nra--dirty 0))
            (maphash (lambda (k v) (should (equal (gethash k m) v))) before)))))))

;; ---- fx VM / interpreter: ids -14 aref, -15 aset, -16 length ----

(defconst nra--resw 4096)

(defun nra--fx (run m id arr idx &optional val)
  (let ((argc (if (= id -15) 3 2)))
    (puthash nra--stk arr m) (puthash (+ nra--stk 8) idx m) (puthash (+ nra--stk 16) val m)
    (puthash nra--resw 0 m)
    (list (funcall run id nra--stk argc argc nra--resw 0 0) (gethash nra--resw m))))

(ert-deftest nra-fx-aref-record-and-vector ()
  (let ((run (nra--fx-fn)))
    (dolist (c '((0 1111) (1 2000) (3 2002)))
      (let ((m (nra--model)))
        (nra--with m
          (should (equal (nra--fx run m -14 nra--rec (nra--fix (car c))) (list 0 (cadr c)))))))
    (dolist (i '(4 -1))
      (let ((m (nra--model)))
        (nra--with m
          (should (equal (nra--fx run m -14 nra--rec (nra--fix i)) (list -3 0))))))
    (let ((m (nra--model)))
      (nra--with m
        (should (equal (nra--fx run m -14 nra--vec (nra--fix 2)) (list 0 3002)))
        (should (equal (nra--fx run m -14 nra--rec 77) (list -3 0)))))))

(ert-deftest nra-fx-aset-record ()
  (let ((run (nra--fx-fn)))
    (let ((m (nra--model)))
      (nra--with m
        (should (equal (nra--fx run m -15 nra--rec (nra--fix 2) (nra--fix 9)) (list 0 (nra--fix 9))))
        (should (= (gethash 40024 m) (nra--fix 9)))
        (should (= nra--dirty 1))))
    (dolist (i '(0 4 -1))
      (let ((m (nra--model)))
        (nra--with m
          (should (equal (nra--fx run m -15 nra--rec (nra--fix i) (nra--fix 9)) (list -3 0)))
          (should (= nra--dirty 0))
          (dotimes (j 3) (should (= (gethash (+ 40016 (* 8 j)) m) (+ 2000 j)))))))))

(ert-deftest nra-fx-length-of-a-record-stays-with-the-builtin ()
  (let ((run (nra--fx-fn)) (m (nra--model)))
    (nra--with m
      (puthash nra--stk nra--rec m) (puthash nra--resw 0 m)
      (should (= (funcall run -16 nra--stk 1 1 nra--resw 0 0) -3))
      (should (= (gethash nra--resw m) 0)))))

;; ---- negative controls: the checks above must fail on a broken body ----

(defun nra--subst-form (from to form)
  (cond ((equal form from) to)
        ((consp form) (cons (nra--subst-form from to (car form)) (nra--subst-form from to (cdr form))))
        (t form)))

(ert-deftest nra-negative-control-dropped-result-write-is-caught ()
  ;; A body that never writes the record read's result must not satisfy the aref check.
  (let* ((mutate (lambda (form)
                   (nra--subst-form '(nl_record_slot_word arr (- i 1)) 0 form)))
         (run (nra--bc-fn mutate)) (m (nra--model)) (got nil))
    (nra--with m
      (setq got (nra--bc-aref run m nra--rec (nra--fix 2))))
    (should-not (equal got (list 0 2001)))))

(ert-deftest nra-negative-control-skipped-bound-check-is-caught ()
  ;; Without the upper bound, index 4 reads past the record: the decline check must see it.
  (let* ((mutate (lambda (form)
                   (nra--subst-form '(> i (nl_record_length arr)) '(> i 100) form)))
         (run (nra--bc-fn mutate)) (m (nra--model)) (got nil))
    (nra--with m
      (setq got (nra--bc-aref run m nra--rec (nra--fix 4))))
    (should-not (equal got (list 2 nra--rec)))))

(ert-deftest nra-negative-control-missing-dirty-is-caught ()
  (let* ((mutate (lambda (form) (nra--subst-form '(wf_dirty) 0 form)))
         (run (nra--bc-fn mutate)) (m (nra--model)))
    (nra--with m
      (puthash nra--mem nra--rec m) (puthash (+ nra--mem 8) (nra--fix 1) m)
      (puthash (+ nra--mem 16) (nra--fix 9) m)
      (funcall run nra--mem 3 73)
      (should (= nra--dirty 0)))))

;; ---- the fast loop's arm: Blength / Baref / Baset answer the new SP or -1 ----

(defun nra--slot-fn ()
  (let* ((f (ndc--find 'wf_bytecode_fast_slot nelisp-standalone--applyfn-bytecode-helpers)))
    (should f)
    (eval (ndc--host (cons 'lambda (cddr f))) t)))

(ert-deftest nra-fast-slot-length ()
  (let ((run (nra--slot-fn)))
    (dolist (c `((3 1)                  ; nil -> 0
                 (,nra--vec ,(nra--fix 4))))
      (let ((m (nra--model)))
        (nra--with m
          (puthash nra--mem (car c) m)
          (should (= (funcall run nra--mem 1 71) 1))
          (should (= (gethash nra--mem m) (cadr c))))))
    ;; a proper two-element list (direct cons words 98 and 202: low bits 010)
    (let ((m (nra--model)))
      (puthash 96 (nra--fix 1) m) (puthash 104 202 m)
      (puthash 200 (nra--fix 2) m) (puthash 208 3 m)
      (nra--with m
        (puthash nra--mem 98 m)
        (should (= (funcall run nra--mem 1 71) 1))
        (should (= (gethash nra--mem m) (nra--fix 2)))))
    ;; improper list, 7-slot vector, a record, a fixnum: left to the general loop, untouched
    (let ((m (nra--model)))
      (puthash 96 (nra--fix 1) m) (puthash 104 (nra--fix 5) m)
      (puthash 50000 (+ 8 (ash 7 8)) m)
      (nra--with m
        (dolist (a (list 98 nra--vec nra--rec (nra--fix 3) 77))
          (puthash nra--mem a m)
          (should (= (funcall run nra--mem 1 71) -1))
          (should (= (gethash nra--mem m) a)))))
    ;; no operand
    (let ((m (nra--model)))
      (nra--with m (should (= (funcall run nra--mem 0 71) -1))))))

(ert-deftest nra-fast-slot-aref-aset ()
  (cl-letf (((symbol-function 'wf_bytecode_vector_primitive) (nra--bc-fn)))
   (let ((run (nra--slot-fn)))
    (let ((m (nra--model)))
      (nra--with m
        (puthash nra--mem nra--rec m) (puthash (+ nra--mem 8) (nra--fix 2) m)
        (should (= (funcall run nra--mem 2 72) 1))          ; two operands -> one result
        (should (= (gethash nra--mem m) 2001))
        (puthash nra--mem nra--vec m) (puthash (+ nra--mem 8) (nra--fix 3) m)
        (should (= (funcall run nra--mem 2 72) 1))
        (should (= (gethash nra--mem m) 3003))
        (puthash nra--mem nra--rec m) (puthash (+ nra--mem 8) (nra--fix 1) m)
        (puthash (+ nra--mem 16) (nra--fix 8) m)
        (should (= (funcall run nra--mem 3 73) 1))          ; three operands -> one result
        (should (= (gethash nra--mem m) (nra--fix 8)))
        (should (= (gethash 40016 m) (nra--fix 8)))))
    ;; declines answer -1 and write nothing (index past the end, too few operands)
    (let ((m (nra--model)))
      (nra--with m
        (puthash nra--mem nra--rec m) (puthash (+ nra--mem 8) (nra--fix 9) m)
        (should (= (funcall run nra--mem 2 72) -1))
        (should (= (gethash nra--mem m) nra--rec))
        (should (= (funcall run nra--mem 1 72) -1))
        (should (= (funcall run nra--mem 2 73) -1)))))))

;; ---- m5_fx_call_subr: the fx VM's direct call of a Subr held by the site's symbol ----

(defconst nra--cache 60006)        ; the site vector [SYMBOL ...]: header 60000, slot 0 at 60008
(defconst nra--sym 61006)          ; symbol object: header 61000 (kind 4), function at 61024
(defconst nra--subr 62006)         ; Subr object: header 62000

(defun nra--subr-model (id mn mx)
  (let ((m (make-hash-table :test #'eql)))
    (puthash 60000 (+ 8 (ash 5 8)) m) (puthash 60008 nra--sym m)
    (puthash 61000 4 m) (puthash 61024 nra--subr m)
    (puthash 62000 (+ 20 (ash (+ (+ id 17) (ash mn 8) (ash mx 16) (ash 1 24)) 8)) m)
    m))

(defun nra--call-subr (m argc &optional cache)
  (let ((ran nil))
    (cl-letf (((symbol-function 'ptr-read-u64) (lambda (p i) (gethash (+ p i) m 0)))
              ((symbol-function 'm5_fx_primitive_run)
               (lambda (&rest a) (push a ran) (puthash 4096 5555 m) 0)))
      (let* ((f (ndc--find 'm5_fx_call_subr (cdr nelisp-cc-fx-call-primitives--source)))
             (run (eval (ndc--host (cons 'lambda (cddr f))) t))
             (rc (funcall run 1000 (or cache nra--cache) nra--stk 3 argc nra--resw)))
        (list rc (car ran))))))

(ert-deftest nra-fx-call-subr-runs-the-descriptor-id ()
  (dolist (c '((1 1 1 1) (10 2 2 2) (-14 2 2 2) (-15 3 3 3) (-16 1 1 1) (3 1 1 1) (4 1 1 1)
               (5 2 2 2) (6 2 2 2) (7 2 2 2) (11 2 2 2)))
    (pcase-let ((`(,id ,mn ,mx ,argc) c))
      (let ((m (nra--subr-model id mn mx)))
        (should (equal (nra--call-subr m argc) (list 0 (list id nra--stk 3 argc nra--resw 1000 0)))))
      ;; the symbol itself as the operand works too
      (let ((m (nra--subr-model id mn mx)))
        (should (equal (nra--call-subr m argc nra--sym) (list 0 (list id nra--stk 3 argc nra--resw 1000 0))))))))

(ert-deftest nra-fx-call-subr-declines ()
  (let ((cases
         `(("wrong arity low" -14 2 2 1 ,#'ignore)
           ("wrong arity high" -14 2 2 3 ,#'ignore)
           ("id outside the leaf set (cdr 109)" 109 1 1 1 ,#'ignore)
           ("allocating id" 112 0 127 1 ,#'ignore)
           ("no id (interp-only subr)" -17 1 1 1 ,(lambda (m) (puthash 62000 (+ 20 (ash 0 8)) m)))
           ("function cell is a cons" -14 2 2 2 ,(lambda (m) (puthash 61024 6002 m)))
           ("function cell empty" -14 2 2 2 ,(lambda (m) (puthash 61024 3 m)))
           ("function cell is a symbol" -14 2 2 2 ,(lambda (m) (puthash 61024 nra--sym m)))
           ("not a symbol object" -14 2 2 2 ,(lambda (m) (puthash 61000 17 m)))
           ("operand is not a vector or symbol" -14 2 2 2 ,(lambda (m) (puthash 60000 17 m))))))
    (dolist (c cases)
      (let ((m (nra--subr-model (nth 1 c) (nth 2 c) (nth 3 c))))
        (funcall (nth 5 c) m)
        (should (equal (list (car c) (car (nra--call-subr m (nth 4 c)))) (list (car c) -3)))
        (should (null (cadr (nra--call-subr m (nth 4 c)))))))
    ;; a non-heap operand
    (let ((m (nra--subr-model -14 2 2)))
      (should (equal (car (nra--call-subr m 2 3)) -3))
      (should (equal (car (nra--call-subr m 2 1)) -3)))))

(provide 'nelisp-record-aref-test)
