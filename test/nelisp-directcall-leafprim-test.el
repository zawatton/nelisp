;;; nelisp-directcall-leafprim-test.el --- Bcall of a primitive held in a variable -*- lexical-binding: t; -*-

;; leafprim: `wf_bytecode_call_leaf' runs a leaf directcall id (a Subr object reached
;; through a symbol object's function slot, or held directly) on the operand words
;; without the generic lookup, `nl_directcall_id' or `nl_directcall_invoke'.  Every
;; shape it does not own answers 2 with nothing written.

(require 'nelisp-directcall-test)

;; Memory model (word-addressed by byte offset): env 1000 (policy word 1112, state
;; pointer 1176 -> 3000 holding the depth), VM words from MEM 5000, slot FI 2.
(defconst nlp--mem-base 5000)
(defconst nlp--fi 2)
(defconst nlp--sym 20006)      ; a symbol object word (tag 6): header at 20000
(defconst nlp--subr 30006)     ; a Subr object word: header at 30000
(defconst nlp--vec 40006)      ; a vector word: header at 40000

(defun nlp--desc (id min max)
  "The header of a Subr object for directcall ID (the packed descriptor)."
  (+ 20 (ash (+ (+ id 17) (ash min 8) (ash max 16) (ash 1 24)) 8)))

(defun nlp--mem (id min max &optional depth policy)
  (let ((m (make-hash-table :test #'eql)))
    (puthash 1176 3000 m) (puthash 3000 (or depth 7) m) (puthash 1112 (or policy 0) m)
    (puthash 20000 4 m) (puthash 20024 nlp--subr m)
    (puthash 30000 (nlp--desc id min max) m)
    (puthash 40000 (+ 8 (ash 4 8)) m)
    m))

(defun nlp--word (m i) (gethash (+ nlp--mem-base (* 8 i)) m 0))
(defun nlp--set (m i v) (puthash (+ nlp--mem-base (* 8 i)) v m))

(defvar nlp--runs nil "Calls of the mocked `nl_directcall_run' (corecall).")
(defvar nlp--run-status -3 "The status the mocked `nl_directcall_run' answers.")
(defvar nlp--seen nil "The operand words the mocked body saw.")

(defmacro nlp--with (mem calls &rest body)
  (declare (indent 2))
  `(cl-letf (((symbol-function 'ptr-read-u64) (lambda (p i) (gethash (+ p i) ,mem 0)))
             ((symbol-function 'heap-read-u64) (lambda (p i) (gethash (+ p i) ,mem 0)))
             ((symbol-function 'ptr-write-u64) (lambda (p i v) (puthash (+ p i) v ,mem)))
             ((symbol-function 'vector-len) (lambda (_) 4))
             ((symbol-function 'wf_dirty) (lambda () (cl-incf (car ,calls))))
             ((symbol-function 'vector-slot-set)
              (lambda (_v i x) (puthash (+ 900000 i) x ,mem)))
             ((symbol-function 'nl_word_canon) (lambda (w) w))
             ((symbol-function 'nl_directcall_fast_run)
              (lambda (_id _b _n out) (puthash out 4242 ,mem) 0))
             ((symbol-function 'nl_directcall_run)
              (lambda (id base argc out env)
                (push (list id base argc out env) nlp--runs)
                (setq nlp--seen (let (w) (dotimes (i argc) (push (gethash (+ base (* 8 i)) ,mem) w)) (nreverse w)))
                (when (eq nlp--run-status 0) (puthash out 5555 ,mem))
                nlp--run-status))
             ((symbol-function 'nl_vector_slot_word)
              (lambda (v i) (gethash (+ v -6 8 (* 8 i)) ,mem 0)))
             ((symbol-function 'nl_record_length) (lambda (_) 3))
             ((symbol-function 'nl_record_type_word) (lambda (_) 1111))
             ((symbol-function 'nl_record_slot_word) (lambda (_r i) (+ 2000 i)))
             ((symbol-function 'nl_directcall_invoke)
              (lambda (&rest _) (error "invoke must not be reached")))
             ((symbol-function 'nl_directcall_id)
              (lambda (&rest _) (error "id must not be reached"))))
     ,@body))

(defun nlp--call (m argc &optional fw)
  (wf_bytecode_call_leaf 1000 nlp--mem-base nlp--fi argc (or fw nlp--sym)))

(defun nlp--cons (m addr car cdr)
  (puthash (- addr 2) car m) (puthash (+ (- addr 2) 8) cdr m) (+ addr 0))

(ert-deftest nlp-car-cdr-identity-hit ()
  (ndc--with-definitions
   (lambda (_)
     (dolist (c '((1 5006 5) (1 3 3) (109 5006 7) (110 123 123)))
       (let* ((m (nlp--mem (car c) 1 1)) (calls (list 0)) (cell 6002))
         (nlp--cons m 6002 5 7)
         (nlp--set m 3 (if (= (cadr c) 5006) cell (cadr c)))
         (nlp--set m 2 nlp--sym)
         (nlp--with m calls
           (should (= (nlp--call m 1) 0))
           (should (= (nlp--word m 2) (if (= (cadr c) 5006) (caddr c) (caddr c))))
           (should (= (nlp--word m 3) 3))))))))

(ert-deftest nlp-subr-object-held-directly ()
  (ndc--with-definitions
   (lambda (_)
     (let* ((m (nlp--mem 1 1 1)) (calls (list 0)))
       (nlp--cons m 6002 5 7) (nlp--set m 3 6002)
       (nlp--with m calls
         (should (= (nlp--call m 1 nlp--subr) 0))
         (should (= (nlp--word m 2) 5)))))))

(ert-deftest nlp-nth-length-logand-aset-hit ()
  (ndc--with-definitions
   (lambda (_)
     (let* ((m (nlp--mem 10 2 2)) (calls (list 0)))
       ;; (nth 1 '(5 7))
       (nlp--cons m 6002 5 6018) (nlp--cons m 6018 7 3)
       (nlp--set m 3 (+ (ash 1 2) 1)) (nlp--set m 4 6002)
       (nlp--with m calls
         (should (= (nlp--call m 2) 0))
         (should (= (nlp--word m 2) 7))
         (should (= (nlp--word m 3) 3)) (should (= (nlp--word m 4) 3))))
     (let* ((m (nlp--mem 10 2 2)) (calls (list 0)))
       (nlp--set m 3 (+ (ash 5 2) 1)) (nlp--set m 4 3)
       (nlp--with m calls            ; (nth 5 nil) -> nil
         (should (= (nlp--call m 2) 0)) (should (= (nlp--word m 2) 3))))
     (let* ((m (nlp--mem -16 1 1)) (calls (list 0)))
       (nlp--set m 3 nlp--vec)
       (nlp--with m calls
         (should (= (nlp--call m 1) 0))
         (should (= (nlp--word m 2) (+ (ash 4 2) 1)))))
     (let* ((m (nlp--mem -16 1 1)) (calls (list 0)))
       (nlp--cons m 6002 5 6018) (nlp--cons m 6018 7 3) (nlp--set m 3 6002)
       (nlp--with m calls
         (should (= (nlp--call m 1) 0))
         (should (= (nlp--word m 2) (+ (ash 2 2) 1)))))
     (let* ((m (nlp--mem 27 0 127)) (calls (list 0)))
       (nlp--set m 3 (+ (ash 29 2) 1)) (nlp--set m 4 (+ (ash 13 2) 1))
       (nlp--with m calls
         (should (= (nlp--call m 2) 0))
         (should (= (nlp--word m 2) (+ (ash 13 2) 1)))))
     (let* ((m (nlp--mem -15 3 3)) (calls (list 0)))
       (nlp--set m 3 nlp--vec) (nlp--set m 4 (+ (ash 2 2) 1)) (nlp--set m 5 (+ (ash 9 2) 1))
       (nlp--with m calls
         (should (= (nlp--call m 3) 0))
         (should (= (car calls) 1))
         (should (= (gethash 900002 m) (+ (ash 9 2) 1)))
         (should (= (nlp--word m 2) (+ (ash 9 2) 1)))
         (should (= (nlp--word m 5) 3)))))))

(ert-deftest nlp-arith-delegates-to-the-fast-body ()
  (ndc--with-definitions
   (lambda (_)
     (dolist (id '(105 106 107 108))
       (let* ((m (nlp--mem id 0 127)) (calls (list 0)))
         (nlp--set m 3 (+ (ash 1 2) 1))
         (nlp--with m calls
           (should (= (nlp--call m 1) 0))
           (should (= (nlp--word m 2) 4242))))))))

(ert-deftest nlp-declines-write-nothing ()
  (ndc--with-definitions
   (lambda (_)
     ;; Each case: (id min max argc setup) where SETUP edits the model; all answer 2
     ;; and leave slot FI and the operand words untouched.
     (let ((cases
            `(("wrong-type car" 1 1 1 1 ,(lambda (m) (nlp--set m 3 (+ (ash 4 2) 1))))
              ("nth negative" 10 2 2 2 ,(lambda (m) (nlp--set m 3 (- 1 4)) (nlp--set m 4 3)))
              ("nth non-list tail" 10 2 2 2 ,(lambda (m) (nlp--set m 3 (+ (ash 3 2) 1)) (nlp--set m 4 77)))
              ("length string" -16 1 1 1 ,(lambda (m) (nlp--set m 3 77)))
              ("aset out of range" -15 3 3 3
               ,(lambda (m) (nlp--set m 3 nlp--vec) (nlp--set m 4 (+ (ash 4 2) 1))))
              ("aset non-fixnum index" -15 3 3 3
               ,(lambda (m) (nlp--set m 3 nlp--vec) (nlp--set m 4 77)))
              ("logand non-fixnum" 27 0 127 2 ,(lambda (m) (nlp--set m 3 78) (nlp--set m 4 5)))
              ("logand no args" 27 0 127 0 ,(lambda (_m) nil))
              ("arity low" 10 2 2 1 ,(lambda (m) (nlp--set m 3 3)))
              ("arity high" 1 1 1 2 ,(lambda (m) (nlp--set m 3 3)))
              ("allocating id" 112 0 127 1 ,(lambda (m) (nlp--set m 3 3)))
              ("hash id" 101 2 3 2 ,(lambda (m) (nlp--set m 3 3)))
              ("debug policy" 1 1 1 1 ,(lambda (m) (nlp--set m 3 3) (puthash 1112 1048576 m)))
              ("depth limit" 1 1 1 1 ,(lambda (m) (nlp--set m 3 3) (puthash 3000 32768 m)))
              ("no state" 1 1 1 1 ,(lambda (m) (nlp--set m 3 3) (puthash 1176 0 m)))
              ("function cell is a cons" 1 1 1 1
               ,(lambda (m) (nlp--set m 3 3) (puthash 20024 6002 m)))
              ("function cell empty" 1 1 1 1 ,(lambda (m) (nlp--set m 3 3) (puthash 20024 0 m)))
              ("not an object" 1 1 1 1 ,(lambda (m) (nlp--set m 3 3) (nlp--set m 2 8)))
              ("no descriptor id" 1 1 1 1 ,(lambda (m) (nlp--set m 3 3) (puthash 30000 20 m))))))
       (dolist (c cases)
         (let* ((m (nlp--mem (nth 1 c) (nth 2 c) (nth 3 c))) (calls (list 0))
                (argc (nth 4 c)) (before nil))
           (nlp--set m 2 nlp--sym)
           (funcall (nth 5 c) m)
           (setq before (copy-hash-table m))
           (nlp--with m calls
             (should (equal (list (car c) (nlp--call m argc (nlp--word m 2))) (list (car c) 2)))
             (should (= (car calls) 0))
             (let ((same t))
               (maphash (lambda (k v) (unless (equal (gethash k m) v) (setq same nil))) before)
               (should same)
               (should (= (hash-table-count m) (hash-table-count before)))))))))))

(ert-deftest nlp-wf-bytecode-call-tries-the-leaf-arm-first ()
  (let ((f (ndc--find 'wf_bytecode_call nelisp-standalone--applyfn-bytecode-helpers)))
    (should f)
    (should (ndc--contains 'wf_bytecode_call_leaf f))
    ;; the old route stays reachable behind it
    (should (ndc--contains 'nl_directcall_invoke f))))

;; ---- corecall: ids the inline arms do not own run their WORD body from the header id ----

(ert-deftest nlp-delegated-leaf-ids-run-the-body-from-the-header-id ()
  (ndc--with-definitions
   (lambda (_)
     (dolist (c '((101 2 3 2) (100 1 127 3) (111 2 2 2) (7 2 2 2) (3 1 1 1) (4 1 1 1)
                  (5 2 2 2) (6 2 2 2) (11 2 2 2)))
       (pcase-let* ((`(,id ,mn ,mx ,argc) c))
         ;; a hit: the body wrote 5555 into slot FI, the operand words are cleared
         (let* ((m (nlp--mem id mn mx)) (calls (list 0)) (nlp--runs nil) (nlp--run-status 0))
           (nlp--set m 2 nlp--sym)
           (dotimes (i argc) (nlp--set m (+ nlp--fi 1 i) (+ (ash (+ 7 i) 2) 1)))
           (nlp--with m calls
             (should (= (nlp--call m argc) 0))
             (should (equal nlp--runs (list (list id (+ nlp--mem-base (* 8 (1+ nlp--fi))) argc
                                                  (+ nlp--mem-base (* 8 nlp--fi)) 1000))))
             (should (= (nlp--word m nlp--fi) 5555))
             (dotimes (i argc) (should (= (nlp--word m (+ nlp--fi 1 i)) 3)))))
         ;; a decline (-3): answer 2, nothing written, operands kept
         (let* ((m (nlp--mem id mn mx)) (calls (list 0)) (nlp--runs nil) (nlp--run-status -3))
           (nlp--set m 2 nlp--sym)
           (dotimes (i argc) (nlp--set m (+ nlp--fi 1 i) (+ (ash (+ 7 i) 2) 1)))
           (nlp--with m calls
             (should (= (nlp--call m argc) 2))
             (should (= (length nlp--runs) 1))
             (should (= (nlp--word m nlp--fi) nlp--sym))
             (dotimes (i argc) (should (= (nlp--word m (+ nlp--fi 1 i)) (+ (ash (+ 7 i) 2) 1))))))
         ;; a signalled call (status 1): answer 1, operands kept for the backtrace
         (let* ((m (nlp--mem id mn mx)) (calls (list 0)) (nlp--runs nil) (nlp--run-status 1))
           (dotimes (i argc) (nlp--set m (+ nlp--fi 1 i) (+ (ash (+ 7 i) 2) 1)))
           (nlp--with m calls
             (should (= (nlp--call m argc) 1))
             (dotimes (i argc) (should (= (nlp--word m (+ nlp--fi 1 i)) (+ (ash (+ 7 i) 2) 1)))))))))))

(ert-deftest nlp-inline-ids-never-delegate ()
  (ndc--with-definitions
   (lambda (_)
     (dolist (c '((1 1 1 1) (109 1 1 1) (110 1 1 1) (10 2 2 2) (-16 1 1 1) (-15 3 3 3)
                  (27 0 127 2) (105 0 127 1) (108 0 127 2) (-14 2 2 2)))
       (let* ((m (nlp--mem (nth 0 c) (nth 1 c) (nth 2 c))) (calls (list 0))
              (nlp--runs nil) (nlp--run-status 0) (argc (nth 3 c)))
         ;; operands the inline arm declines on (non-list, non-vector, non-fixnum 77)
         (dotimes (i argc) (nlp--set m (+ nlp--fi 1 i) 77))
         (nlp--with m calls
           (nlp--call m argc)
           (should (null nlp--runs))))))))

(ert-deftest nlp-aref-inline-vector-and-record ()
  (ndc--with-definitions
   (lambda (_)
     ;; vector (header kind 8, 4 slots from the mock): slot 2 holds 999
     (let* ((m (nlp--mem -14 2 2)) (calls (list 0)) (nlp--runs nil))
       (nlp--set m 2 nlp--sym)
       (nlp--set m 3 nlp--vec) (nlp--set m 4 (+ (ash 2 2) 1))
       (puthash (+ nlp--vec -6 8 16) 999 m)
       (nlp--with m calls
         (should (= (nlp--call m 2) 0))
         (should (= (nlp--word m nlp--fi) 999))
         (should (= (nlp--word m 3) 3)) (should (= (nlp--word m 4) 3))
         (should (null nlp--runs))))
     ;; vector index out of range: declines through the delegate, which declines
     (let* ((m (nlp--mem -14 2 2)) (calls (list 0)) (nlp--runs nil) (nlp--run-status -3))
       (nlp--set m 2 nlp--sym)
       (nlp--set m 3 nlp--vec) (nlp--set m 4 (+ (ash 4 2) 1))
       (nlp--with m calls
         (should (= (nlp--call m 2) 2))
         (should (= (nlp--word m nlp--fi) nlp--sym))))
     ;; record (header kind 12, mocked length 3): index 0 = type word, K = slot K-1, 3 = last
     (dolist (c '((0 1111) (1 2000) (3 2002)))
       (let* ((m (nlp--mem -14 2 2)) (calls (list 0)) (nlp--runs nil))
         (nlp--set m 2 nlp--sym)
         (puthash (- nlp--vec 6) 12 m)
         (nlp--set m 3 nlp--vec) (nlp--set m 4 (+ (ash (car c) 2) 1))
         (nlp--with m calls
           (should (= (nlp--call m 2) 0))
           (should (= (nlp--word m nlp--fi) (cadr c))))))
     ;; record index past the slot count declines
     (let* ((m (nlp--mem -14 2 2)) (calls (list 0)) (nlp--runs nil) (nlp--run-status -3))
       (puthash (- nlp--vec 6) 12 m)
       (nlp--set m 3 nlp--vec) (nlp--set m 4 (+ (ash 4 2) 1))
       (nlp--with m calls
         (should (= (nlp--call m 2) 2)))))))

(ert-deftest nlp-call-subr-probes-once ()
  (let* ((f (ndc--find 'wf_bytecode_call_subr nelisp-standalone--applyfn-bytecode-helpers))
         (run (eval (ndc--host (cons 'lambda (cddr f))) t))
         (log nil) (leaf 0))
    (should f)
    (cl-letf (((symbol-function 'ptr-read-u64) (lambda (p i) (+ p i)))
              ((symbol-function 'wf_bytecode_call_leaf)
               (lambda (&rest a) (push (cons 'leaf a) log) leaf))
              ((symbol-function 'wf_bytecode_call)
               (lambda (&rest a) (push (cons 'call a) log) 42)))
      ;; leaf hit and leaf error both stop there
      (dolist (r '(0 1))
        (setq leaf r log nil)
        (should (= (funcall run 1000 5000 4 2 77) r))
        (should (= (length log) 1)))
      ;; a leaf decline takes the full call once, telling it the leaf was tried
      (setq leaf 2 log nil)
      (should (= (funcall run 1000 5000 4 2 77) 42))
      (should (equal (cdr (assq 'call log)) '(1000 5000 4 2 77 1)))
      ;; no function slot below SP: no leaf probe
      (setq leaf 0 log nil)
      (should (= (funcall run 1000 5000 1 2 77) 42))
      (should-not (assq 'leaf log)))))

(ert-deftest nlp-delegated-body-sees-live-operands-and-control ()
  ;; The delegated body must run while the operand words are still in the caller's
  ;; rooted frame (they are what keeps fresh arguments alive); the clear comes after.
  ;; Negative control: a leaf whose operand clear runs BEFORE the body is caught.
  (ndc--with-definitions
   (lambda (_)
     (let* ((m (nlp--mem 101 2 3)) (calls (list 0)) (nlp--runs nil) (nlp--run-status 0) (nlp--seen nil))
       (nlp--set m 2 nlp--sym)
       (nlp--set m 3 (+ (ash 7 2) 1)) (nlp--set m 4 (+ (ash 8 2) 1))
       (nlp--with m calls
         (should (= (nlp--call m 2) 0))
         (should (equal nlp--seen (list (+ (ash 7 2) 1) (+ (ash 8 2) 1)))))
       ;; control: swap the order of body and clear in a copy of the definition
       (let* ((f (ndc--find 'wf_bytecode_call_leaf (nelisp-standalone--directcall-defuns)))
              (src (prin1-to-string (cons 'lambda (cddr f))))
              (bad (replace-regexp-in-string
                    (regexp-quote "(setq st (nl_directcall_run id base argc out env))")
                    "(seq (ptr-write-u64 mem (* (+ fi 1) 8) 3) (setq st (nl_directcall_run id base argc out env)))" src t t))
              (run (eval (ndc--host (read bad)) t))
              (m2 (nlp--mem 101 2 3)) (calls2 (list 0)))
         (should-not (equal bad src))
         (nlp--set m2 2 nlp--sym)
         (nlp--set m2 3 (+ (ash 7 2) 1)) (nlp--set m2 4 (+ (ash 8 2) 1))
         (setq nlp--seen nil)
         (nlp--with m2 calls2
           (funcall run 1000 nlp--mem-base nlp--fi 2 nlp--sym)
           (should-not (equal nlp--seen (list (+ (ash 7 2) 1) (+ (ash 8 2) 1))))))))))

(provide 'nelisp-directcall-leafprim-test)
