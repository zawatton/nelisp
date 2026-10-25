;;; nelisp-directcall-test.el --- Validated primitive argv boundary -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defun ndc--find (name forms)
  (cl-find-if (lambda (f) (and (eq (car-safe f) 'defun) (eq (cadr f) name))) forms))

(defun ndc--contains (symbol form)
  (or (eq form symbol) (and (consp form) (or (ndc--contains symbol (car form))
                                           (ndc--contains symbol (cdr form))))))

(defvar ndc--descriptors nil)

(defun ndc--host (form)
  "Adapt IR syntax/truth and raw shifts, retaining the production algorithm."
  (cond ((atom form) form)
        ((eq (car form) 'data-addr) 'ndc--descriptors)
        ;; Binding initialisers are code too (cbfast: `sar', 0/1 truth).
        ((memq (car form) '(let let*))
         (cons (car form)
               (cons (mapcar (lambda (b) (if (consp b) (list (car b) (ndc--host (cadr b))) b))
                             (cadr form))
                     (mapcar #'ndc--host (cddr form)))))
        ((memq (car form) '(if while))
         (cons (car form) (cons `(ndc--truth ,(ndc--host (cadr form)))
                               (mapcar #'ndc--host (cddr form)))))
        (t (cons (pcase (car form) ('seq 'progn) ('shl 'ash) ('sar 'ndc--sar)
                        (_ (car form)))
                 (mapcar #'ndc--host (cdr form))))))
(defun ndc--truth (v) (and v (not (equal v 0))))
(defun ndc--sar (v n) (ash v (- n)))

(defun ndc--with-definitions (thunk)
  (should (fboundp 'nelisp-standalone--directcall-defuns))
  (let* ((forms (nelisp-standalone--directcall-defuns))
         (ndc--descriptors (nth 2 (cl-find-if (lambda (f) (eq (car f) 'data-blob)) forms)))
         (defs (cl-remove-if-not (lambda (f) (eq (car f) 'defun)) forms))
         (old (mapcar (lambda (f) (cons (cadr f) (and (fboundp (cadr f))
                                                     (symbol-function (cadr f))))) defs)))
    (unwind-protect
        (cl-letf (((symbol-function 'heap-read-u32)
                   (lambda (table offset)
                     (let ((v 0))
                       (dotimes (i 4 v)
                         (setq v (logior v (ash (aref table (+ offset i)) (* i 8)))))))))
          (dolist (f defs) (eval (ndc--host f) t)) (funcall thunk defs))
      (dolist (p old) (if (cdr p) (fset (car p) (cdr p)) (fmakunbound (car p)))))))

(ert-deftest ndc-a-bytecode-uses-current-cell-before-list-allocation ()
  (let ((f (ndc--find 'wf_bytecode_call nelisp-standalone--applyfn-bytecode-helpers)))
    (should f)
    (should (ndc--contains 'nelisp_env_lookup_function_raw_w f))
    (should (ndc--contains 'nl_directcall_invoke f))
    (should (ndc--contains 'wf_bytecode_w_call_words f))))

(ert-deftest ndc-a-fixnum-fold-declines-without-result-effect ()
  (ndc--with-definitions
   (lambda (_defs)
     (let ((mem (make-hash-table :test #'eql)) (run-count 0))
       (cl-letf (((symbol-function 'heap-read-u64) (lambda (p i) (gethash (+ p i) mem)))
                 ((symbol-function 'ptr-write-u64) (lambda (p i v) (puthash (+ p i) v mem)))
                 ((symbol-function 'm5_fx_primitive_run) (lambda (&rest _) (cl-incf run-count) -3)))
         (puthash 100 29 mem) (puthash 108 13 mem) (puthash 200 999 mem)
         (should (= (nl_directcall_run 27 100 2 200 0) 0))
         (should (= (gethash 200 mem) 13))
         (puthash 108 3 mem) (puthash 200 999 mem)
         (should (= (nl_directcall_run 27 100 2 200 0) -3))
         (should (= (gethash 200 mem) 999))
         (should (= (nl_directcall_run 27 100 0 200 0) -3))
         (should (= (gethash 200 mem) 999))
         ;; `car' (id 1) has a WORD body of its own; `nth' (id 10) still declines
         ;; through the reference primitive runner.
         (should (= (nl_directcall_run 10 100 1 200 0) -3))
         (should (= run-count 1)))))))

(ert-deftest ndc-a-descriptor-validates-name-shape-and-arity ()
  (ndc--with-definitions
   (lambda (_defs)
     (let ((mem (make-hash-table :test #'eql)) (names (make-hash-table :test #'eql)))
       (puthash 10000 20006 mem) (puthash 10008 10018 mem)
       (puthash 10016 20022 mem) (puthash 10024 3 mem)
       (puthash 20006 "builtin" names) (puthash 20022 "car" names)
       (puthash 20038 "length" names)
       (cl-letf (((symbol-function 'ptr-read-u64) (lambda (p i) (gethash (+ p i) mem)))
                 ((symbol-function 'nl_sym_sid) (lambda (w) (or (nelisp-standalone--sid (gethash w names)) 0)))
                 ((symbol-function 'nl_val_tag) (lambda (w) (if (gethash w names) 4 (if (= w 3) 0 7))))
                 ((symbol-function 'sexp-name-eq) (lambda (w s) (if (equal (gethash w names) s) 1 0))))
         (should (= (nl_directcall_id 10002 20022 1) 1))
         (should (= (nl_directcall_id 10002 20022 2) 0))
         (should (= (nl_directcall_id 10002 20038 1) 0))
         (puthash 10024 10002 mem)
         (should (= (nl_directcall_id 10002 20022 1) 0))
         (puthash 10024 3 mem) (puthash 20006 "closure" names)
         (should (= (nl_directcall_id 10002 20022 1) 0)))))))

(ert-deftest ndc-a-owned-helper-closure-is-emitted ()
  (should (fboundp 'nelisp-standalone--directcall-defuns))
  (let ((forms (nelisp-standalone--eval-call-defuns)))
    (dolist (n '(nl_directcall_id nl_directcall_run nl_directcall_invoke))
      (should (ndc--find n forms)))))

(ert-deftest ndc-a-missing-function-sentinel-never-dereferenced ()
  (ndc--with-definitions
   (lambda (_defs)
     (cl-letf (((symbol-function 'ptr-read-u64) (lambda (&rest _) (error "Invalid pointer read"))))
       (dolist (w '(0 2 3 10))
         (should (= (nl_directcall_id w 20022 1) 0)))))))

(ert-deftest ndc-a-bytecode-reads-the-symbol-objects-function-slot-before-the-mirror ()
  ;; dcproto: a symbol OBJECT (low bits 110) keeps its function at +24, which is
  ;; what the mirror lookup answers for it; only another spelling, or a slot that
  ;; is not a cons, takes the lookup.
  (let* ((f (ndc--find 'wf_bytecode_call nelisp-standalone--applyfn-bytecode-helpers))
         (run (eval (ndc--host (cons 'lambda (cddr f))) t))
         (mem (make-hash-table :test #'eql)) (lookups 0) (invoked nil))
    (cl-letf (((symbol-function 'heap-read-u64) (lambda (p i) (gethash (+ p i) mem)))
              ((symbol-function 'ptr-write-u64) (lambda (p i v) (puthash (+ p i) v mem)))
              ;; leafprim: the leaf arm declines, so the unchanged route below is the one tested.
              ((symbol-function 'wf_bytecode_call_leaf) (lambda (&rest _) 2))
              ((symbol-function 'nl_val_tag) (lambda (w) (if (memq w '(20022 20024)) 4 7)))
              ((symbol-function 'nelisp_env_lookup_function_raw_w)
               (lambda (&rest _) (cl-incf lookups) 10002))
              ((symbol-function 'nl_directcall_invoke)
               (lambda (fn original _base _argc _env out)
                 (setq invoked (list fn original)) (puthash out 13 mem) 0)))
      (puthash 20040 10002 mem)        ; function slot of the object 20022 (= 20016 + 24)
      ;; An object whose slot is a cons: no lookup.
      (puthash 100 20022 mem) (puthash 108 29 mem)
      (should (= (funcall run 200 100 2 1 0) 0))
      (should (= lookups 0))
      (should (equal invoked '(10002 20022)))
      ;; A view spelling (low bits 000), and an object whose slot is not a cons:
      ;; the mirror lookup answers.
      (puthash 100 20024 mem) (puthash 108 29 mem)
      (should (= (funcall run 200 100 2 1 0) 0))
      (should (= lookups 1))
      (puthash 20040 12 mem) (puthash 100 20022 mem) (puthash 108 29 mem)
      (should (= (funcall run 200 100 2 1 0) 0))
      (should (= lookups 2)))))

(ert-deftest ndc-a-success-clears-dead-operands-but-error-retains-them ()
  (let* ((f (ndc--find 'wf_bytecode_call nelisp-standalone--applyfn-bytecode-helpers))
         (run (eval (ndc--host (cons 'lambda (cddr f))) t))
         (mem (make-hash-table :test #'eql)) (status 0))
    (cl-letf (((symbol-function 'heap-read-u64) (lambda (p i) (gethash (+ p i) mem)))
              ((symbol-function 'ptr-write-u64) (lambda (p i v) (puthash (+ p i) v mem)))
              ((symbol-function 'wf_bytecode_call_leaf) (lambda (&rest _) 2))
              ((symbol-function 'nl_val_tag) (lambda (w) (if (= w 20022) 4 7)))
              ;; (the function slot of the object 20022 is not a cons here)
              ((symbol-function 'nelisp_env_lookup_function_raw_w) (lambda (&rest _) 10002))
              ((symbol-function 'nl_directcall_invoke)
               (lambda (_fn _original _base _argc _env out)
                 (when (= status 0) (puthash out 13 mem)) status)))
      (dolist (st '(0 1))
        (setq status st)
        (puthash 20040 0 mem)
        (puthash 100 20022 mem) (puthash 108 29 mem) (puthash 116 13 mem)
        (puthash 124 999 mem)
        (should (= (funcall run 200 100 3 2 0) st))
        (should (= (gethash 108 mem) (if (= st 0) 3 29)))
        (should (= (gethash 116 mem) (if (= st 0) 3 13)))
        (should (= (gethash 124 mem) 999))))))

(ert-deftest ndc-b-evaluated-funcall-and-apply-use-argv-before-fallback ()
  (let ((forms (cdr nelisp-standalone--combiner-cons-word)))
    (should (ndc--contains 'nl_directcall_invoke
                          (ndc--find 'nl_eic_funcall_words forms)))
    (should (ndc--contains 'nl_directcall_apply_words
                          (ndc--find 'nl_eic_apply_words forms)))))

(ert-deftest ndc-b-legacy-boundaries-use-private-argv-adapter ()
  (require 'nelisp-cc-evalport-combiner-apply)
  ;; The full provider's fset body needs earlier, unrelated owner patches.
  ;; Exercise these two real boundary bodies through their actual n4 emitter.
  (let* ((boundaries (cl-remove-if-not
                      (lambda (f) (memq (cadr f) '(nl_apply_do_funcall nl_apply_do_apply)))
                      (cdr nelisp-cc-evalport-combiner-apply--source)))
         (forms (cdr (nelisp-standalone--patch-n4-apply (cons 'seq boundaries)))))
    (should (= (length boundaries) 2))
    (dolist (n '(nl_apply_do_funcall nl_apply_do_apply))
      (should (ndc--contains 'nl_directcall_list_call (ndc--find n forms))))))

(ert-deftest ndc-b-production-word-boundaries-use-private-argv-adapter ()
  ;; These are the entries the final word-ABI builtin dispatcher calls.
  (dolist (n '(nl_apply_do_funcall_w nl_apply_do_apply_w))
    (let ((f (ndc--find n nelisp-standalone--rooted-subr-helpers)))
      (should f)
      (should (ndc--contains 'nl_directcall_list_words f)))))

(ert-deftest ndc-b-ineligible-function-declines-before-argument-frame ()
  (ndc--with-definitions
   (lambda (_defs)
     (let ((frames 0))
       (cl-letf (((symbol-function 'nl_sp_nargs) (lambda (_) 2))
                 ((symbol-function 'nl_cons_car_word) (lambda (_) 20022))
                 ((symbol-function 'nl_val_tag) (lambda (_) 4))
                 ((symbol-function 'nelisp_env_lookup_function_raw_w) (lambda (&rest _) 2))
                 ((symbol-function 'nl_root_mark) (lambda (_) 0))
                 ((symbol-function 'nl_root_frame) (lambda (&rest _) (cl-incf frames) 0)))
         (should (= (nl_directcall_list_words 100 200 0) 2))
         (should (= frames 0)))))))

(ert-deftest ndc-b-apply-validates-spread-before-function-lookup ()
  (ndc--with-definitions
   (lambda (defs)
     (should (ndc--find 'nl_directcall_apply_words defs))
     (let ((lookups 0))
       (cl-letf (((symbol-function 'heap-read-u64) (lambda (&rest _) 555))
                 ((symbol-function 'nl_eic_apply_list_ok) (lambda (_) 0))
                 ((symbol-function 'nelisp_env_lookup_function_raw_w)
                  (lambda (&rest _) (cl-incf lookups) 0)))
         (should (= (nl_directcall_apply_words 100 2 200) 2))
         (should (= lookups 0)))))))


(provide 'nelisp-directcall-test)
