;;; nelisp-interp-primitives-test.el --- Interpreter argv transport -*- lexical-binding: t; -*-

(require 'nelisp-bytecode-stackset-fast-test)

;; Generate the interpreter once per test load.  Its large admission table
;; does not change while these tests execute; rebuilding it per lookup hides
;; the cost of the operation the test is intended to exercise.
(defvar wip--definitions
  (append (nelisp-standalone--eval-call-defuns)
          (nelisp-standalone--subr-desc-defuns)))

(defun wip--function (name)
  "Return the production definition named NAME from the cached unit."
  (or (assq name (mapcar (lambda (f) (cons (cadr f) f)) wip--definitions))
      (error "Missing interpreter definition %s" name)))

(defun wip--call (name &rest args)
  "Run NAME's production IR using the existing checked host adapter."
  (let ((form (cdr (wip--function name))))
    (apply (eval (cons 'lambda (cddr (sst--host-form form))) t) args)))

(ert-deftest wip-interpreter-primitive-metadata ()
  (dolist (spec '(("aref" 27 2 2) ("aset" 28 3 3) ("length" 29 1 1)
                  ("%" 30 2 2) ("logand" 31 0 15)))
    (should (equal (assoc (car spec) nelisp-standalone--eval-prims) spec))
    (let* ((id (nth 1 spec)) (mn (nth 2 spec)) (mx (nth 3 spec))
           (arity (assoc (car spec) (nelisp-standalone--builtin-arities)))
           (desc (logior id (ash mn 8) (ash mx 12)
                         (ash (nth 1 arity) 16)
                         (ash (if (eq (nth 2 arity) 'many) 255 (nth 2 arity)) 24))))
      (should (= (wip--call 'nl_subr_prim desc mn) id))
      (should (= (wip--call 'nl_subr_arity_bad desc mn) 0))
      (when (> mn 0)
        (should (= (wip--call 'nl_subr_arity_bad desc (1- mn)) 1)))
      (unless (= mx 15)
        (should (= (wip--call 'nl_subr_arity_bad desc (1+ mx)) 1))))))

(ert-deftest wip-reuses-word-primitives-without-list-transport ()
  (let (calls writes)
    (cl-letf (((symbol-function 'm5_fx_primitive_run)
               (lambda (&rest args) (push args calls) 0))
              ((symbol-function 'nl_directcall_run)
               (lambda (&rest args) (push args calls) 0))
              ((symbol-function 'heap-read-u64)
               (lambda (&rest _) (error "Transport must forward the rooted argv")))
              ((symbol-function 'ptr-write-u64)
               (lambda (&rest args) (push args writes))))
      (dolist (spec '((27 2 -14) (28 3 -15) (29 1 -16) (30 2 -17)))
        (should (= (wip--call 'nl_eic_prim2_run (car spec) 4096 (cadr spec) 8192) 0))
        (should (equal (pop calls)
                       (list (nth 2 spec) 4096 (cadr spec) (cadr spec) 8192 0 0))))
      (dolist (argc '(0 1 2 6))
        (should (= (wip--call 'nl_eic_prim2_run 31 4096 argc 8192) 0))
        (should (equal (pop calls) (list 27 4096 argc 8192 0))))
      (should-not writes))))

(ert-deftest wip-decline-does-not-publish-or-convert-arguments ()
  (cl-letf (((symbol-function 'm5_fx_primitive_run) (lambda (&rest _) -3))
            ((symbol-function 'nl_directcall_run) (lambda (&rest _) -3))
            ((symbol-function 'heap-read-u64) (lambda (&rest _) 1))
            ((symbol-function 'bf_arrayp_raw) (lambda (_) 1))
            ((symbol-function 'ptr-write-u64) (lambda (&rest _) (error "Unexpected write"))))
    (dolist (id '(27 28 29 30 31))
      (should (= (wip--call 'nl_eic_prim2_run id 4096 2 8192) -3)))))

(ert-deftest wip-real-remainder-and-logand-bodies ()
  (let ((words (make-hash-table :test #'eql)))
    (cl-letf (((symbol-function 'heap-read-u64)
               (lambda (p off) (or (gethash (+ p off) words) (error "Bad argv read"))))
              ((symbol-function 'ptr-write-u64)
               (lambda (p off value) (puthash (+ p off) value words)))
              ((symbol-function 'nl_word_tag64) (lambda (_) 1))
              ((symbol-function 'm5_fx_primitive_run)
               (eval (cons 'lambda (cddr (sst--host-form
                      (sst--find nelisp-cc-fx-call-primitives--source
                       (lambda (f) (and (eq (car-safe f) 'defun)
                                        (eq (cadr f) 'm5_fx_primitive_run))))))) t))
              ((symbol-function 'nl_directcall_run)
               (eval (cons 'lambda (cddr (sst--host-form
                                        (cdr (wip--function 'nl_directcall_run))))) t)))
      (dolist (row '((30 (-13 4) -1) (30 (13 -4) 1) (30 (-13 -4) -1)
                     (31 (15 7 3) 3) (31 (-1 7) 7) (31 (-1) -1)))
        (cl-loop for value in (cadr row) for i from 0
                 do (puthash (+ 4096 (* i 8)) (1+ (* value 4)) words))
        (should (= (wip--call 'nl_eic_prim2_run (car row) 4096 (length (cadr row)) 8192) 0))
        (should (= (gethash 8192 words) (1+ (* 4 (nth 2 row)))))))))

(ert-deftest wip-aset-invalid-array-keeps-gnu-condition-data ()
  (let (offenders)
    (cl-letf (((symbol-function 'm5_fx_primitive_run) (lambda (&rest _) -3))
              ((symbol-function 'heap-read-u64)
               (lambda (p off) (if (= (+ p off) 4096) 37 1)))
              ((symbol-function 'bf_arrayp_raw) (lambda (_) 0))
              ((symbol-function 'nl_word_tag64) (lambda (_) 2))
              ((symbol-function 'wf_word_view) #'identity)
              ((symbol-function 'bf_wrong_type_arrayp)
               (lambda (value) (push value offenders) 1))
              ((symbol-function 'ptr-write-u64) (lambda (&rest _) (error "Unexpected store"))))
      (should (= (wip--call 'nl_eic_prim2_run 28 4096 3 8192) 1))
      (should (equal offenders '(37))))))

(ert-deftest wip-aset-decline-preserves-index-and-adapter-errors ()
  (dolist (row '((3 2) (1 9) (1 12)))
    (cl-letf (((symbol-function 'm5_fx_primitive_run) (lambda (&rest _) -3))
              ((symbol-function 'heap-read-u64)
               (lambda (p off) (if (= (+ p off) 4096) 8198 (car row))))
              ((symbol-function 'bf_arrayp_raw) (lambda (_) 0))
              ((symbol-function 'nl_word_tag64) (lambda (_) (cadr row)))
              ((symbol-function 'wf_word_view) (lambda (_) (error "Unexpected conversion")))
              ((symbol-function 'bf_wrong_type_arrayp) (lambda (_) (error "Wrong error precedence"))))
      (should (= (wip--call 'nl_eic_prim2_run 28 4096 3 8192) -3)))))

(defun wip--descriptor (name)
  "Run the real name dispatcher, adapting only physical symbol metadata."
  (let ((form (cl-letf (((symbol-function 'nelisp-standalone--sid-read-form)
                         (lambda (w) (list 'wip--sid w))))
                (car (nelisp-standalone--subr-desc-defuns)))))
    (cl-letf (((symbol-function 'wip--sid) (lambda (n) (or (nelisp-standalone--sid n) 0)))
              ((symbol-function 'sexp-tag) (lambda (_) 4))
              ((symbol-function 'nl_word_length) #'length)
              ((symbol-function 'sexp-name-eq) (lambda (a b) (if (equal a b) 1 0))))
      (funcall (eval (cons 'lambda (cddr (sst--host-form form))) t) name))))

(ert-deftest wip-name-descriptor-includes-sidless-primitives ()
  (dolist (spec '(("aref" . 27) ("aset" . 28) ("length" . 29) ("%" . 30) ("logand" . 31)))
    (should (= (logand (wip--descriptor (car spec)) 255) (cdr spec))))
  (should (= (wip--descriptor "wip-unknown-builtin") 0)))

(provide 'nelisp-interp-primitives-test)
;;; nelisp-interp-primitives-test.el ends here
