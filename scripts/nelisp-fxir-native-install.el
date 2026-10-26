;;; nelisp-fxir-native-install.el --- native prelude closures (nativecomp N1)  -*- lexical-binding: t -*-

;; Spliced into the boot source after the prelude, with `nelisp-nc--table'
;; defined just before it (generated from lisp/nelisp-fxir-native-data.el).
;; A table row is (NAME ID CALLEES IR ARITY [:mutant]): the fxir native
;; function nc_ID was lowered from, the names its call nodes call and the
;; record's (MIN MAX REST-INDEX).  A live fx record
;; whose code compiles again to the same fxir with the same callees gets its
;; NLOCALS slot replaced by -(16+ID); the word VM then runs nc_ID.  The code
;; vector, its constants and call-site caches stay the live ones (read by the
;; native code where the VM reads them), so redefinition (a new closure body
;; has no record), advice and fset (calls go through the function cell) behave
;; as on the VM; only GNU's opcode primitives (car, eq, aref, ...) are inlined,
;; as the byte compiler does.  A mismatch leaves the record on the VM.  A
;; :mutant row (the negative control: a deliberately wrong `<') is never
;; installed here; `nelisp-nc-select' switches a record to it on request.

(defun nelisp-nc--recompile (fn)
  "(CODE NLOCALS IR) of closure FN compiled as `nelisp--fx-register-fn' does."
  (when (and (eq (car-safe fn) 'closure) (nelisp--fx-env-ok-p (nth 1 fn)))
    (let* ((args (nth 2 fn)) (body (nthcdr 3 fn))
           (nelisp--fx-declared (remq t (nth 1 fn)))
           (shape (nelisp--fx-arglist-shape args)))
      (when (and shape (not (cl-some #'nelisp--fx-special-p (car shape)))
                 (not (nelisp--fx-hopeless-p body)))
        (let* ((nelisp--fx-missing nil) (nelisp--fx-called nil) (inhibit-message t)
               (macros (nelisp--fx-macros-used body))
               (nelisp--fx-expand (and macros t))
               (nelisp--fx-outer-env (nth 1 fn))
               (nelisp--fx-want-ir t)
               (c (condition-case nil (nelisp--fx-compile (cons 'progn body) (car shape))
                    (error nil))))
          (when c (list (car c) (cdr c) (gethash (car c) nelisp--fx-ir-table))))))))

(defun nelisp-nc--code-same-p (a b)
  "Non-nil when code vectors A and B hold the same code (call sites by name)."
  (and (= (length a) (length b))
       (let ((i 0) (ok t))
         (while (and ok (< i (length a)))
           (let ((x (aref a i)) (y (aref b i)))
             (unless (if (and (vectorp x) (vectorp y) (> (length x) 0) (> (length y) 0))
                         (eq (aref x 0) (aref y 0))
                       (equal x y))
               (setq ok nil)))
           (setq i (1+ i)))
         ok)))

(defun nelisp-nc--callees (ir code)
  "Callee names of IR's call nodes in walk order, read from CODE."
  (let ((out nil))
    (cl-labels ((walk (n)
                  (when (consp n)
                    (cond ((eq (car n) :call)
                           (let ((c (aref code (nth 1 n))))
                             (push (if (vectorp c) (aref c 0) c) out))
                           (mapc #'walk (nthcdr 3 n)))
                          ((eq (car n) :inline) (mapc #'walk (nthcdr 3 n)))
                          ((eq (car n) :let)
                           (dolist (it (nth 1 n)) (walk (nth 2 it)))
                           (walk (nth 2 n)))
                          ((memq (car n) '(:const :slot :varref :alias)) nil)
                          ((eq (car n) :setslot) (walk (nth 2 n)))
                          (t (mapc #'walk (cdr n)))))))
      (walk (nth 1 ir)))
    (nreverse out)))

(defvar nelisp-nc--converted nil "Names whose fx record runs native code.")

(defun nelisp-nc--row (name &optional mutant)
  (let ((rows nelisp-nc--table) (found nil))
    (while (and rows (not found))
      (when (and (eq (car (car rows)) name)
                 (eq (and (memq :mutant (car rows)) t) (and mutant t)))
        (setq found (car rows)))
      (setq rows (cdr rows)))
    found))

(defun nelisp-nc--convert (name fn &optional mutant)
  "Switch NAME's fx record (closure FN) to its native function when the fxir matches."
  (let ((row (nelisp-nc--row name mutant)))
    (when (and row (eq (car-safe fn) 'closure))
      (let ((rec (gethash (nthcdr 3 fn) nelisp--fx-table)))
        (when (and (vectorp rec) (> (length rec) 7) (integerp (aref rec 1))
                   (not (= (aref rec 1) -1)))
          (let ((c (nelisp-nc--recompile fn)))
            (when (and c (nth 2 c)
                       (equal (list (aref rec 2) (aref rec 5) (aref rec 6)) (nth 4 row))
                       (nelisp-nc--code-same-p (car c) (aref rec 0))
                       (equal (nth 2 c) (nth 3 row))
                       (equal (nelisp-nc--callees (nth 2 c) (car c)) (nth 2 row)))
              (aset rec 1 (- -16 (nth 1 row)))
              (unless mutant (push name nelisp-nc--converted))
              t)))))))

(defun nelisp-nc-select (name mode)
  "Run NAME's record on MODE: `native', `vm' or `mutant'.  Answers non-nil on success.
For transcripts (tools/ai/nc1-native-check.sh): `vm' gives the record back
its compiled NLOCALS."
  (let* ((fn (symbol-function name))
         (rec (and (eq (car-safe fn) 'closure) (gethash (nthcdr 3 fn) nelisp--fx-table))))
    (when (vectorp rec)
      (if (eq mode 'vm)
          (let ((c (nelisp-nc--recompile fn)))
            (when c (aset rec 1 (cadr c)) t))
        (progn
          ;; Back to the VM first, so the comparison sees compiled NLOCALS.
          (let ((c (nelisp-nc--recompile fn))) (when c (aset rec 1 (cadr c))))
          (nelisp-nc--convert name fn (eq mode 'mutant)))))))
