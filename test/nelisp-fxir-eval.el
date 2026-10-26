;;; nelisp-fxir-eval.el --- reference evaluator for fxir  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; nativecomp N0 checker.  Runs a function from the fxir that
;; `nelisp--fx-compile' builds beside its code vector (grammar above
;; `nelisp--fx-want-ir' in scripts/nelisp-stdlib-prelude.el), reading every
;; constant, symbol and call-site cache from the live code vector at the index
;; the node names, as the word VM does.  Calls go through the function cell
;; (`apply' of the call site's symbol), the fixnum ops call their function,
;; and a VARBIND binds dynamically around the rest of its `let'.
;;
;; `nelisp-fxir-install' replaces a function's cell by a wrapper that runs the
;; fxir of its fx record, so a transcript taken with it can be compared with
;; the transcripts of the VM and of the interpreter (tools/ai/nc1-fxir-check.sh).
;; Loading this file into a binary whose prelude predates fxir first loads the
;; compiler from scripts/nelisp-stdlib-prelude.el (a hot patch, as lane
;; fxadopt measured its compiler).
;;
;; NELISP_FXIR_MUTANT=1 makes the evaluator deliberately wrong (`:or' answers
;; its last operand), the negative control: the transcript must then differ.

;;; Code:

(defvar nelisp-fxir--mutant (equal (getenv "NELISP_FXIR_MUTANT") "1"))

(unless (boundp 'nelisp--fx-ir-table)
  ;; Hot patch: the prelude's own fxir definitions, read from the source.
  (let ((file (expand-file-name "scripts/nelisp-stdlib-prelude.el"
                                (or (getenv "NELISP_FXIR_ROOT") default-directory)))
        (want '(nelisp--fx-want-ir nelisp--fx-ir-table nelisp--fx-compile)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((form t))
        (while (and want (setq form (condition-case nil (read (current-buffer)) (end-of-file nil))))
          (when (and (memq (car-safe form) '(defvar defun)) (memq (nth 1 form) want))
            (eval form t)
            (setq want (delq (nth 1 form) want))))))
    (when want (error "fxir hot patch: not found %S" want))))

(defun nelisp-fxir--ev (n code slots)
  "Value of fxir node N; CODE is the live code vector, SLOTS the locals."
  (let ((k (car n)))
    (cond
     ((eq k :const) (aref code (nth 1 n)))
     ((eq k :slot) (aref slots (nth 1 n)))
     ((eq k :varref) (symbol-value (aref code (nth 1 n))))
     ((eq k :alias) (symbol-value (aref (aref code (nth 1 n)) 0)))
     ((eq k :setslot) (aset slots (nth 1 n) (nelisp-fxir--ev (nth 2 n) code slots)))
     ((eq k :if) (if (nelisp-fxir--ev (nth 1 n) code slots)
                     (nelisp-fxir--ev (nth 2 n) code slots)
                   (nelisp-fxir--ev (nth 3 n) code slots)))
     ((eq k :seq) (let ((r nil)) (dolist (x (cdr n)) (setq r (nelisp-fxir--ev x code slots))) r))
     ((eq k :prog1) (prog1 (nelisp-fxir--ev (nth 1 n) code slots)
                      (dolist (x (nthcdr 2 n)) (nelisp-fxir--ev x code slots))))
     ((eq k :and) (let ((r t) (xs (cdr n)))
                    (while (and xs r) (setq r (nelisp-fxir--ev (car xs) code slots) xs (cdr xs)))
                    r))
     ((eq k :or) (let ((r nil) (xs (cdr n)))
                   (while (and xs (or nelisp-fxir--mutant (not r)))
                     (setq r (nelisp-fxir--ev (car xs) code slots) xs (cdr xs)))
                   r))
     ((eq k :while) (while (nelisp-fxir--ev (nth 1 n) code slots)
                      (nelisp-fxir--ev (nth 2 n) code slots))
      nil)
     ((eq k :let) (nelisp-fxir--let (nth 1 n) (nth 2 n) code slots))
     ((memq k '(:call :inline))
      (let ((f (aref code (nth (if (eq k :call) 1 2) n))) (args nil))
        (dolist (x (nthcdr 3 n)) (push (nelisp-fxir--ev x code slots) args))
        (apply (if (vectorp f) (aref f 0) f) (nreverse args))))
     (t (error "fxir: bad node %S" n)))))

(defun nelisp-fxir--let (items body code slots)
  "Run the binding ITEMS of a `:let' in order, then BODY."
  (if (null items) (nelisp-fxir--ev body code slots)
    (let* ((it (car items)) (v (nelisp-fxir--ev (nth 2 it) code slots)))
      (if (eq (car it) :store)
          (progn (aset slots (nth 1 it) v)
                 (nelisp-fxir--let (cdr items) body code slots))
        ;; :varbind -- dynamic, around the rest of the `let'.
        (eval (list 'let (list (list (aref code (nth 1 it)) (list 'quote v)))
                    (list 'funcall (list 'quote (lambda () (nelisp-fxir--let (cdr items) body code slots)))))
              nil)))))

(defun nelisp-fxir-run (rec ir args orig)
  "Apply the fx record REC through its fxir IR to ARGS; ORIG when the VM would not."
  (let* ((code (aref rec 0)) (nloc (max 0 (aref rec 1))) (min (aref rec 2)) (max (aref rec 5))
         (rest (aref rec 6)) (n (length args)) (c (aref code 1)) (decline nil))
    ;; ENTRY-CHECK: a lexical name of the body turned special declines.
    (when (vectorp c)
      (let ((i 2))
        (while (< i (length c))
          (when (special-variable-p (aref c i)) (setq decline t))
          (setq i (1+ i)))))
    ;; A retired record (`nelisp--fx-retire': a callee became a macro, or
    ;; the body was recompiled) no longer starts; the closure runs as usual.
    (if (or decline (aref rec 7) (< n min) (and (>= max 0) (> n max)))
        (apply orig args)
      (let ((slots (make-vector nloc nil)) (i 0) (a args))
        (while (and a (or (< rest 0) (< i rest)))
          (aset slots i (car a)) (setq a (cdr a) i (1+ i)))
        (when (>= rest 0) (aset slots rest (copy-sequence a)))
        (nelisp-fxir--ev (nth 1 ir) code slots)))))

(defvar nelisp-fxir-installed nil "Names whose cells run their fxir.")

(defun nelisp-fxir-install (name)
  "Make NAME's function cell run its fx record's fxir; nil when it has none."
  (let* ((fn (symbol-function name))
         (rec (and (eq (car-safe fn) 'closure) (gethash (nthcdr 3 fn) nelisp--fx-table)))
         ;; NLOCALS -1 is a compiler lowering (string compare), not fx code.
         (ir (and (vectorp rec) (/= (aref rec 1) -1)
                  (gethash (aref rec 0) nelisp--fx-ir-table))))
    (when ir
      ;; The wrapper keeps the closure's interactive form (`commandp').
      (let ((spec (and (fboundp 'interactive-form) (interactive-form fn))))
        (fset name (if spec
                       (eval (list 'lambda '(&rest args) spec
                                   (list 'nelisp-fxir-run (list 'quote rec) (list 'quote ir)
                                         'args (list 'quote fn)))
                             t)
                     (lambda (&rest args) (nelisp-fxir-run rec ir args fn)))))
      (push name nelisp-fxir-installed)
      t)))

;; ---- structural checks and harvest helpers ----

(defun nelisp-fxir-recompile (fn)
  "Compile closure FN as `nelisp--fx-register-fn' does, without touching the table.
Answers (CODE NLOCALS IR) or nil."
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

(defun nelisp-fxir-code-same-p (a b)
  "Non-nil when code vectors A and B are the same code (call-site caches by name)."
  (and (= (length a) (length b))
       (let ((i 0) (ok t))
         (while (and ok (< i (length a)))
           (let ((x (aref a i)) (y (aref b i)))
             (unless (if (and (vectorp x) (vectorp y) (> (length x) 0) (> (length y) 0))
                         (equal (aref x 0) (aref y 0))
                       (equal x y))
               (setq ok nil)))
           (setq i (1+ i)))
         ok)))

(defun nelisp-fxir-problems (ir code nlocals)
  "Operand indices of IR that do not name the CODE word the VM reads."
  (let ((bad nil) (expect '((:const . 1) (:varref . 19) (:alias . 18) (:varbind . 20))))
    (cl-labels
        ((op-at (k op) (unless (and (integerp k) (> k 0) (< k (length code))
                                    (eql (aref code (1- k)) op))
                         (push (list op k) bad)))
         (slot (n) (unless (and (integerp n) (>= n 0) (< n (max nlocals 1))) (push (list :slot n) bad)))
         (walk (n)
           (let ((k (car n)))
             (cond
              ((assq k expect) (op-at (nth 1 n) (cdr (assq k expect)))
               (when (eq k :varbind) (walk (nth 2 n))))
              ((eq k :slot) (slot (nth 1 n)))
              ((eq k :setslot) (slot (nth 1 n)) (walk (nth 2 n)))
              ((eq k :call) (op-at (nth 1 n) 16)
               (unless (eql (aref code (1+ (nth 1 n))) (nth 2 n)) (push (list :argc n) bad))
               (unless (= (length (nthcdr 3 n)) (nth 2 n)) (push (list :nargs (nth 1 n)) bad))
               (mapc #'walk (nthcdr 3 n)))
              ((eq k :inline) (op-at (nth 2 n) (nth 1 n)) (mapc #'walk (nthcdr 3 n)))
              ((eq k :let)
               (dolist (it (nth 1 n))
                 (if (eq (car it) :store) (progn (slot (nth 1 it)) (walk (nth 2 it))) (walk it)))
               (walk (nth 2 n)))
              ((eq k :if) (walk (nth 1 n)) (walk (nth 2 n)) (walk (nth 3 n)))
              ((eq k :while) (walk (nth 1 n)) (walk (nth 2 n)))
              ((memq k '(:seq :prog1 :and :or)) (mapc #'walk (cdr n)))
              (t (push (list :node k) bad))))))
      (if (eq (car-safe ir) :fn) (walk (nth 1 ir)) (push :no-fn bad)))
    bad))

(defun nelisp-fxir-callees (ir code)
  "Callee names of IR's call and inline nodes in walk order, read from CODE."
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

(provide 'nelisp-fxir-eval)
;;; nelisp-fxir-eval.el ends here
