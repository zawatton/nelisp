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

;; Shared function-information provider (validated source parts).  Same root
;; resolution as the hot patch above, with this file's own checkout as the
;; fallback; it is a plain source load, so no cache is built.
(unless (fboundp 'nelisp-function--source-parts)
  (let ((file (expand-file-name
               "lisp/nelisp-function-info.el"
               (or (getenv "NELISP_FXIR_ROOT")
                   (and load-file-name
                        (expand-file-name ".." (file-name-directory load-file-name)))
                   default-directory))))
    (load file nil t)
    (unless (fboundp 'nelisp-function--source-parts)
      (error "fxir: function-info provider missing: %s" file))))

(defconst nelisp-fxir--dynamic-slot (make-symbol "fxir-dynamic-slot"))

(defun nelisp-fxir--ev (n code slots)
  "Value of fxir node N; CODE is the live code vector, SLOTS the locals."
  (let ((k (car n)))
    (cond
     ((eq k :const) (aref code (nth 1 n)))
     ((eq k :slot) (aref slots (nth 1 n)))
     ((eq k :varref) (symbol-value (aref code (nth 1 n))))
     ((eq k :alias) (symbol-value (aref (aref code (nth 1 n)) 0)))
     ((eq k :setslot) (aset slots (nth 1 n) (nelisp-fxir--ev (nth 2 n) code slots)))
     ((eq k :late-ref)
      (let ((value (aref slots (nth 1 n))))
        (cond ((eq value nelisp-fxir--dynamic-slot)
               (symbol-value (aref code (nth 2 n))))
              ((nth 3 n) (cdr value))
              (t value))))
     ((eq k :late-set)
      (let ((value (nelisp-fxir--ev (nth 2 n) code slots))
            (slot (nth 1 n))
            (old (aref slots (nth 1 n))))
        (cond ((eq old nelisp-fxir--dynamic-slot)
               (set (aref code (nth 3 n)) value))
              ((nth 4 n) (setcdr old value))
              (t (aset slots slot value)))))
     ((eq k :late-special-p)
      (eq (aref slots (nth 1 n)) nelisp-fxir--dynamic-slot))
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
     ;; nativecomp N3 handlers, by the host's own special forms.
     ((eq k :catch) (catch (nelisp-fxir--ev (nth 1 n) code slots)
                      (nelisp-fxir--ev (nth 2 n) code slots)))
     ((eq k :throw) (throw (nelisp-fxir--ev (nth 1 n) code slots)
                           (nelisp-fxir--ev (nth 2 n) code slots)))
     ((eq k :unwind-protect) (unwind-protect (nelisp-fxir--ev (nth 1 n) code slots)
                               (nelisp-fxir--ev (nth 2 n) code slots)))
     ((eq k :condition-case) (nelisp-fxir--condition-case n code slots))
     ((memq k '(:call :inline))
      (let ((f (aref code (nth (if (eq k :call) 1 2) n))) (args nil))
        (dolist (x (nthcdr 3 n)) (push (nelisp-fxir--ev x code slots) args))
        (apply (if (vectorp f) (aref f 0) f) (nreverse args))))
     (t (error "fxir: bad node %S" n)))))

(defun nelisp-fxir--let (items body code slots)
  "Run the binding ITEMS of a `:let' in order, then BODY."
  (if (null items) (nelisp-fxir--ev body code slots)
    (let* ((it (car items))
           (v (nelisp-fxir--ev (nth 2 it) code slots))
           (late (eq (car it) :late-bind))
           (slot (nth 1 it))
           (name (cond (late (aref code (nth 3 it)))
                       ((eq (car it) :varbind) (aref code slot)))))
      (if (or (eq (car it) :store) (and late (not (special-variable-p name))))
          (progn (aset slots (nth 1 it) v)
                 (nelisp-fxir--let (cdr items) body code slots))
        (when late
          (when (nth 4 it) (setq v (cdr v)))
          (aset slots slot nelisp-fxir--dynamic-slot))
        (eval (list 'let (list (list name (list 'quote v)))
                    (list 'funcall (list 'quote (lambda () (nelisp-fxir--let (cdr items) body code slots)))))
              nil)))))

(defun nelisp-fxir--spec-matches (spec sig)
  "Non-nil when condition SPEC (a clause's car) handles signal SIG."
  (let ((conds (get sig 'error-conditions)))
    (cond ((eq spec t) t)
          ((symbolp spec) (and spec (or (eq spec sig) (memq spec conds))))
          (t (let ((hit nil))
               (dolist (s spec) (when (memq s conds) (setq hit t)))
               hit)))))

(defun nelisp-fxir--condition-case (n code slots)
  "Run (:condition-case VS X S (K H)...): clauses tried in order, else re-signal."
  (let* ((vs (nth 1 n)) (done nil)
         (v (condition-case err
                (prog1 (nelisp-fxir--ev (nth 2 n) code slots) (setq done t))
              (t (let ((cl (nthcdr 4 n)) (hit nil))
                   (while (and cl (not hit))
                     (when (nelisp-fxir--spec-matches (aref code (car (car cl))) (car err))
                       (setq hit (car cl)))
                     (setq cl (cdr cl)))
                   (unless hit (signal (car err) (cdr err)))
                   (when vs (aset slots vs err))
                   (nelisp-fxir--ev (nth 1 hit) code slots))))))
    (if (and done (nth 3 n))
        (progn (when vs (aset slots vs v)) (nelisp-fxir--ev (nth 3 n) code slots))
      v)))

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
         ;; Validated source parts; bytecode, subrs and malformed shapes give nil.
         ;; :body is the live body list, the same object the table is keyed by.
         (parts (and fn (not (symbolp fn)) (nelisp-function--source-parts fn)))
         (rec (and (memq (plist-get parts :representation)
                         '(legacy-closure interpreted-function))
                   (gethash (plist-get parts :body) nelisp--fx-table)))
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

(defun nelisp-fxir-analyze (function)
  "Analyze FUNCTION once, as `nelisp--fx-register-fn' would compile it.
Return a versioned plist: :schema 1, :status `accepted' or `rejected',
:reason (nil when accepted, else the failed guard or the error symbol),
:condition (the original condition data, or nil), :node (always nil: no
guard names a source node), and on acceptance :code :nlocals :ir :callees.
This is the only admission and compile path; nothing is installed or
published, and the fx table is not touched."
  (let* ((parts (nelisp-function--source-parts function))
         (env (plist-get parts :environment))
         (body (plist-get parts :body))
         (shape nil) (reason nil) (condition nil) (c nil))
    (cond
     ((null parts)
      ;; No validated source: bytecode, subr, unresolved, malformed.
      (let ((info (nelisp-function-info function)))
        (setq reason (or (plist-get info :reason) (plist-get info :kind)
                         'not-a-function))))
     ((not (nelisp--fx-env-ok-p env)) (setq reason 'unsupported-environment))
     (t
      (let ((nelisp--fx-declared (remq t env)))
        (setq shape (nelisp--fx-arglist-shape (plist-get parts :arglist)))
        (cond
         ((null shape) (setq reason 'unsupported-arglist))
         ((cl-some #'nelisp--fx-special-p (car shape))
          (setq reason 'special-arguments))
         ((nelisp--fx-hopeless-p body) (setq reason 'unsupported-body))
         (t
          (let* ((nelisp--fx-missing nil) (nelisp--fx-called nil) (inhibit-message t)
                 (macros (nelisp--fx-macros-used body))
                 (nelisp--fx-expand (and macros t))
                 (nelisp--fx-outer-env env)
                 (nelisp--fx-want-ir t))
            (condition-case err
                (progn
                  ;; Keep the original body for source identity, but compile
                  ;; the same executable forms as `nelisp--fx-register-fn'.
                  (let ((forms (if (and (stringp (car body)) (cdr body))
                                   (cdr body)
                                 body)))
                    (setq c (nelisp--fx-compile (cons 'progn forms) (car shape))))
                  (unless c (setq reason 'compiler-returned-nil)))
              (error (setq reason (car err) condition err)))))))))
    (if (and c (null reason))
        (let ((ir (gethash (car c) nelisp--fx-ir-table)))
          (list :schema 1 :status 'accepted :reason nil :condition nil :node nil
                :code (car c) :nlocals (cdr c) :ir ir
                :callees (nelisp-fxir-callees ir (car c))))
      (list :schema 1 :status 'rejected :reason reason :condition condition
            :node nil))))

(defun nelisp-fxir-recompile (fn)
  "Compile closure FN as `nelisp--fx-register-fn' does, without touching the table.
Answers (CODE NLOCALS IR) or nil.  A thin view of `nelisp-fxir-analyze'.
As before, a symbol is not a source closure for this compatibility entry."
  (unless (symbolp fn)
    (let ((a (nelisp-fxir-analyze fn)))
      (when (eq (plist-get a :status) 'accepted)
        (list (plist-get a :code) (plist-get a :nlocals) (plist-get a :ir))))))

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
                                    (if (consp op) (memq (aref code (1- k)) op)
                                      (eql (aref code (1- k)) op)))
                         (push (list op k) bad)))
         (slot (n) (unless (and (integerp n) (>= n 0) (< n (max nlocals 1))) (push (list :slot n) bad)))
         (late-slot (n k boxed ops)
           (slot n)
           (op-at k ops)
           (unless (and (integerp k) (<= 0 k) (< (1+ k) (length code))
                        (symbolp (aref code k))
                        (eql (if boxed (- -1 n) n) (aref code (1+ k))))
             (push (list :late-slot n k) bad)))
         (walk (n &optional destination)
           (let ((k (car n)))
             (cond
              ((assq k expect) (op-at (nth 1 n) (cdr (assq k expect)))
               (when (eq k :varbind) (walk (nth 2 n))))
              ((memq k '(:slot :late-special-p)) (slot (nth 1 n)))
              ((eq k :setslot) (slot (nth 1 n)) (walk (nth 2 n) (nth 1 n)))
              ((eq k :late-ref) (late-slot (nth 1 n) (nth 2 n) (nth 3 n) '(33)))
              ((memq k '(:late-bind :late-set))
               (late-slot (nth 1 n) (nth 3 n) (nth 4 n) (if (eq k :late-bind) '(32 36) '(34)))
               (walk (nth 2 n)))
              ((eq k :call)
               (let* ((site (nth 1 n)) (op (aref code (1- site))))
                 (op-at site '(16 29 30 31))
                 (unless (eql (aref code (1+ site)) (nth 2 n)) (push (list :argc n) bad))
                 (when (memq op '(30 31))
                   (unless (equal (nth 3 n) (list :slot (aref code (+ site 2))))
                     (push (list :source-slot n) bad))
                   (when (and (= op 31) (not (eql destination (aref code (+ site 3)))))
                     (push (list :destination-slot n) bad))))
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
              ((memq k '(:catch :throw :unwind-protect)) (mapc #'walk (cdr n)))
              ((eq k :condition-case)
               (when (nth 1 n) (slot (nth 1 n)))
               (walk (nth 2 n)) (when (nth 3 n) (walk (nth 3 n)))
               (dolist (c (nthcdr 4 n)) (op-at (- (car c) 2) 23) (walk (nth 1 c))))
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
                          ((memq (car n) '(:const :slot :varref :alias :late-ref :late-special-p)) nil)
                          ((memq (car n) '(:setslot :late-set)) (walk (nth 2 n)))
                          (t (mapc #'walk (cdr n)))))))
      (walk (nth 1 ir)))
    (nreverse out)))

(provide 'nelisp-fxir-eval)
;;; nelisp-fxir-eval.el ends here
