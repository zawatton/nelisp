;;; nl-typecheck.el --- gradual type checker (Doc 236 Stage 2) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A reading pass: each top-level form is read with the real reader,
;; macroexpanded, and walked with bidirectional local inference.  It never
;; changes what is evaluated.  Everything unknown has the dynamic type t,
;; which is consistent with every type, so unknown code never errors.
;;
;; Annotation sources: (declare (ftype (function ...))) in defun,
;; `nl-typed' signatures, `cl-the', `nelisp-declare-type' forms (also from
;; a NAME-types.el side file), and the Stage 1 primitive table.
;;
;; Type info is a cons (TYPE . DEFINITE).  DEFINITE is non-nil when the
;; type comes from a literal or a declaration; those clashes are `error'.
;; Clashes involving an inferred type are `warning'.  Passing t where a
;; non-t type is declared is a `note' under `nelisp-typecheck-strict'.
;; Plain Elisp (no cl-lib) so it can later load on standalone NeLisp.

;;; Code:

(require 'nl-typecheck-types)
(require 'nl-typecheck-prims)
(require 'nl-typecheck-runtime)

(defvar nelisp-typecheck-strict nil
  "Non-nil makes implicit casts from t to a declared type `note' diagnostics.")

(defvar nl-typecheck-summary-dir nil
  "When non-nil, directory receiving per-source function summaries.")

(defvar nl-typecheck-last-stats nil
  "Plist (:forms N :skipped N) of the last `nl-typecheck-file' run.")

(defvar nl-typecheck--sigs nil "Hash: annotated function symbol -> signature.")
(defvar nl-typecheck--defs nil "Hash: symbols defined in the current file.")
(defvar nl-typecheck--assigned nil "Hash: symbols that are `setq' targets.")
(defvar nl-typecheck--assume nil
  "Alist (SYMBOL . TYPE): assumed union of the values `setq' stores in SYMBOL.
A symbol missing from it is widened to `t' when bound.")
(defvar nl-typecheck--record nil
  "Hash (or nil): SYMBOL -> infos of the values assigned to it in this pass.")
(defvar nl-typecheck--diags nil "Diagnostics of the current file, reversed.")
(defvar nl-typecheck--file "" "File name as given by the caller.")
(defvar nl-typecheck--src nil "Buffer holding the source text.")
(defvar nl-typecheck--start 1 "Start position of the current form.")
(defvar nl-typecheck--end 1 "End position of the current form.")
(defvar nl-typecheck--occ nil "Hash: callee -> occurrences already reported.")
(defvar nl-typecheck--summary nil "Summary entries, reversed.")
(defvar nl-typecheck--extra nil
  "Extra diagnostic keys (:param :expected :actual-type) for the report.")

(defconst nl-typecheck-predicates
  '((consp . cons) (stringp . string) (integerp . integer)
    (fixnump . fixnum) (bignump . bignum) (numberp . number)
    (symbolp . symbol) (vectorp . vector) (listp . list) (floatp . float)
    (atom . atom) (keywordp . keyword) (arrayp . array)
    (sequencep . sequence) (characterp . character) (natnump . natnum)
    (hash-table-p . hash-table) (bufferp . buffer) (markerp . marker)
    (booleanp . boolean) (null . null)
    (integer-or-marker-p . integer-or-marker)
    (number-or-marker-p . number-or-marker))
  "Type predicates that narrow a variable.")

;;;; Diagnostics ------------------------------------------------------

(defun nl-typecheck--position (callee)
  "Return a source position for a diagnostic about CALLEE."
  (let ((pos nl-typecheck--start))
    (when (and callee nl-typecheck--src (buffer-live-p nl-typecheck--src))
      (with-current-buffer nl-typecheck--src
        (let* ((n (gethash callee nl-typecheck--occ 0))
               (needle (concat "(" (symbol-name callee)))
               (found nil) (k 0))
          (puthash callee (1+ n) nl-typecheck--occ)
          (save-excursion
            (goto-char nl-typecheck--start)
            (while (and (not found) (< k 200)
                        (search-forward needle nl-typecheck--end t))
              (let ((c (char-after)))
                (when (or (null c) (memq c '(?\s ?\t ?\n ?\) ?\r)))
                  (when (>= k n) (setq found (- (point) (length needle))))
                  (setq k (1+ k)))))
            (when found (setq pos found))))))
    pos))

(defun nl-typecheck--report (level callee fmt &rest args)
  "Record a LEVEL diagnostic about CALLEE with message FMT and ARGS."
  (unless (and (eq level 'note) (not nelisp-typecheck-strict))
    (let ((pos (nl-typecheck--position callee)) (line 1) (col 1))
      (with-current-buffer nl-typecheck--src
        (save-excursion
          (goto-char pos)
          (setq line (line-number-at-pos pos))
          (setq col (1+ (- pos (line-beginning-position))))))
      (setq nl-typecheck--diags
            (cons (append (list :file nl-typecheck--file :line line :col col
                                :level level :message (apply #'format fmt args))
                          (and callee (list :callee callee))
                          nl-typecheck--extra)
                  nl-typecheck--diags)))))

(defun nl-typecheck-format-diagnostic (diag)
  "Format DIAG as FILE:LINE:COL: LEVEL: MESSAGE."
  (format "%s:%d:%d: %s: %s" (plist-get diag :file) (plist-get diag :line)
          (plist-get diag :col) (plist-get diag :level)
          (plist-get diag :message)))

;;;; Types ------------------------------------------------------------

(defun nl-typecheck--join (infos)
  "Return the joined info of INFOS."
  (let ((types nil) (def t) (top nil))
    (dolist (i infos)
      (if (eq (car i) t) (setq top t) (setq types (cons (car i) types)))
      (unless (cdr i) (setq def nil)))
    (if (or top (null infos))
        (cons (if infos t nil) nil)
      (cons (nl-typecheck-normalize (cons 'or (nreverse types))) def))))

(defun nl-typecheck--literal (value)
  "Return the info of the constant VALUE."
  (cond ((null value) '(null . t))
        ((eq value t) '((member t) . t))
        ((keywordp value) (cons (list 'member value) t))
        ((symbolp value) (cons (list 'member value) t))
        ((and (integerp value) (>= value nl-typecheck-types-min-fixnum)
              (<= value nl-typecheck-types-max-fixnum))
         (cons (list 'integer value value) t))
        ((integerp value) '(integer . t))
        ((floatp value) '(float . t))
        ((stringp value) '(string . t))
        ((vectorp value) '(vector . t))
        ((consp value) '(cons . t))
        (t '(t))))

(defun nl-typecheck--empty-p (type)
  "Non-nil when TYPE is provably empty."
  (and type (not (eq type t)) (nl-typecheck-subtype-p type nil)))

(defun nl-typecheck--refine (var type positive env)
  "Return ENV with VAR narrowed to TYPE (POSITIVE) or to its complement."
  (let ((b (assq var env)))
    (if (null b)
        env
      (let* ((cur (car (cdr b)))
             (add (if positive type (list 'not type)))
             (new (if (eq cur t) add (list 'and cur add))))
        (cons (cons var (cons (nl-typecheck-normalize new) nil)) env)))))

(defun nl-typecheck--const-value (form)
  "Return (VALUE) when FORM is a constant usable in `eq' narrowing."
  (cond ((null form) (list nil))
        ((eq form t) (list t))
        ((keywordp form) (list form))
        ((integerp form) (list form))
        ((and (consp form) (eq (car form) 'quote) (symbolp (car (cdr form))))
         (list (car (cdr form))))
        (t nil)))

(defun nl-typecheck--narrow (cnd env)
  "Return (THEN-ENV . ELSE-ENV) for condition CND under ENV."
  (let ((head (and (consp cnd) (car cnd))) (args (and (consp cnd) (cdr cnd))))
    (cond
     ((not (and head (symbolp head))) (cons env env))
     ((and (memq head '(not null)) (consp args) (consp (car args)))
      (let ((r (nl-typecheck--narrow (car args) env)))
        (cons (cdr r) (car r))))
     ((and (assq head nl-typecheck-predicates) (consp args)
           (symbolp (car args)) (null (cdr args)))
      (let ((ty (cdr (assq head nl-typecheck-predicates))) (v (car args)))
        (cons (nl-typecheck--refine v ty t env)
              (nl-typecheck--refine v ty nil env))))
     ((and (memq head '(eq eql)) (consp args) (consp (cdr args)))
      (let* ((a (car args)) (b (car (cdr args)))
             (v (cond ((and (symbolp a) (assq a env)
                            (nl-typecheck--const-value b))
                       a)
                      ((and (symbolp b) (assq b env)
                            (nl-typecheck--const-value a))
                       b)))
             (c (and v (nl-typecheck--const-value (if (eq v a) b a)))))
        (if (and v c)
            (cons (nl-typecheck--refine v (list 'member (car c)) t env) env)
          (cons env env))))
     ((and (eq head 'cl-typep) (consp args) (symbolp (car args))
           (consp (cdr args)) (consp (car (cdr args)))
           (eq (car (car (cdr args))) 'quote))
      (let ((ty (car (cdr (car (cdr args))))))
        (if (nl-typecheck-type-known-p ty)
            (cons (nl-typecheck--refine (car args) ty t env)
                  (nl-typecheck--refine (car args) ty nil env))
          (cons env env))))
     ((eq head 'and)
      (let ((then env))
        (dolist (a args) (setq then (car (nl-typecheck--narrow a then))))
        (cons then env)))
     ((eq head 'or)
      (let ((else env))
        (dolist (a args) (setq else (cdr (nl-typecheck--narrow a else))))
        (cons env else)))
     (t (cons env env)))))

;;;; Walker -----------------------------------------------------------

(defun nl-typecheck--infer-seq (forms env)
  "Infer FORMS in order under ENV and return the info of the last."
  (let ((info (cons nil t)))
    (while (consp forms)
      (setq info (nl-typecheck--infer (car forms) env))
      (setq forms (cdr forms)))
    info))

(defun nl-typecheck--infer-all (forms env)
  "Return the infos of FORMS (a possibly improper list) under ENV."
  (let ((out nil))
    (while (consp forms)
      (setq out (cons (nl-typecheck--infer (car forms) env) out))
      (setq forms (cdr forms)))
    (nreverse out)))

(defun nl-typecheck--assigned-type (var ty)
  "Return the type of assigned VAR with initial type TY under the assumptions."
  (let ((a (assq var nl-typecheck--assume)))
    (if a
        (car (nl-typecheck--join (list (cons ty nil) (cons (cdr a) nil))))
      t)))

(defun nl-typecheck--bind (var info env)
  "Return ENV extended with VAR bound to INFO.
An assigned VAR gets the join of INFO and the assumed assigned types."
  (cons (cons var (if (gethash var nl-typecheck--assigned)
                      (cons (nl-typecheck--assigned-type var (car info)) nil)
                    (cons (car info) nil)))
        env))

(defun nl-typecheck--walk-lambda (lam env)
  "Walk lambda form LAM under ENV with unknown parameter types."
  (let ((env2 env))
    (dolist (p (car (cdr lam)))
      (when (and (symbolp p) (not (memq p '(&optional &rest))))
        (setq env2 (cons (cons p (cons t nil)) env2))))
    (nl-typecheck--infer-seq (cdr (cdr lam)) env2)))

(defun nl-typecheck--infer-let (form env star)
  "Infer a let (STAR for let*) FORM under ENV."
  (let ((env2 env) (pending nil))
    (dolist (b (car (cdr form)))
      (let* ((var (if (consp b) (car b) b))
             (init (if (and (consp b) (consp (cdr b))) (car (cdr b)) nil))
             (info (nl-typecheck--infer init (if star env2 env))))
        (if star
            (setq env2 (nl-typecheck--bind var info env2))
          (setq pending (cons (cons var info) pending)))))
    (dolist (p (nreverse pending))
      (setq env2 (nl-typecheck--bind (car p) (cdr p) env2)))
    (nl-typecheck--infer-seq (cdr (cdr form)) env2)))

(defun nl-typecheck--infer-cond (clauses env)
  "Infer `cond' CLAUSES under ENV."
  (let ((infos nil) (cur env) (total nil))
    (dolist (cl clauses)
      (when (consp cl)
        (let* ((ti (nl-typecheck--infer (car cl) cur))
               (r (nl-typecheck--narrow (car cl) cur)))
          (if (cdr cl)
              (setq infos (cons (nl-typecheck--infer-seq (cdr cl) (car r))
                                infos))
            (setq infos (cons ti infos)))
          (when (eq (car cl) t) (setq total t))
          (setq cur (cdr r)))))
    (unless total (setq infos (cons '(null . t) infos)))
    (nl-typecheck--join infos)))

(defun nl-typecheck--infer-call (name args env)
  "Infer a call of NAME with argument forms ARGS under ENV."
  (let* ((infos (nl-typecheck--infer-all args env))
         (annotated (gethash name nl-typecheck--sigs))
         (sig (or annotated
                  (and (not (gethash name nl-typecheck--defs))
                       (nl-typecheck-prims-signature name)))))
    (if (null sig)
        (cons t nil)
      (nl-typecheck--check-call name sig (and annotated t) args infos)
      (let ((r (car (cdr (cdr sig)))))
        (if (eq r t) (cons t nil) (cons r (and annotated t)))))))

(defun nl-typecheck-param-type (arglist i)
  "Return the declared type of position I in ARGLIST, or :none."
  (let ((mode 'req) (k 0) (found :none) (rest-ty :none))
    (dolist (a arglist)
      (cond ((eq a '&optional) (setq mode 'opt))
            ((eq a '&rest) (setq mode 'rest))
            ((eq mode 'rest) (setq rest-ty a))
            (t (when (= k i) (setq found a)) (setq k (1+ k)))))
    (if (eq found :none) rest-ty found)))

(defun nl-typecheck--check-call (name sig annotated args infos)
  "Compare argument INFOS of a call to NAME with signature SIG."
  (let ((i 0) (n (length args)) (arglist (car (cdr sig))))
    (when annotated
      (let ((ar (nl-typecheck-prims-spec-arity sig)))
        (when (or (< n (car ar)) (and (integerp (cdr ar)) (> n (cdr ar))))
          (nl-typecheck--report
           'error name "wrong number of arguments to %S: got %d, expected %s"
           name n (if (eq (cdr ar) 'many) (format "%d or more" (car ar))
                    (if (= (car ar) (cdr ar)) (format "%d" (car ar))
                      (format "%d to %d" (car ar) (cdr ar))))))))
    (dolist (info infos)
      (let ((pt (nl-typecheck-param-type arglist i)) (at (car info)))
        (setq i (1+ i))
        (cond
         ((or (eq pt :none) (eq pt t)))
         ((eq at t)
          (nl-typecheck--report
           'note name "implicit cast from t to %S for argument %d of %S"
           pt i name))
         ((nl-typecheck--empty-p at))
         ((nl-typecheck-disjoint-p at pt)
          (let ((nl-typecheck--extra
                 (list :param i :expected pt
                       :actual-type (nl-typecheck-normalize at))))
            (nl-typecheck--report
             (if (cdr info) 'error 'warning) name
             "argument %d of %S has type %S, expected %S"
             i name (nl-typecheck-normalize at) pt))))))))

(defun nl-typecheck--infer (form env)
  "Infer FORM under ENV and return its (TYPE . DEFINITE) info."
  (cond
   ((null form) '(null . t))
   ((eq form t) '((member t) . t))
   ((keywordp form) (cons (list 'member form) t))
   ((symbolp form) (let ((b (assq form env))) (if b (cdr b) '(t))))
   ((not (consp form)) (nl-typecheck--literal form))
   (t (nl-typecheck--infer-cons form env))))

(defun nl-typecheck--infer-cons (form env)
  "Infer the cons FORM under ENV."
  (let ((head (car form)) (args (cdr form)))
    (cond
     ((not (symbolp head))
      (if (and (consp head) (eq (car head) 'lambda))
          (progn (nl-typecheck--walk-lambda head env)
                 (nl-typecheck--infer-all args env)
                 '(t))
        (nl-typecheck--infer-all form env)
        '(t)))
     ((eq head 'quote)
      (if (consp args) (nl-typecheck--literal (car args)) '(t)))
     ((eq head 'function)
      (when (and (consp args) (consp (car args)) (eq (car (car args)) 'lambda))
        (nl-typecheck--walk-lambda (car args) env))
      '(function . t))
     ((eq head 'lambda) (nl-typecheck--walk-lambda form env) '(function . t))
     ((eq head 'nl-typecheck--the) (nl-typecheck--infer-the args env))
     ((eq head 'if)
      (if (not (consp args))
          '(t)
        (nl-typecheck--infer (car args) env)
        (let* ((r (nl-typecheck--narrow (car args) env))
               (a (nl-typecheck--infer (car (cdr args)) (car r)))
               (b (if (cdr (cdr args))
                      (nl-typecheck--infer-seq (cdr (cdr args)) (cdr r))
                    '(null . t))))
          (nl-typecheck--join (list a b)))))
     ((eq head 'cond) (nl-typecheck--infer-cond args env))
     ((memq head '(progn)) (nl-typecheck--infer-seq args env))
     ((eq head 'prog1)
      (let ((i (nl-typecheck--infer (car args) env)))
        (nl-typecheck--infer-seq (cdr args) env)
        i))
     ((eq head 'and)
      (let ((cur env) (infos nil))
        (dolist (a args)
          (setq infos (cons (nl-typecheck--infer a cur) infos))
          (setq cur (car (nl-typecheck--narrow a cur))))
        (nl-typecheck--join (cons '(null . t) (list (car infos))))))
     ((eq head 'or)
      (let ((cur env) (infos nil))
        (dolist (a args)
          (setq infos (cons (nl-typecheck--infer a cur) infos))
          (setq cur (cdr (nl-typecheck--narrow a cur))))
        (nl-typecheck--join infos)))
     ((eq head 'while)
      (when (consp args)
        (nl-typecheck--infer (car args) env)
        (nl-typecheck--infer-seq (cdr args) (car (nl-typecheck--narrow (car args) env))))
      '(null . t))
     ((eq head 'let) (nl-typecheck--infer-let form env nil))
     ((eq head 'let*) (nl-typecheck--infer-let form env t))
     ((eq head 'setq)
      (let ((info '(null . t)) (r args))
        (while (consp r)
          (when (consp (cdr r))
            (setq info (nl-typecheck--infer (car (cdr r)) env))
            (when (and nl-typecheck--record (symbolp (car r)))
              (puthash (car r) (cons info (gethash (car r) nl-typecheck--record))
                       nl-typecheck--record)))
          (setq r (if (consp (cdr r)) (cdr (cdr r)) nil)))
        (cons (car info) nil)))
     ((eq head 'condition-case)
      (when (consp args)
        (let ((env2 (cons (cons (car args) (cons t nil)) env)))
          (nl-typecheck--infer (car (cdr args)) env)
          (dolist (h (cdr (cdr args)))
            (when (consp h) (nl-typecheck--infer-seq (cdr h) env2)))))
      '(t))
     (t (nl-typecheck--infer-call head args env)))))

(defun nl-typecheck--infer-the (args env)
  "Infer a cl-the node, ARGS being the quoted type and the form."
  (let* ((ty (car (cdr (car args)))) (info (nl-typecheck--infer (car (cdr args)) env)))
    (cond
     ((not (nl-typecheck-type-known-p ty)) info)
     (t
      (when (and (not (eq (car info) t)) (not (nl-typecheck--empty-p (car info)))
                 (nl-typecheck-disjoint-p (car info) ty))
        (nl-typecheck--report (if (cdr info) 'error 'warning) nil
                              "cl-the %S applied to a value of type %S"
                              ty (nl-typecheck-normalize (car info))))
      (cons ty t)))))

;;;; Top-level forms --------------------------------------------------

(defun nl-typecheck--collect-assigned (form table)
  "Record every `setq' target found anywhere in FORM into TABLE."
  (let ((stack (list form)))
    (while stack
      (let ((x (car stack)))
        (setq stack (cdr stack))
        (when (consp x)
          (when (eq (car x) 'setq)
            (let ((r (cdr x)))
              (while (consp r)
                (puthash (car r) t table)
                (setq r (if (consp (cdr r)) (cdr (cdr r)) nil)))))
          (setq stack (cons (car x) (cons (cdr x) stack))))))))

(defun nl-typecheck--collect-local (form)
  "Collect the `setq' targets of FORM globally; return non-nil if any."
  (let ((local (make-hash-table :test 'eq)) (any nil))
    (nl-typecheck--collect-assigned form local)
    (maphash (lambda (k _v) (setq any t) (puthash k t nl-typecheck--assigned))
             local)
    any))

(defun nl-typecheck--records-to-assume (rec)
  "Turn the recorded assignments REC into an assumption alist.
Symbols with an unknown (`t') assigned type are left out, so they widen."
  (let ((out nil))
    (maphash (lambda (k infos)
               (let ((ty (car (nl-typecheck--join infos))))
                 (unless (eq ty t) (setq out (cons (cons k ty) out)))))
             rec)
    out))

(defun nl-typecheck--assume-equal (a b)
  "Return non-nil when assumption alists A and B hold the same types."
  (and (= (length a) (length b))
       (let ((ok t))
         (dolist (x a)
           (let ((y (assq (car x) b)))
             (unless (and y (equal (cdr x) (cdr y))) (setq ok nil))))
         ok)))

(defun nl-typecheck--run-with-assumptions (any run)
  "Call RUN with flow-insensitive assignment types; return its last value.
When ANY, up to three silent trial passes record the types assigned to
each `setq' target and iterate to a fixed point; the last call of RUN is
the only one whose diagnostics survive.  Without convergence every
assigned variable is widened to `t'."
  (if (not any)
      (funcall run)
    (let ((assume nil) (iter 0) (stable nil) (occ0 nl-typecheck--occ))
      (while (and (not stable) (< iter 3))
        (let* ((rec (make-hash-table :test 'eq))
               (nl-typecheck--record rec)
               (nl-typecheck--diags nil)
               (nl-typecheck--summary nil)
               (nl-typecheck--occ (make-hash-table :test 'eq))
               (nl-typecheck--assume assume)
               (next nil))
          (maphash (lambda (k v) (puthash k v nl-typecheck--occ)) occ0)
          (funcall run)
          (setq next (nl-typecheck--records-to-assume rec))
          (if (nl-typecheck--assume-equal next assume)
              (setq stable t)
            (setq assume next))
          (setq iter (1+ iter))))
      (let ((nl-typecheck--assume (if stable assume nil))
            (nl-typecheck--record nil))
        (funcall run)))))

(defun nl-typecheck--expand (form)
  "Macroexpand FORM in a scratch buffer; return the result or :skip."
  (condition-case nil
      (with-temp-buffer
        (macroexpand-all
         form
         (list (cons 'cl-the
                     (lambda (type f)
                       (list 'nl-typecheck--the (list 'quote type) f))))))
    (error :skip)))

(defun nl-typecheck-ftype (body)
  "Return (SIG . BODY-WITHOUT-DECLARE) from the leading declares of BODY."
  (let ((sig nil) (rest nil))
    (dolist (f body)
      (if (and (consp f) (eq (car f) 'declare))
          (dolist (spec (cdr f))
            (when (and (consp spec) (eq (car spec) 'ftype)
                       (consp (cdr spec)) (consp (car (cdr spec)))
                       (eq (car (car (cdr spec))) 'function))
              (setq sig (car (cdr spec)))))
        (setq rest (cons f rest))))
    (cons sig (nreverse rest))))

(defun nl-typecheck--register (name sig)
  "Register SIG as the declared signature of NAME."
  (when (and name (symbolp name) (consp sig) (eq (car sig) 'function)
             (consp (cdr sig)) (listp (car (cdr sig))))
    (puthash name sig nl-typecheck--sigs)))

(defun nl-typecheck--process-function (name arglist body sig)
  "Check function NAME with ARGLIST and BODY; SIG is its declared type."
  (when sig (nl-typecheck--register name sig))
  (let ((exp (nl-typecheck--expand (cons 'lambda (cons arglist body)))))
    ;; GNU expands a lambda to (function (lambda ...)); the standalone
    ;; runtime leaves it as (lambda ...).
    (if (not (and (consp exp) (memq (car exp) '(function lambda))))
        :skip
      (let* ((lam (if (eq (car exp) 'function) (car (cdr exp)) exp))
             (env nil) (i 0) (mode 'req))
        (nl-typecheck--run-with-assumptions
         (nl-typecheck--collect-local lam)
         (lambda ()
        (setq env nil i 0 mode 'req)
        (dolist (p arglist)
          (cond
           ((eq p '&optional) (setq mode 'opt))
           ((eq p '&rest) (setq mode 'rest))
           ((eq mode 'rest) (setq env (cons (cons p '(list)) env)))
           (t
            (let ((ty (if sig (nl-typecheck-param-type (car (cdr sig)) i) :none)))
              (setq i (1+ i))
              (setq env
                    (cons (cons p (cond
                                   ((or (eq ty :none) (eq ty t)) (cons t nil))
                                   ((gethash p nl-typecheck--assigned)
                                    (cons (nl-typecheck--assigned-type
                                           p (if (eq mode 'opt) (list 'or 'null ty) ty))
                                          nil))
                                   (t (cons (if (eq mode 'opt) (list 'or 'null ty) ty) t))))
                          env))))))
        (let ((info (nl-typecheck--infer-seq (cdr (cdr lam)) env)))
          (when (and sig (consp (cdr (cdr sig))))
            (let ((rt (car (cdr (cdr sig)))))
              (when (and (not (eq rt t)) (not (eq (car info) t))
                         (not (nl-typecheck--empty-p (car info)))
                         (nl-typecheck-disjoint-p (car info) rt))
                (nl-typecheck--report
                 'warning nil "%S is declared to return %S but its body returns %S"
                 name rt (nl-typecheck-normalize (car info))))))
          (when sig
            (setq nl-typecheck--summary
                  (cons (list name (car (cdr sig)) (car (cdr (cdr sig))))
                        nl-typecheck--summary))))))
        nil))))

(defun nl-typecheck--process-form (form)
  "Check one top-level FORM; return :skip when it cannot be expanded."
  (let ((head (and (consp form) (car form))))
    (cond
     ((and (eq head 'defun) (consp (cdr form)) (consp (cdr (cdr form))))
      (let ((d (nl-typecheck-ftype (cdr (cdr (cdr form))))))
        (nl-typecheck--process-function
         (car (cdr form)) (car (cdr (cdr form))) (cdr d) (car d))))
     ((and (eq head 'nl-typed) (memq (car (cdr form)) '(defun nl-defun))
           (consp (nthcdr 3 form)))
      (let* ((name (nth 2 form)) (decls (nth 3 form)) (types nil) (args nil))
        (dolist (d decls)
          (when (consp d)
            (setq args (cons (car d) args))
            (setq types (cons (car (cdr d)) types))))
        (nl-typecheck--process-function
         name (nreverse args) (nthcdr 4 form)
         (list 'function (nreverse types) t))))
     ((memq head '(progn eval-and-compile eval-when-compile))
      (let ((res nil))
        (dolist (f (cdr form))
          (when (eq (nl-typecheck--process-form f) :skip) (setq res :skip)))
        res))
     ((eq head 'nelisp-declare-type)
      (when (and (consp (cdr form)) (consp (cdr (cdr form))))
        (let ((sym (car (cdr form))) (ty (car (cdr (cdr form)))))
          (when (and (consp sym) (eq (car sym) 'quote)) (setq sym (car (cdr sym))))
          (when (and (consp ty) (eq (car ty) 'quote)) (setq ty (car (cdr ty))))
          (nl-typecheck--register sym ty)))
      nil)
     (t
      (let ((exp (nl-typecheck--expand form)))
        (if (eq exp :skip)
            :skip
          (nl-typecheck--run-with-assumptions
           (nl-typecheck--collect-local exp)
           (lambda () (nl-typecheck--infer exp nil)))
          nil))))))

(defun nl-typecheck--note-defs (form)
  "Record the function names FORM defines (so table signatures are not used)."
  (when (consp form)
    (let ((head (car form)))
      (cond
       ((memq head '(defun defmacro defsubst cl-defun cl-defmacro cl-defsubst
                           define-inline cl-defgeneric cl-defmethod nl-defun))
        (when (symbolp (car (cdr form)))
          (puthash (car (cdr form)) t nl-typecheck--defs)))
       ((eq head 'nl-typed)
        (when (symbolp (nth 2 form)) (puthash (nth 2 form) t nl-typecheck--defs)))
       ((and (eq head 'defalias) (consp (cdr form)) (consp (car (cdr form)))
             (eq (car (car (cdr form))) 'quote))
        (puthash (car (cdr (car (cdr form)))) t nl-typecheck--defs))
       ((memq head '(progn eval-and-compile eval-when-compile))
        (dolist (f (cdr form)) (nl-typecheck--note-defs f)))))))

(defun nl-typecheck-read-forms (path)
  "Return (FORMS . BUFFER) for PATH; each form is (FORM START END)."
  (let ((buf (generate-new-buffer " *nl-typecheck-src*")) (forms nil)
        (bad nil))
    (when (and (fboundp 'auto-compression-mode)
               (not (bound-and-true-p auto-compression-mode)))
      (auto-compression-mode 1))
    (with-current-buffer buf
      (insert-file-contents path)
      (goto-char (point-min))
      (let ((more t))
        (while more
          (skip-chars-forward " \t\n\r\f")
          (while (eq (char-after) ?\;)
            (forward-line 1)
            (skip-chars-forward " \t\n\r\f"))
          (if (eobp)
              (setq more nil)
            (let ((start (point)))
              (condition-case nil
                  (let ((f (read (current-buffer))))
                    (setq forms (cons (list f start (point)) forms)))
                (error (setq bad t more nil))))))))
    (list (nreverse forms) buf bad)))

(defun nl-typecheck--side-file (path)
  "Return the NAME-types.el side file for PATH if it exists."
  (let ((base (if (string-match "\\(\\.el\\)\\(\\.gz\\)?\\'" path)
                  (substring path 0 (match-beginning 0))
                path)))
    (let ((f (concat base "-types.el")))
      (and (file-exists-p f) f))))

(defun nl-typecheck--load-side-file (path)
  "Register the nelisp-declare-type forms of side file PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (condition-case nil
        (let ((f nil))
          (while (progn (skip-chars-forward " \t\n\r\f")
                        (while (eq (char-after) ?\;)
                          (forward-line 1)
                          (skip-chars-forward " \t\n\r\f"))
                        (not (eobp)))
            (setq f (read (current-buffer)))
            (when (and (consp f) (eq (car f) 'nelisp-declare-type))
              (nl-typecheck--process-form f))))
      (error nil))))

(defun nl-typecheck--write-summary (path)
  "Write the per-function summary for PATH when a directory is configured."
  (when (and nl-typecheck-summary-dir nl-typecheck--summary
             (fboundp 'secure-hash))
    (let* ((hash (with-current-buffer nl-typecheck--src
                   (secure-hash 'sha1 (current-buffer))))
           (file (expand-file-name (concat hash ".eld") nl-typecheck-summary-dir)))
      (make-directory nl-typecheck-summary-dir t)
      (with-temp-file file
        (insert (format ";; summary of %s\n" path))
        (prin1 (sort (copy-sequence nl-typecheck--summary)
                     (lambda (a b) (string< (symbol-name (car a))
                                            (symbol-name (car b)))))
               (current-buffer))
        (insert "\n")))))

(defun nl-typecheck--process-forms (forms)
  "Check FORMS, a list of (FORM START END); return the number skipped.
Uses the dynamically bound checker state, so a caller may keep a
signature table across calls (the REPL layers do)."
  (let ((skipped 0))
    (dolist (f forms)
      (let ((nl-typecheck--start (nth 1 f))
            (nl-typecheck--end (nth 2 f)))
        (clrhash nl-typecheck--assigned)
        (when (eq :skip (condition-case nil
                            (nl-typecheck--process-form (car f))
                          (error :skip)))
          (setq skipped (1+ skipped)))))
    skipped))

(defun nl-typecheck-file (path &optional scope)
  "Check the source file PATH (.el or .el.gz) and return diagnostics.
SCOPE is `typed', `own' or `vendor'; vendor scope drops notes.  The
statistics of the run are left in `nl-typecheck-last-stats'."
  (let* ((nl-typecheck--sigs (make-hash-table :test 'eq))
         (nl-typecheck--defs (make-hash-table :test 'eq))
         (nl-typecheck--assigned (make-hash-table :test 'eq))
         (nl-typecheck--occ (make-hash-table :test 'eq))
         (nl-typecheck--diags nil)
         (nl-typecheck--summary nil)
         (nl-typecheck--file path)
         (read (nl-typecheck-read-forms path))
         (forms (car read))
         (nl-typecheck--src (car (cdr read)))
         (skipped (if (car (cdr (cdr read))) 1 0))
         (total (length forms))
         (nelisp-typecheck-strict (and (not (eq scope 'vendor))
                                       nelisp-typecheck-strict))
         (side (nl-typecheck--side-file path)))
    (unwind-protect
        (progn
          (dolist (f forms) (nl-typecheck--note-defs (car f)))
          (when side
            (let ((nl-typecheck--start 1) (nl-typecheck--end 1))
              (nl-typecheck--load-side-file side)))
          (setq skipped (+ skipped (nl-typecheck--process-forms forms)))
          (nl-typecheck--write-summary path))
      (kill-buffer nl-typecheck--src))
    (setq nl-typecheck-last-stats (list :forms total :skipped skipped))
    (nreverse nl-typecheck--diags)))

(define-error 'nl-typecheck-rejected "Input rejected by the type checker")

(defun nl-typecheck-check-repl-form (form label sigs defs)
  "Check FORM against signatures SIGS and defined symbols DEFS.
Diagnostics are reported against LABEL; return them in source order."
  (let* ((nl-typecheck--sigs sigs)
         (nl-typecheck--defs defs)
         (nl-typecheck--assigned (make-hash-table :test 'eq))
         (nl-typecheck--occ (make-hash-table :test 'eq))
         (nl-typecheck--diags nil)
         (nl-typecheck--summary nil)
         (nl-typecheck--extra nil)
         (nl-typecheck--file label)
         (nl-typecheck--src (generate-new-buffer " *nl-typecheck-repl*")))
    (unwind-protect
        (progn
          (with-current-buffer nl-typecheck--src
            (let ((print-length nil) (print-level nil))
              (prin1 form nl-typecheck--src)))
          (nl-typecheck--note-defs form)
          (nl-typecheck--process-forms
           (list (list form 1 (with-current-buffer nl-typecheck--src
                                (point-max)))))
          (nreverse nl-typecheck--diags))
      (kill-buffer nl-typecheck--src))))

(provide 'nl-typecheck)
;;; nl-typecheck.el ends here
