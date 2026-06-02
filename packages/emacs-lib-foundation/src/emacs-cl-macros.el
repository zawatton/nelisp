;;; emacs-cl-macros.el --- Minimal cl-lib subset for NeLisp standalone  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 10 — extracted from `emacs-stub.el' (= the Phase 4
;; batch 3 placeholder).  Vendor `cl-macs.el' / `cl-seq.el' fail to
;; load under NeLisp standalone because of deep pcase patterns we
;; do not yet support; this file ships *just the surface* the anvil
;; modules + Phase 9 buffer code use, mapped to plain elisp on top
;; of bootstrap eval primitives.
;;
;; Public surface (each gated on `unless (fboundp ...)' so loading
;; under host Emacs is a cheap no-op):
;;
;;   - macros: cl-defun, cl-incf, cl-decf, cl-loop, cl-defstruct,
;;             cl-case, cl-pushnew, cl-letf, cl-letf*, cl-flet,
;;             cl-labels, cl-block, cl-return-from, cl-return,
;;             cl-defgeneric, cl-defmethod
;;   - fns:    cl-some, cl-every, cl-find, cl-position,
;;             cl-remove-if(-not), cl-delete-if(-not),
;;             cl-delete-duplicates, cl-union, cl-intersection,
;;             cl-sort, cl-getf, cl-first/second/third
;;
;; Feature provides: cl-macs / cl-seq / cl-extra / cl-generic so
;; that vendor `(require ...)' chains short-circuit without actually
;; loading the heavyweight files.
;;
;; Internal helpers were renamed `emacs-stub--cl-*' →
;; `emacs-cl-macros--*' as part of Phase 10's emacs-stub split.
;;
;; Depends on: `pcase' (= emacs-pcase.el).  Make sure that loads first
;; if you want full pattern support inside cl-* expansions.

;;; Code:

(defun emacs-cl-macros--define-p (symbol)
  "Return non-nil when SYMBOL should be supplied by this shim.
Host Emacs often has CL names installed as autoloads before `cl-lib'
is loaded.  When this local shim wins `load-path', those autoloads
would otherwise prevent the shim from defining the requested macro."
  (or (not (fboundp symbol))
      (and (fboundp 'autoloadp)
           (autoloadp (symbol-function symbol)))))

;;;; --- arglist parsing helpers ------------------------------------------

(defun emacs-cl-macros--split-arglist (arglist)
  "Split ARGLIST into (POSITIONAL OPTIONALS RESTSYM KEYS).
KEYS = list of (KEYWORD-NAME PARAM-SYM DEFAULT-FORM) triples."
  (let ((positional nil)
        (optionals nil)
        (restsym nil)
        (keys nil)
        (mode 'positional)
        (cur arglist))
    (while cur
      (let ((tok (car cur)))
        (cond
         ((eq tok '&optional) (setq mode 'optional))
         ((eq tok '&rest)     (setq mode 'rest))
         ((eq tok '&key)      (setq mode 'key))
         ((eq tok '&aux)      (setq mode 'aux))
         (t
          (cond
           ((eq mode 'positional) (setq positional (cons tok positional)))
           ((eq mode 'optional)
            (setq optionals (cons tok optionals)))
           ((eq mode 'rest)
            (setq restsym tok))
           ((eq mode 'key)
            (let* ((sym (if (consp tok) (car tok) tok))
                   (default (if (consp tok) (car (cdr tok)) nil))
                   (kwname (intern
                            (concat ":"
                                    (symbol-name sym)))))
              (setq keys (cons (list kwname sym default) keys))))
           ;; &aux: drop (= local lets, rarely critical for stubs)
           ((eq mode 'aux) nil)))))
      (setq cur (cdr cur)))
    (let ((rev-positional nil) (rev-optionals nil) (rev-keys nil)
          (p positional) (o optionals) (k keys))
      (while p (setq rev-positional (cons (car p) rev-positional)) (setq p (cdr p)))
      (while o (setq rev-optionals (cons (car o) rev-optionals)) (setq o (cdr o)))
      (while k (setq rev-keys (cons (car k) rev-keys)) (setq k (cdr k)))
      (list rev-positional rev-optionals restsym rev-keys))))

(defun emacs-cl-macros--key-bindings (keys restsym)
  "Build let-bindings for KEYS by scanning RESTSYM (= the &rest var).
Each binding is (PARAM (or (cadr (memq KW RESTSYM)) DEFAULT))."
  (let ((out nil)
        (cur keys))
    (while cur
      (let* ((entry (car cur))
             (kw (car entry))
             (sym (car (cdr entry)))
             (def (car (cdr (cdr entry)))))
        (setq out (cons (list sym
                              (list 'or
                                    (list 'car
                                          (list 'cdr
                                                (list 'memq (list 'quote kw) restsym)))
                                    def))
                        out)))
      (setq cur (cdr cur)))
    (let ((rev nil) (c out))
      (while c (setq rev (cons (car c) rev)) (setq c (cdr c)))
      rev)))

;;;; --- cl-defun ---------------------------------------------------------

(when (or (not (boundp 'emacs-version))
          (emacs-cl-macros--define-p 'cl-defun))
  ;; cl-defun supporting &optional, &rest, &key (= adequate for
  ;; anvil-memory / anvil-state arglists).
  ;;
  ;; Strategy: expand (cl-defun NAME (POS &optional O &key K1 K2) BODY) to
  ;; (defun NAME (POS &optional O &rest --cl-keys)
  ;;   (let ((K1 (or (cadr (memq :K1 --cl-keys)) DEFAULT))
  ;;         (K2 (or (cadr (memq :K2 --cl-keys)) DEFAULT)))
  ;;     BODY))
  ;; If &rest is present in the original arglist, reuse that name instead
  ;; of synthesizing --cl-keys.
  (defvar emacs-cl-macros--defun-call-count 0
    "Bumped each time the cl-defun macro stub expands a form.")
  ;; Two registration paths needed:
  ;;   1. build-tool/eval recognizes the (macro lambda ...) function cell
  ;;      → use plain `defmacro' (writes to env.set_function)
  ;;   2. nelisp-eval-form (the FULL self-host evaluator) consults
  ;;      `nelisp--macros' hashtable, NOT the function cell → also
  ;;      puthash into nelisp--macros so the takeover path expands too
  ;;
  ;; Path (2) registration happens at the bottom of this `unless'
  ;; clause via `(when (boundp 'nelisp--macros) ...)' guard.
  (defmacro cl-defun (name arglist &rest body)
    "Stub: cl-defun with &optional / &rest / &key support."
    (setq emacs-cl-macros--defun-call-count
          (+ 1 emacs-cl-macros--defun-call-count))
    (let* ((parts (emacs-cl-macros--split-arglist arglist))
           (positional (car parts))
           (optionals (car (cdr parts)))
           (restsym (car (cdr (cdr parts))))
           (keys (car (cdr (cdr (cdr parts))))))
      (cond
       ;; No &key — emit plain defun with original layout (preserve &rest).
       ((null keys)
        (let ((out positional))
          (when optionals
            (let ((tail (cons '&optional nil))
                  (o optionals))
              (while o (setq tail (append tail (list (car o)))) (setq o (cdr o)))
              (let ((all out) (t2 tail))
                (while t2 (setq all (append all (list (car t2)))) (setq t2 (cdr t2)))
                (setq out all))))
          (when restsym
            (setq out (append out (list '&rest restsym))))
          (cons 'defun (cons name (cons out body)))))
       (t
        ;; &key present — synthesize &rest --cl-keys, scan it for kw values.
        (let* ((rest-name (or restsym '--cl-keys))
               (real-arglist positional))
          (when optionals
            (let ((tail (cons '&optional nil))
                  (o optionals))
              (while o (setq tail (append tail (list (car o)))) (setq o (cdr o)))
              (setq real-arglist (append real-arglist tail))))
          (setq real-arglist (append real-arglist (list '&rest rest-name)))
          (let* ((bindings (emacs-cl-macros--key-bindings keys rest-name))
                 (real-body (list (cons 'let* (cons bindings body)))))
            (cons 'defun (cons name (cons real-arglist real-body))))))))))

;;;; --- cl-incf / cl-decf ------------------------------------------------

(when (or (not (boundp 'emacs-version))
          (emacs-cl-macros--define-p 'cl-incf))
  (defmacro cl-incf (place &optional delta)
    "Stub: increment PLACE by DELTA, defaulting to 1."
    (let ((value (list '+ place (or delta 1))))
      (if (symbolp place)
          (list 'setq place value)
        (list 'setf place value)))))

(when (or (not (boundp 'emacs-version))
          (emacs-cl-macros--define-p 'cl-decf))
  (defmacro cl-decf (place &optional delta)
    (let ((value (list '- place (or delta 1))))
      (if (symbolp place)
          (list 'setq place value)
        (list 'setf place value)))))

;;;; --- cl-some / cl-every / cl-position / cl-find ---------------------

(unless (fboundp 'cl-some)
  (defun cl-some (predicate sequence &rest more)
    "Stub: return first non-nil PREDICATE result over SEQUENCE.
Ignores MORE (= multi-list version)."
    (ignore more)
    (let ((cur sequence)
          (result nil))
      (while (and cur (not result))
        (setq result (funcall predicate (car cur)))
        (setq cur (cdr cur)))
      result)))

(unless (fboundp 'cl-every)
  (defun cl-every (predicate sequence &rest more)
    (ignore more)
    (let ((cur sequence)
          (ok t))
      (while (and cur ok)
        (unless (funcall predicate (car cur)) (setq ok nil))
        (setq cur (cdr cur)))
      ok)))

(unless (fboundp 'cl-position)
  (defun cl-position (item sequence &rest keys)
    "Doc 51 (2026-05-04) MVP `cl-position'.
Return integer index of first (or last with `:from-end t')
occurrence of ITEM in SEQUENCE, or nil.  SEQUENCE may be a
list, vector, or string.  Honours `:test FN' (= comparator,
default `eql'-style equality)."
    (let* ((from-end (plist-get keys :from-end))
           ;; Default test: `eq' only.  Falling back to `equal' on a
           ;; mismatch would walk structurally — fatal on cyclic
           ;; cl-defstruct values (e.g. window parent <-> children).
           ;; Callers that need value-equality (chars, strings) work
           ;; under `eq' too because nelisp interns those, and any
           ;; caller that *needs* structural comparison can pass
           ;; `:test #'equal' explicitly.
           (test (or (plist-get keys :test) #'eq))
           (n (cond ((null sequence) 0)
                    ((stringp sequence) (length sequence))
                    ((vectorp sequence) (length sequence))
                    (t (length sequence))))
           (get-elt (cond ((null sequence) (lambda (_i) nil))
                          ((stringp sequence)
                           (lambda (i) (aref sequence i)))
                          ((vectorp sequence)
                           (lambda (i) (aref sequence i)))
                          (t (lambda (i) (nth i sequence))))))
      (cond
       (from-end
        (let ((i (1- n)) (found nil))
          (while (and (>= i 0) (not found))
            (when (funcall test item (funcall get-elt i))
              (setq found i))
            (setq i (1- i)))
          found))
       (t
        (let ((i 0) (found nil))
          (while (and (< i n) (not found))
            (when (funcall test item (funcall get-elt i))
              (setq found i))
            (setq i (1+ i)))
          found))))))

(unless (fboundp 'cl-find)
  (defun cl-find (item sequence &rest keys)
    "Doc 51 MVP `cl-find'.
Return the first element of SEQUENCE matching ITEM (= via
`:test' or `eql'), or nil.  Accepts list, vector, or string."
    (let ((idx (apply #'cl-position item sequence keys)))
      (cond
       ((null idx) nil)
       ((stringp sequence) (aref sequence idx))
       ((vectorp sequence) (aref sequence idx))
       (t (nth idx sequence))))))

;;;; --- cl-remove-if(-not) / cl-delete-if(-not) -----------------------

(unless (fboundp 'cl-remove-if-not)
  (defun cl-remove-if-not (predicate sequence &rest _keys)
    (let ((acc nil) (cur sequence))
      (while cur
        (when (funcall predicate (car cur))
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (nreverse acc))))

(unless (fboundp 'cl-remove-if)
  (defun cl-delete-if (predicate sequence &rest _keys)
    "Stub: alias for cl-remove-if (in-place delete not supported)."
    (let ((acc nil) (cur sequence))
      (while cur
        (unless (funcall predicate (car cur))
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (let ((rev nil))
        (while acc (setq rev (cons (car acc) rev)) (setq acc (cdr acc)))
        rev)))
  (defun cl-delete-if-not (predicate sequence &rest _keys)
    (let ((acc nil) (cur sequence))
      (while cur
        (when (funcall predicate (car cur))
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (let ((rev nil))
        (while acc (setq rev (cons (car acc) rev)) (setq acc (cdr acc)))
        rev)))
  (defun cl-remove-if (predicate sequence &rest _keys)
    (let ((acc nil) (cur sequence))
      (while cur
        (unless (funcall predicate (car cur))
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (nreverse acc))))

(unless (fboundp 'cl-delete-if)
  (defalias 'cl-delete-if 'cl-remove-if))

;;;; --- cl-delete-duplicates / cl-union / cl-intersection / cl-sort ---

(unless (fboundp 'cl-delete-duplicates)
  (defun cl-delete-duplicates (sequence &rest _keys)
    (let ((acc nil) (cur sequence))
      (while cur
        (unless (member (car cur) acc)
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (nreverse acc))))

(unless (fboundp 'cl-union)
  (defun cl-union (list1 list2 &rest _keys)
    (let ((acc list1) (cur list2))
      (while cur
        (unless (member (car cur) acc)
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      acc)))

(unless (fboundp 'cl-intersection)
  (defun cl-intersection (list1 list2 &rest _keys)
    (let ((acc nil) (cur list1))
      (while cur
        (when (member (car cur) list2)
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (nreverse acc))))

(unless (fboundp 'cl-sort)
  (defun cl-sort (sequence predicate &rest _keys)
    (sort sequence predicate)))

;;;; --- cl-loop ----------------------------------------------------------

(when (or (not (boundp 'emacs-version))
          (emacs-cl-macros--define-p 'cl-loop))
  ;; cl-loop is incredibly complex; provide a minimal version that
  ;; handles the patterns anvil-memory uses (= for X in LIST do/collect).
  (defmacro cl-loop (&rest clauses)
    "Stub: minimal cl-loop supporting `for VAR in LIST do/collect/sum/count/...'.
For patterns this stub does not recognise, returns nil."
    (emacs-cl-macros--loop-build clauses)))

(unless (fboundp 'emacs-cl-macros--loop-build)
  (defun emacs-cl-macros--loop-build (clauses)
    "Build expansion for cl-loop CLAUSES.

Recognised shapes:
  for VAR in LIST                      iterator
  for VAR from N to M                  numeric iterator (Phase 4 B)
  for VAR from N below M               numeric iterator (Phase 4 B)
  with VAR = VAL                       binding
  do FORM …                            unconditional side-effect
  collect FORM                         accumulate into list
  sum FORM                             accumulate sum
  count FORM                           count truthy
  when COND return FORM                early-exit with FORM
  when COND do FORM                    conditional side-effect
  when COND collect FORM               conditional accumulate

The bodyless form `(cl-loop BODY...)' (= no for/with/do keyword,
just a body to repeat forever with `cl-return' for exit) is also
recognised — Phase 4 B added it so nelisp-regex.el's parse-concat
loops work.

Unrecognised shapes return nil (= caller gets a no-op expansion)."
    (let ((var nil) (list-form nil) (do-forms nil) (collect-form nil)
          (sum-form nil) (count-form nil) (with-bindings nil)
          (when-return-cond nil) (when-return-form nil)
          (when-do-cond nil) (when-do-forms nil)
          (when-collect-cond nil) (when-collect-form nil)
          (numeric-from nil) (numeric-to nil) (numeric-below nil)
          (bodyless-forms nil)
          (cur clauses) (recognised t))
      ;; Phase 4 B (2026-05-06): detect the *bodyless* form first.
      ;; If the very first clause is not a known keyword, treat the
      ;; whole CLAUSES as a body that repeats forever.  The expansion
      ;; wraps it in a `cl-block nil' so `cl-return' exits cleanly.
      (when (and clauses
                 (not (memq (car clauses)
                            '(for with do collect sum count when
                                  while until repeat finally return
                                  named))))
        (setq bodyless-forms clauses
              cur nil
              recognised t))
      (while (and cur recognised)
        (let ((kw (car cur)))
          (cond
           ((eq kw 'for)
            (setq var (car (cdr cur)))
            (cond
             ((eq (car (cdr (cdr cur))) 'in)
              (setq list-form (car (cdr (cdr (cdr cur)))))
              (setq cur (cdr (cdr (cdr (cdr cur))))))
             ;; Phase 4 B: `for VAR from N {to,below} M' numeric form.
             ((eq (car (cdr (cdr cur))) 'from)
              (setq numeric-from (car (cdr (cdr (cdr cur)))))
              (let ((kw2 (car (cdr (cdr (cdr (cdr cur))))))
                    (val2 (car (cdr (cdr (cdr (cdr (cdr cur))))))))
                (cond
                 ((eq kw2 'to)
                  (setq numeric-to val2)
                  (setq cur (cdr (cdr (cdr (cdr (cdr (cdr cur))))))))
                 ((eq kw2 'below)
                  (setq numeric-below val2)
                  (setq cur (cdr (cdr (cdr (cdr (cdr (cdr cur))))))))
                 (t (setq recognised nil)))))
             (t
              ;; Unsupported `for' form (= `on LIST' etc.).  Mark
              ;; unrecognised so the outer while bails — without this
              ;; guard `cur' never advances and we hang.  Callers
              ;; needing the unsupported shapes must pre-rewrite into
              ;; a plain `while' / `dolist' loop.
              (setq recognised nil))))
           ((eq kw 'do)
            (setq do-forms (cons (car (cdr cur)) do-forms))
            (setq cur (cdr (cdr cur))))
           ((eq kw 'collect)
            (setq collect-form (car (cdr cur)))
            (setq cur (cdr (cdr cur))))
           ((eq kw 'sum)
            (setq sum-form (car (cdr cur)))
            (setq cur (cdr (cdr cur))))
           ((eq kw 'count)
            (setq count-form (car (cdr cur)))
            (setq cur (cdr (cdr cur))))
           ((eq kw 'with)
            (let ((wname (car (cdr cur))))
              (when (eq (car (cdr (cdr cur))) '=)
                (setq with-bindings
                      (append with-bindings
                              (list (list wname (car (cdr (cdr (cdr cur))))))))
                (setq cur (cdr (cdr (cdr (cdr cur))))))))
           ;; `when COND return FORM' — early exit with FORM.
           ;; `when COND do FORM' — conditional side-effect.
           ((eq kw 'when)
            (let ((cond-form (car (cdr cur)))
                  (next-kw (car (cdr (cdr cur))))
                  (next-form (car (cdr (cdr (cdr cur))))))
              (cond
               ((eq next-kw 'return)
                (setq when-return-cond cond-form
                      when-return-form next-form
                      cur (cdr (cdr (cdr (cdr cur))))))
               ((eq next-kw 'do)
                (setq when-do-cond cond-form
                      when-do-forms (cons next-form when-do-forms)
                      cur (cdr (cdr (cdr (cdr cur))))))
               ((eq next-kw 'collect)
                (setq when-collect-cond cond-form
                      when-collect-form next-form
                      cur (cdr (cdr (cdr (cdr cur))))))
               (t (setq recognised nil)))))
           (t (setq recognised nil)))))
      (cond
       ((not recognised) nil)
       ;; Phase 4 B: bodyless infinite loop wrapped in `cl-block nil'
       ;; so a `cl-return' inside BODY exits cleanly.  Used by
       ;; nelisp-regex.el's parse-concat / parse-alt scan loops.
       (bodyless-forms
        (list 'cl-block nil
              (cons 'while
                    (cons t bodyless-forms))))
       ;; Phase 4 B: numeric `for VAR from N to M' / `from N below M'.
       ((and numeric-from (or numeric-to numeric-below))
        (let ((cmp (if numeric-to '<= '<))
              (limit (or numeric-to numeric-below))
              (rev nil))
          (while do-forms (setq rev (cons (car do-forms) rev))
                 (setq do-forms (cdr do-forms)))
          (list 'let (cons (list var numeric-from) with-bindings)
                (list 'while (list cmp var limit)
                      (cons 'progn rev)
                      (list 'setq var (list '1+ var))))))
       ;; `when COND return FORM' — wrap iteration in a catch and
       ;; throw on first hit.  Result of cl-loop = FORM (or nil).
       (when-return-cond
        (let ((tag-sym (make-symbol "--loop-tag--"))
              (result-sym (make-symbol "--loop-r--")))
          (list 'let (cons (list result-sym nil) with-bindings)
                (list 'catch (list 'quote tag-sym)
                      (list 'dolist (list var list-form)
                            (list 'when when-return-cond
                                  (list 'setq result-sym when-return-form)
                                  (list 'throw (list 'quote tag-sym) nil))))
                result-sym)))
       (collect-form
        (let ((acc-sym (make-symbol "--loop-acc--")))
          (list 'let (cons (list acc-sym nil) with-bindings)
                (list 'dolist (list var list-form)
                      (list 'setq acc-sym (list 'cons collect-form acc-sym)))
                (list 'nreverse acc-sym))))
       (when-collect-cond
        (let ((acc-sym (make-symbol "--loop-acc--")))
          (list 'let (cons (list acc-sym nil) with-bindings)
                (list 'dolist (list var list-form)
                      (list 'when when-collect-cond
                            (list 'setq acc-sym
                                  (list 'cons when-collect-form acc-sym))))
                (list 'nreverse acc-sym))))
       (sum-form
        (let ((acc-sym (make-symbol "--loop-sum--")))
          (list 'let (cons (list acc-sym 0) with-bindings)
                (list 'dolist (list var list-form)
                      (list 'setq acc-sym (list '+ acc-sym sum-form)))
                acc-sym)))
       (count-form
        (let ((acc-sym (make-symbol "--loop-count--")))
          (list 'let (cons (list acc-sym 0) with-bindings)
                (list 'dolist (list var list-form)
                      (list 'when count-form
                            (list 'setq acc-sym (list '+ acc-sym 1))))
                acc-sym)))
       (when-do-cond
        ;; `when COND do FORMS …'
        (let ((rev nil))
          (while when-do-forms
            (setq rev (cons (car when-do-forms) rev))
            (setq when-do-forms (cdr when-do-forms)))
          (list 'let with-bindings
                (list 'dolist (list var list-form)
                      (cons 'when (cons when-do-cond rev))))))
       (do-forms
        (let ((rev nil))
          (while do-forms (setq rev (cons (car do-forms) rev)) (setq do-forms (cdr do-forms)))
          (list 'let with-bindings
                (cons 'dolist (cons (list var list-form) rev)))))
       (t (list 'let with-bindings nil))))))

;;;; --- cl-defgeneric / cl-defmethod / cl-defstruct -------------------

(unless (fboundp 'cl-defgeneric)
  (defmacro cl-defgeneric (name arglist &rest body)
    "Stub: defgeneric → defun (= no real generic dispatch)."
    (cons 'defun (cons name (cons arglist body)))))

(unless (fboundp 'cl-defmethod)
  (defmacro cl-defmethod (name arglist &rest body)
    "Stub: defmethod → defun (= last-defined wins, no specializer dispatch).
When NAME is a setf-method list `(setf X)', intern the printed form
`\"(setf X)\"' as a symbol so `defun' has a usable target.  Strips
specializer cons-cells from arglist (e.g. `(SEQUENCE array)' → `SEQUENCE')."
    (let ((real-name
           (cond
            ((symbolp name) name)
            ((and (consp name) (eq (car name) 'setf))
             (intern (format "(setf %s)" (car (cdr name)))))
            (t (intern (format "%S" name))))))
      (cons 'defun
            (cons real-name
                  (cons (mapcar (lambda (a) (if (consp a) (car a) a)) arglist)
                        body))))))

(when (or (not (boundp 'emacs-version))
          (emacs-cl-macros--define-p 'cl-defstruct))
  (defmacro cl-defstruct (name &rest slots)
    "Stub: defstruct → minimal alist-backed accessors.

Skips a leading docstring among SLOTS (= host `cl-defstruct'
accepts an optional docstring before the slot list).  For the
NAME-options shape `(NAME (:constructor X) (:copier nil) ...)'
extracts X as the constructor name when supplied (otherwise
defaults to `make-NAME')."
    (let* ((sname (if (consp name) (car name) name))
           ;; Walk NAME's option list (= cdr when name is a cons)
           ;; and pick out (:constructor X) — earliest wins.
           (ctor-from-opts
            (and (consp name)
                 (let ((opts (cdr name)) found)
                   (while (and opts (not found))
                     (let ((o (car opts)))
                       (when (and (consp o) (eq (car o) :constructor)
                                  (consp (cdr o)) (symbolp (cadr o)))
                         (setq found (cadr o))))
                     (setq opts (cdr opts)))
                   found)))
           (ctor-name (or ctor-from-opts
                          (intern (concat "make-" (symbol-name sname)))))
           ;; (:predicate X) → use X as the predicate name instead of
           ;; the default `NAME-p'.
           (pred-from-opts
            (and (consp name)
                 (let ((opts (cdr name)) found)
                   (while (and opts (not found))
                     (let ((o (car opts)))
                       (when (and (consp o) (eq (car o) :predicate)
                                  (consp (cdr o)) (symbolp (cadr o)))
                         (setq found (cadr o))))
                     (setq opts (cdr opts)))
                   found)))
           (pred-name (or pred-from-opts
                          (intern (concat (symbol-name sname) "-p"))))
           (conc-from-opts
            (and (consp name)
                 (let ((opts (cdr name)) found seen)
                   (while (and opts (not seen))
                     (let ((o (car opts)))
                       (when (and (consp o) (eq (car o) :conc-name))
                         (setq seen t)
                         (setq found (cadr o))))
                     (setq opts (cdr opts)))
                   (if seen found :absent))))
           (conc-name (cond
                       ((eq conc-from-opts :absent)
                        (concat (symbol-name sname) "-"))
                       ((null conc-from-opts) "")
                       ((symbolp conc-from-opts)
                        (symbol-name conc-from-opts))
                       ((stringp conc-from-opts)
                        conc-from-opts)
                       (t (concat (symbol-name sname) "-"))))
           ;; If the first element of SLOTS is a string, treat it as
           ;; the struct's docstring and drop it before slot-name
           ;; extraction.
           (slot-list (if (and (consp slots) (stringp (car slots)))
                          (cdr slots)
                        slots))
           (slot-names (mapcar (lambda (s) (if (consp s) (car s) s)) slot-list))
           (slot-defaults
            (mapcar (lambda (s)
                      (let ((slot (if (consp s) (car s) s))
                            (default (and (consp s)
                                          (consp (cdr s))
                                          (cadr s))))
                        (cons slot default)))
                    slot-list)))
      (let ((forms nil))
        ;; CTOR constructor → returns alist of slots.
        ;; Built with `list' so the inner `sname' splice is explicit
        ;; (= nelisp's reader rejects `,X' outside a backquote, so we
        ;; cannot use the convenient backtick form here).
        (push (list 'defun ctor-name
                    '(&rest args)
                    (list 'let
                          (list
                           (list 'alist
                                 (cons 'list
                                       (mapcar
                                        (lambda (cell)
                                          (list 'cons
                                                (list 'quote
                                                      (intern
                                                       (concat ":"
                                                               (symbol-name
                                                                (car cell)))))
                                                (cdr cell)))
                                        slot-defaults)))
                           '(cur args))
                          '(while cur
                             (let ((cell (assoc (car cur) alist)))
                               (if cell
                                   (setcdr cell (car (cdr cur)))
                                 (setq alist
                                       (cons (cons (car cur)
                                                   (car (cdr cur)))
                                             alist))))
                             (setq cur (cdr (cdr cur))))
                          (list 'cons (list 'quote sname) 'alist)))
              forms)
        ;; NAME-p predicate (or whatever (:predicate X) renamed it to).
        (push (list 'defun pred-name
                    '(obj)
                    (list 'and '(consp obj) (list 'eq '(car obj) (list 'quote sname))))
              forms)
        ;; NAME-SLOT accessor + setter for each slot.
        ;;
        ;; The accessor returns (cdr (assoc :slot (cdr obj))).
        ;; The setter mutates the cell when present, otherwise pushes a
        ;; fresh (cons :slot value) onto (cdr obj).  Setter is bound on
        ;; both `name-slot--setter` (callable) and on the accessor's
        ;; symbol `cl-struct-setter` property so that our minimal `setf`
        ;; macro can find it via `(get accessor 'cl-struct-setter)`.
        (dolist (slot slot-names)
          (let* ((kw (intern (concat ":" (symbol-name slot))))
                 (acc (intern (concat conc-name (symbol-name slot))))
                 (setter (intern (concat conc-name (symbol-name slot) "--setter")))
                 (gv-setter (intern (format "(setf %s)" acc))))
            (push (list 'defun acc
                        '(obj)
                        (list 'cdr (list 'assoc kw '(cdr obj))))
                  forms)
            (push (list 'defun setter
                        '(obj val)
                        (list 'let
                              (list (list 'cell (list 'assoc kw '(cdr obj))))
                              (list 'if 'cell
                                    '(progn (setcdr cell val) val)
                                    (list 'progn
                                          (list 'setcdr 'obj
                                                (list 'cons (list 'cons kw 'val) '(cdr obj)))
                                          'val))))
                  forms)
            (push (list 'put (list 'quote acc)
                        (list 'quote 'cl-struct-setter)
                        (list 'quote setter))
                  forms)
            (push (list 'when '(boundp 'emacs-version)
                        (list 'defalias
                              (list 'quote gv-setter)
                              (list 'lambda '(val obj)
                                    (list setter 'obj 'val))))
                  forms)))
        (cons 'progn (nreverse forms))))))

;;;; --- cl-case / cl-pushnew --------------------------------------------

(unless (fboundp 'cl-case)
  (defmacro cl-case (expr &rest cases)
    "Stub: cl-case → equivalent to cond with eql tests."
    (let ((value-sym (make-symbol "--cl-case--"))
          (clauses nil))
      (dolist (c cases)
        (let ((key (car c)) (body (cdr c)))
          (cond
           ((or (eq key 't) (eq key 'otherwise))
            (push (cons t body) clauses))
           ((listp key)
            (push (cons (list 'memql value-sym (list 'quote key)) body) clauses))
           (t (push (cons (list 'eql value-sym (list 'quote key)) body) clauses)))))
      (let ((rev nil))
        (while clauses (setq rev (cons (car clauses) rev)) (setq clauses (cdr clauses)))
        (list 'let (list (list value-sym expr))
              (cons 'cond rev))))))

(when (or (not (boundp 'emacs-version))
          (emacs-cl-macros--define-p 'cl-pushnew))
  (defmacro cl-pushnew (item place &rest _keys)
    (list 'unless (list 'member item place)
          (list 'setq place (list 'cons item place)))))

;;;; --- cl-letf / cl-flet / cl-block -----------------------------------

(when (or (not (boundp 'emacs-version))
          (emacs-cl-macros--define-p 'cl-letf))
  (defmacro cl-letf (bindings &rest body)
    "Minimal `cl-letf' for variable and function-cell bindings.
This covers the common test/vendor pattern of temporarily rebinding
`(symbol-function 'foo)' while preserving plain lexical `let*'
bindings."
    (let (let-bindings
          setup-forms
          cleanup-forms)
      (dolist (binding bindings)
        (let ((place (car binding))
              (value (cadr binding)))
          (cond
           ((symbolp place)
            (push binding let-bindings))
           ((and (consp place)
                 (eq (car place) 'symbol-function)
                 (consp (cdr place))
                 (eq (caadr place) 'quote))
            (let* ((symbol (cadadr place))
                   (had (make-symbol "cl-letf-had-function"))
                   (old (make-symbol "cl-letf-old-function")))
              (push (list had (list 'fboundp (list 'quote symbol)))
                    let-bindings)
              (push (list old
                          (list 'and had
                                (list 'symbol-function
                                      (list 'quote symbol))))
                    let-bindings)
              (push (list 'let '((native-comp-enable-subr-trampolines nil))
                          (list 'fset (list 'quote symbol) value))
                    setup-forms)
              (push (list 'if had
                          (list 'let '((native-comp-enable-subr-trampolines nil))
                                (list 'fset (list 'quote symbol) old))
                          (list 'fmakunbound (list 'quote symbol)))
                    cleanup-forms)))
           (t
            (error "cl-letf: unsupported place: %S" place)))))
      (list 'let* (nreverse let-bindings)
            (list 'unwind-protect
                  (cons 'progn
                        (append (nreverse setup-forms) body))
                  (cons 'progn cleanup-forms))))))

(when (or (not (boundp 'emacs-version))
          (emacs-cl-macros--define-p 'cl-letf*))
  (defalias 'cl-letf* 'cl-letf))

(unless (fboundp 'cl-flet)
  (defmacro cl-flet (bindings &rest body)
    "Doc 51 (2026-05-04) MVP `cl-flet'.

Each BINDINGS entry is `(NAME ARGS . BODY-FORMS)' — install
each NAME globally as a function while BODY runs, restore the
previous binding (or unbind) on exit via `unwind-protect'.
Less hygienic than upstream's code-walker (= the names are
visible globally during BODY), but matches every Layer 2
caller we have audited (e.g. `cl-flet' in `emacs-redisplay'
defining `emit-before-strings' / `emit-after-strings')."
    (let* ((names (mapcar #'car bindings))
           (saves nil)
           (installs nil)
           (restores nil))
      (dolist (b bindings)
        (let* ((name (car b))
               (args (cadr b))
               (body-forms (cddr b))
               (sym-saved (intern (concat "--cl-flet-saved-"
                                          (symbol-name name)))))
          (push (list sym-saved
                      (list 'and (list 'fboundp (list 'quote name))
                            (list 'symbol-function (list 'quote name))))
                saves)
          (push (list 'defalias (list 'quote name)
                      (cons 'lambda (cons args body-forms)))
                installs)
          (push (list 'if sym-saved
                      (list 'defalias (list 'quote name) sym-saved)
                      (list 'fmakunbound (list 'quote name)))
                restores)))
      (ignore names)
      (list 'let (nreverse saves)
            (cons 'unwind-protect
                  (cons (cons 'progn
                              (append (nreverse installs)
                                      body))
                        (nreverse restores)))))))

(unless (fboundp 'cl-labels)
  (defalias 'cl-labels 'cl-flet))

;; cl-block / cl-return-from / cl-return:
;;   - host driver:    host emacs's cl-lib provides real impls
;;   - nelisp driver:  NeLisp upstream `lisp/nelisp-cl-macros.el'
;;                     ships the real catch+throw impl as part of
;;                     STDLIB_SOURCES (= Rust-min migration 2026-05-06)
;; Either way `(fboundp 'cl-block)` is true at load time so the stub
;; below stays inert.  Kept as a fallback for environments that load
;; this module without either driver providing the real impl.
(unless (fboundp 'cl-block)
  (defmacro cl-block (_name &rest body)
    "Stub: cl-block → progn (= no return-from support)."
    (cons 'progn body)))

;;;; --- cl-getf / cl-first/second/third --------------------------------

(unless (fboundp 'cl-getf)
  (defalias 'cl-getf 'plist-get))

(unless (fboundp 'cl-first)
  (defalias 'cl-first 'car))
(unless (fboundp 'cl-second)
  (defun cl-second (l) (car (cdr l))))
(unless (fboundp 'cl-third)
  (defun cl-third (l) (car (cdr (cdr l)))))
(unless (fboundp 'cl-rest)
  (defalias 'cl-rest 'cdr))

;; cl-defmacro: minimal stub that aliases plain `defmacro'.  Common Lisp
;; arglist features (&key / &whole etc. on macro arglists) are not used
;; by the Phase 1e nelisp-actor / nelisp-process call sites, so the
;; thin alias is sufficient.  If a future caller needs CL arglist
;; semantics inside macros, swap this for the same expansion logic
;; `cl-defun' uses above.
(unless (fboundp 'cl-defmacro)
  (defmacro cl-defmacro (name arglist &rest body)
    "Phase 1e stub: forwards to plain `defmacro'."
    (cons 'defmacro (cons name (cons arglist body)))))

;; List helpers commonly used by upstream packages.  caar / cadr / cddr
;; are already defined by `emacs-list.el'; cdar is missing.
(unless (fboundp 'caar)
  (defun caar (x) (car (car x))))
(unless (fboundp 'cdar)
  (defun cdar (x) (cdr (car x))))

;;;; --- feature provides ------------------------------------------------

;; Provide cl-macs / cl-seq as features so vendor (require ...) chains
;; succeed without actually loading the heavyweight files.
(unless (featurep 'cl-macs) (provide 'cl-macs))
(unless (featurep 'cl-seq) (provide 'cl-seq))
(unless (featurep 'cl-extra) (provide 'cl-extra))
(unless (featurep 'cl-generic) (provide 'cl-generic))

(provide 'emacs-cl-macros)

;;; emacs-cl-macros.el ends here
