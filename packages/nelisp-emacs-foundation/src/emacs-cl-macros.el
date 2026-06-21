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
;;             cl-defgeneric, cl-defmethod, cl-deftype, cl-progv,
;;             letrec
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
           (autoloadp (symbol-function symbol)))
      (get symbol 'emacs-stub-placeholder)))

(defun emacs-cl-macros--defstruct-ctor-parts (arglist)
  "Return (FORMALS AUX-BINDINGS VALUE-SYMS) for constructor ARGLIST.
`cl-defstruct' constructor lambda lists can mention non-slot helper
arguments and `&aux' bindings.  The standalone evaluator does not bind
`&aux' itself, so generated constructors lower those bindings into an
ordinary `let'."
  (let ((cur arglist)
        (formals nil)
        (aux-bindings nil)
        (value-syms nil)
        (in-aux nil))
    (while cur
      (let ((item (car cur)))
        (cond
         ((eq item '&aux)
          (setq in-aux t))
         (in-aux
          (let ((var (if (consp item) (car item) item))
                (init (and (consp item) (consp (cdr item)) (cadr item))))
            (push (list var init) aux-bindings)
            (push var value-syms)))
         ((memq item '(&optional &rest))
          (push item formals))
         ((consp item)
          (let ((var (car item)))
            (push var formals)
            (push var value-syms)))
         (t
          (push item formals)
          (push item value-syms))))
      (setq cur (cdr cur)))
    (list (nreverse formals)
          (nreverse aux-bindings)
          (nreverse value-syms))))

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

;;;; --- cl numeric predicates (Doc 15 B4 breadth) ---------------------
;; cl-evenp / cl-oddp / cl-plusp / cl-minusp were void; many packages and
;; `cl-loop' clauses rely on them.  Plain defuns (the cl-lib `cl-defsubst'
;; forms are unavailable here).

(unless (fboundp 'cl-evenp)
  (defun cl-evenp (x) "Return non-nil if integer X is even." (= 0 (% x 2))))

(unless (fboundp 'cl-oddp)
  (defun cl-oddp (x) "Return non-nil if integer X is odd." (not (= 0 (% x 2)))))

(unless (fboundp 'cl-plusp)
  (defun cl-plusp (x) "Return non-nil if number X is positive." (> x 0)))

(unless (fboundp 'cl-minusp)
  (defun cl-minusp (x) "Return non-nil if number X is negative." (< x 0)))

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

(unless (fboundp 'cl-position-if)
  (defun cl-position-if (predicate sequence &rest keys)
    "Return index of first element in SEQUENCE matching PREDICATE.

Supports the same minimal sequence shapes and `:from-end' key as the
local `cl-position' shim."
    (let* ((from-end (plist-get keys :from-end))
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
            (when (funcall predicate (funcall get-elt i))
              (setq found i))
            (setq i (1- i)))
          found))
       (t
        (let ((i 0) (found nil))
          (while (and (< i n) (not found))
            (when (funcall predicate (funcall get-elt i))
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

(unless (fboundp 'emacs-cl-macros--loop-destructure-bindings)
  (defun emacs-cl-macros--loop-destructure-bindings (pattern source)
    "Return `let' bindings destructuring PATTERN from SOURCE."
    (let ((bindings nil)
          (cur pattern)
          (access source))
      (while (consp cur)
        (when (car cur)
          (setq bindings
                (cons (list (car cur) (list 'car access)) bindings)))
        (setq access (list 'cdr access))
        (setq cur (cdr cur)))
      (when cur
        (setq bindings (cons (list cur access) bindings)))
      (nreverse bindings))))

(unless (fboundp 'emacs-cl-macros--loop-wrap-body)
  (defun emacs-cl-macros--loop-wrap-body (pattern item forms)
    "Return one loop body form for PATTERN bound from ITEM and FORMS."
    (if (symbolp pattern)
        (cons 'progn forms)
      (cons 'let
            (cons (emacs-cl-macros--loop-destructure-bindings pattern item)
                  forms)))))

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
                      cur (cdr (cdr (cdr (cdr cur)))))
                (while (and cur (eq (car cur) 'and))
                  (let ((and-kw (car (cdr cur)))
                        (and-form (car (cdr (cdr cur)))))
                    (cond
                     ((eq and-kw 'do)
                      (setq when-do-forms (cons and-form when-do-forms)
                            cur (cdr (cdr (cdr cur)))))
                     ((eq and-kw 'collect)
                      (setq when-collect-cond cond-form
                            when-collect-form and-form
                            cur (cdr (cdr (cdr cur)))))
                     (t (setq recognised nil
                              cur nil))))))
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
              (result-sym (make-symbol "--loop-r--"))
              (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
          (list 'let (cons (list result-sym nil) with-bindings)
                (list 'catch (list 'quote tag-sym)
                      (list 'dolist (list loop-var list-form)
                            (emacs-cl-macros--loop-wrap-body
                             var loop-var
                             (list (list 'when when-return-cond
                                         (list 'setq result-sym when-return-form)
                                         (list 'throw (list 'quote tag-sym) nil))))))
                result-sym)))
       ((or collect-form when-collect-cond)
        (let ((acc-sym (make-symbol "--loop-acc--"))
              (loop-var (if (symbolp var) var (make-symbol "--loop-item--")))
              (body nil)
              (rev nil))
          (when collect-form
            (setq body
                  (append body
                          (list (list 'setq acc-sym
                                      (list 'cons collect-form acc-sym))))))
          (when when-do-cond
            (while when-do-forms
              (setq rev (cons (car when-do-forms) rev))
              (setq when-do-forms (cdr when-do-forms)))
            (setq body
                  (append body
                          (list (cons 'when
                                      (cons when-do-cond rev))))))
          (when when-collect-cond
            (setq body
                  (append body
                          (list (list 'when when-collect-cond
                                      (list 'setq acc-sym
                                            (list 'cons when-collect-form acc-sym)))))))
          (list 'let (cons (list acc-sym nil) with-bindings)
                (list 'dolist (list loop-var list-form)
                      (emacs-cl-macros--loop-wrap-body var loop-var body))
                (list 'nreverse acc-sym))))
       (sum-form
        (let ((acc-sym (make-symbol "--loop-sum--"))
              (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
          (list 'let (cons (list acc-sym 0) with-bindings)
                (list 'dolist (list loop-var list-form)
                      (emacs-cl-macros--loop-wrap-body
                       var loop-var
                       (list (list 'setq acc-sym (list '+ acc-sym sum-form)))))
                acc-sym)))
       (count-form
        (let ((acc-sym (make-symbol "--loop-count--"))
              (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
          (list 'let (cons (list acc-sym 0) with-bindings)
                (list 'dolist (list loop-var list-form)
                      (emacs-cl-macros--loop-wrap-body
                       var loop-var
                       (list (list 'when count-form
                                   (list 'setq acc-sym (list '+ acc-sym 1))))))
                acc-sym)))
       (when-do-cond
        ;; `when COND do FORMS …'
        (let ((rev nil)
              (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
          (while when-do-forms
            (setq rev (cons (car when-do-forms) rev))
            (setq when-do-forms (cdr when-do-forms)))
          (list 'let with-bindings
                (list 'dolist (list loop-var list-form)
                      (emacs-cl-macros--loop-wrap-body
                       var loop-var
                       (list (cons 'when (cons when-do-cond rev))))))))
       (do-forms
        (let ((rev nil)
              (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
          (while do-forms (setq rev (cons (car do-forms) rev)) (setq do-forms (cdr do-forms)))
          (list 'let with-bindings
                (list 'dolist (list loop-var list-form)
                      (emacs-cl-macros--loop-wrap-body var loop-var rev)))))
       (t (list 'let with-bindings nil))))))

;;;; --- cl-defgeneric / cl-defmethod / cl-defstruct -------------------

(when (or (not (boundp 'emacs-version))
          (emacs-cl-macros--define-p 'cl-deftype))
  (defmacro cl-deftype (name arglist &rest body)
    "Standalone load-time fallback: ignore CL type declarations."
    (ignore arglist body)
    (list 'quote name)))

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
    "Stub: defstruct → minimal alist/vector-backed accessors.

Skips a leading docstring among SLOTS (= host `cl-defstruct'
accepts an optional docstring before the slot list).  For the
NAME-options shape `(NAME (:constructor X) (:copier nil) ...)'
supports default, disabled, renamed, and positional constructors.
Also supports the `(:type vector)' shape used by `avl-tree.el'."
    (let* ((sname (if (consp name) (car name) name))
           (opts (and (consp name) (cdr name)))
           (ctor-opts nil)
           (ctor-saw nil)
           (pred-saw nil)
           (pred-from-opts nil)
           (type-from-opts nil)
           (conc-from-opts
            (let ((cur opts) found seen)
              (while (and cur (not seen))
                (let ((o (car cur)))
                  (when (and (consp o) (eq (car o) :conc-name))
                    (setq seen t)
                    (setq found (cadr o))))
                (setq cur (cdr cur)))
              (if seen found :absent)))
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
      (let ((cur opts))
        (while cur
          (let ((o (car cur)))
            (cond
             ((and (consp o) (eq (car o) :constructor))
              (setq ctor-saw t)
              (when (cadr o)
                (setq ctor-opts
                      (cons (list (cadr o) (cadr (cdr o))) ctor-opts))))
             ((and (consp o) (eq (car o) :predicate))
              (setq pred-saw t)
              (setq pred-from-opts (cadr o)))
             ((and (consp o) (eq (car o) :type))
              (setq type-from-opts (cadr o)))))
          (setq cur (cdr cur))))
      (unless ctor-saw
        (setq ctor-opts
              (list (list (intern (concat "make-" (symbol-name sname))) nil))))
      (let ((forms nil))
        ;; Constructor generation.  Positional `(:constructor NAME
        ;; (SLOT ...))' is needed by `avl-tree.el'; the no-arg option
        ;; keeps the old keyword constructor behavior.
        (dolist (ctor (nreverse ctor-opts))
          (let* ((ctor-name (car ctor))
                 (ctor-args (car (cdr ctor)))
                 (ctor-parts
                  (and ctor-args
                       (emacs-cl-macros--defstruct-ctor-parts ctor-args)))
                 (ctor-formals (car ctor-parts))
                 (ctor-aux-bindings (cadr ctor-parts))
                 (ctor-value-syms (caddr ctor-parts)))
            (push
             (if (eq type-from-opts 'vector)
                 (if ctor-args
                     (list 'defun ctor-name ctor-formals
                           (let ((body
                                  (cons 'vector
                                        (mapcar
                                         (lambda (slot)
                                           (if (memq slot ctor-value-syms)
                                               slot
                                             (cdr (assoc slot slot-defaults))))
                                         slot-names))))
                             (if ctor-aux-bindings
                                 (list 'let ctor-aux-bindings body)
                               body)))
                   (list 'defun ctor-name '(&rest args)
                         (list 'let
                               (list (list 'values
                                           (cons 'vector
                                                 (mapcar #'cdr slot-defaults)))
                                     '(cur args))
                               '(while cur
                                  (let ((pos (cl-position
                                              (intern
                                               (substring (symbol-name (car cur)) 1))
                                              (quote nil))))
                                    (ignore pos))
                                  (setq cur (cdr (cdr cur))))
                               'values)))
               (if ctor-args
                   (list 'defun ctor-name ctor-formals
                         (let ((body
                                (list 'cons
                                      (list 'quote sname)
                                      (cons 'list
                                            (mapcar
                                             (lambda (slot)
                                               (list 'cons
                                                     (list 'quote
                                                           (intern
                                                            (concat ":"
                                                                    (symbol-name slot))))
                                                     (if (memq slot ctor-value-syms)
                                                         slot
                                                       (cdr (assoc slot slot-defaults)))))
                                             slot-names)))))
                           (if ctor-aux-bindings
                               (list 'let ctor-aux-bindings body)
                             body)))
                 (list 'defun ctor-name
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
                             (list 'cons (list 'quote sname) 'alist)))))
             forms)))
        ;; NAME-p predicate (or whatever (:predicate X) renamed it to).
        (let ((pred-name (if pred-saw
                             pred-from-opts
                           (intern (concat (symbol-name sname) "-p")))))
          (when pred-name
            (push (list 'defun pred-name
                        '(obj)
                        (if (eq type-from-opts 'vector)
                            (list 'and '(vectorp obj)
                                  (list '= '(length obj) (length slot-names)))
                          (list 'and '(consp obj)
                                (list 'eq '(car obj) (list 'quote sname)))))
                  forms)))
        ;; NAME-SLOT accessor + setter for each slot.
        ;;
        ;; The alist accessor returns (cdr (assoc :slot (cdr obj))).
        ;; The vector accessor returns (aref obj INDEX).  Setter is
        ;; registered on the accessor's `cl-struct-setter` property so
        ;; our minimal `setf` macro can find it.
        (let ((index 0))
          (dolist (slot slot-names)
          (let* ((kw (intern (concat ":" (symbol-name slot))))
                 (acc (intern (concat conc-name (symbol-name slot))))
                 (setter (intern (concat conc-name (symbol-name slot) "--setter")))
                 (gv-setter (intern (format "(setf %s)" acc))))
            (push (if (eq type-from-opts 'vector)
                      (list 'defun acc '(obj) (list 'aref 'obj index))
                    (list 'defun acc
                          '(obj)
                          (list 'cdr (list 'assoc kw '(cdr obj)))))
                  forms)
            (push (if (eq type-from-opts 'vector)
                      (list 'defun setter '(obj val)
                            (list 'aset 'obj index 'val))
                    (list 'defun setter
                          '(obj val)
                          (list 'let
                                (list (list 'cell (list 'assoc kw '(cdr obj))))
                                (list 'if 'cell
                                      '(progn (setcdr cell val) val)
                                      (list 'progn
                                            (list 'setcdr 'obj
                                                  (list 'cons (list 'cons kw 'val) '(cdr obj)))
                                            'val)))))
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
                  forms)
            (setq index (1+ index)))))
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

(unless (fboundp 'letrec)
  (defmacro letrec (bindings &rest body)
    "Minimal `letrec' for recursive local function values.
Each variable is bound before initializer evaluation, then assigned in
order.  This is enough for vendor load-time macros such as
`let-when-compile', where a local lambda recursively calls itself."
    (let ((lets nil)
          (sets nil)
          (cur bindings))
      (while cur
        (let ((binding (car cur)))
          (setq lets (cons (list (car binding) nil) lets))
          (setq sets
                (cons (list 'setq (car binding)
                            (car (cdr binding)))
                      sets)))
        (setq cur (cdr cur)))
      (cons 'let
            (cons (nreverse lets)
                  (append (nreverse sets) body))))))

(unless (fboundp 'cl-progv)
  (defmacro cl-progv (_symbols _values &rest body)
    "Load-time fallback for `cl-progv' using global value cells.
This is not full dynamic binding, but it is enough for vendor
`let-when-compile': bind a runtime list of symbols while BODY computes
macro-time constants, then restore previously bound values."
    (let ((syms (make-symbol "cl-progv-symbols"))
          (vals (make-symbol "cl-progv-values"))
          (saved (make-symbol "cl-progv-saved"))
          (cur (make-symbol "cl-progv-cur"))
          (vcur (make-symbol "cl-progv-vcur"))
          (sym (make-symbol "cl-progv-sym"))
          (val (make-symbol "cl-progv-val"))
          (cell (make-symbol "cl-progv-cell")))
      (list 'let (list (list syms _symbols)
                       (list vals _values)
                       (list saved nil))
            (list 'let (list (list cur syms)
                             (list vcur vals))
                  (list 'while cur
                        (list 'let (list (list sym (list 'car cur))
                                         (list val (list 'car vcur)))
                              (list 'setq saved
                                    (list 'cons
                                          (list 'list sym
                                                (list 'boundp sym)
                                                (list 'and
                                                      (list 'boundp sym)
                                                      (list 'symbol-value sym)))
                                          saved))
                              (list 'set sym val)
                              (list 'setq cur (list 'cdr cur))
                              (list 'setq vcur (list 'cdr vcur)))))
            (list 'unwind-protect
                  (cons 'progn body)
                  (list 'while saved
                        (list 'let (list (list cell (list 'car saved)))
                              (list 'if
                                    (list 'car (list 'cdr cell))
                                    (list 'set (list 'car cell)
                                          (list 'car (list 'cdr (list 'cdr cell))))
                                    ;; No `makunbound' in the small
                                    ;; bootstrap yet.  Restore unbound
                                    ;; cells to nil rather than leaking
                                    ;; macro-time values.
                                    (list 'set (list 'car cell) nil))
                              (list 'setq saved (list 'cdr saved)))))))))

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

(unless (fboundp 'emacs-cl-macros--symbol-macrolet-walk)
  (defun emacs-cl-macros--symbol-macrolet-walk (form env)
    "Replace symbol references in FORM according to ENV."
    (cond
     ((symbolp form)
      (let ((cell (assq form env)))
        (if cell (cdr cell) form)))
     ((not (consp form)) form)
     ((memq (car form) '(quote function)) form)
     ((eq (car form) 'setq)
      (let ((pairs (cdr form))
            (out nil))
        (while pairs
          (let* ((place (car pairs))
                 (value (cadr pairs))
                 (cell (and (symbolp place) (assq place env))))
            (setq out
                  (append out
                          (list (if cell (cdr cell) place)
                                (emacs-cl-macros--symbol-macrolet-walk
                                 value env)))))
          (setq pairs (cddr pairs)))
        (cons 'setq out)))
     ((memq (car form) '(let let*))
      (let ((bindings (cadr form))
            (body (cddr form))
            (shadowed nil)
            (new-bindings nil)
            new-env)
        (dolist (binding bindings)
          (let ((var (if (symbolp binding) binding (car binding))))
            (push var shadowed)
            (push (if (symbolp binding)
                      binding
                    (list var
                          (emacs-cl-macros--symbol-macrolet-walk
                           (cadr binding) env)))
                  new-bindings)))
        (setq new-env
              (let ((cur env) (acc nil))
                (while cur
                  (unless (memq (caar cur) shadowed)
                    (push (car cur) acc))
                  (setq cur (cdr cur)))
                (nreverse acc)))
        (cons (car form)
              (cons (nreverse new-bindings)
                    (mapcar (lambda (body-form)
                              (emacs-cl-macros--symbol-macrolet-walk
                               body-form new-env))
                            body)))))
     (t
      (mapcar (lambda (item)
                (emacs-cl-macros--symbol-macrolet-walk item env))
              form)))))

(unless (fboundp 'cl-symbol-macrolet)
  (defmacro cl-symbol-macrolet (bindings &rest body)
    "Minimal symbol macro substitution used by generator.el CPS rewrites."
    (let ((env (mapcar (lambda (binding)
                         (cons (car binding) (cadr binding)))
                       bindings)))
      (cons 'progn
            (mapcar (lambda (body-form)
                      (emacs-cl-macros--symbol-macrolet-walk body-form env))
                    body)))))

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

;;;; --- Doc 16 breadth round 5: cl numeric + list helpers (were void) ---
;; cl-caddr / cl-signum / cl-gcd / cl-lcm / cl-isqrt / cl-list* /
;; cl-revappend / cl-ldiff were void in the standalone runtime.  All are
;; keyword-free and gated on `unless (fboundp ...)'.  cl-gcd uses `mod'
;; with positive operands only (the bare reader has no `/=', and the
;; runtime's `mod' is truncate-semantics for negatives + 2-arg `floor' is
;; broken -- so these stay clear of those primitives.  The division-family
;; cl helpers are added in round 6 below, working around the same bugs.

(unless (fboundp 'cl-caddr)
  (defun cl-caddr (x) "Return the `car' of the `cddr' of X." (car (cddr x))))

(unless (fboundp 'cl-signum)
  (defun cl-signum (x)
    "Return 1 if X is positive, -1 if negative, 0 if zero.
Matches Emacs `cl-signum', which always returns an integer sign."
    (cond ((> x 0) 1)
          ((< x 0) -1)
          (t 0))))

(unless (fboundp 'cl-gcd)
  (defun cl-gcd (&rest args)
    "Return the greatest common divisor of the integer ARGS."
    (let ((g 0))
      (dolist (a args g)
        (setq a (abs a))
        (while (not (= a 0))
          (let ((r (mod g a))) (setq g a a r)))))))

(unless (fboundp 'cl-lcm)
  (defun cl-lcm (&rest args)
    "Return the least common multiple of the integer ARGS."
    (if (memq 0 args)
        0
      (let ((l 1))
        (dolist (a args l)
          (setq a (abs a))
          (setq l (/ (* l a) (cl-gcd l a))))))))

(unless (fboundp 'cl-isqrt)
  (defun cl-isqrt (n)
    "Return the integer square root of the non-negative integer N."
    (if (< n 2)
        n
      (let ((x n) (y (/ (+ 1 n) 2)))
        (while (< y x)
          (setq x y y (/ (+ x (/ n x)) 2)))
        x))))

(unless (fboundp 'cl-list*)
  (defun cl-list* (arg &rest args)
    "Return a list of ARG and ARGS, using the last argument as the tail."
    (if args (cons arg (apply #'cl-list* args)) arg)))

(unless (fboundp 'cl-revappend)
  (defun cl-revappend (list tail)
    "Return a copy of LIST reversed, with TAIL appended to the end."
    (append (reverse list) tail)))

(unless (fboundp 'cl-ldiff)
  (defun cl-ldiff (list sublist)
    "Return a copy of LIST up to (but not including) the cons cell SUBLIST."
    (let ((result nil))
      (while (and (consp list) (not (eq list sublist)))
        (push (car list) result)
        (setq list (cdr list)))
      (nreverse result))))

;;;; --- Doc 16 breadth round 6: cl division family (were void) ---------
;; cl-floor / cl-ceiling / cl-round / cl-truncate / cl-mod / cl-rem each
;; return (QUOTIENT REMAINDER) per Emacs `cl-lib'.  The runtime's native
;; 2-arg `floor'/`ceiling'/`truncate' ignore the divisor and `mod' is
;; truncate-semantics for negatives, so these are built only from `/'
;; (correct toward-zero truncation) plus the 1-arg `floor'/`ceiling'/
;; `round'/`truncate' (correct on int and float): integers use an exact
;; truncate-then-adjust, floats use the real ratio then a 1-arg rounding.

(unless (fboundp 'cl-truncate)
  (defun cl-truncate (x &optional y)
    "Return (list Q R) where Q = X/Y truncated toward zero and R = X - Y*Q.
Y defaults to 1."
    (setq y (or y 1))
    (if (or (floatp x) (floatp y))
        (let* ((q (truncate (/ x y))) (r (- x (* y q)))) (list q r))
      (let* ((q (/ x y)) (r (- x (* y q)))) (list q r)))))

(unless (fboundp 'cl-floor)
  (defun cl-floor (x &optional y)
    "Return (list Q R) where Q = floor of X/Y and R = X - Y*Q.  Y defaults to 1."
    (setq y (or y 1))
    (if (or (floatp x) (floatp y))
        (let* ((q (floor (/ x y))) (r (- x (* y q)))) (list q r))
      (let* ((q (/ x y)) (r (- x (* y q))))
        (when (and (not (= r 0)) (< (* x y) 0))
          (setq q (- q 1) r (+ r y)))
        (list q r)))))

(unless (fboundp 'cl-ceiling)
  (defun cl-ceiling (x &optional y)
    "Return (list Q R) where Q = ceiling of X/Y and R = X - Y*Q.  Y defaults to 1."
    (setq y (or y 1))
    (if (or (floatp x) (floatp y))
        (let* ((q (ceiling (/ x y))) (r (- x (* y q)))) (list q r))
      (let* ((q (/ x y)) (r (- x (* y q))))
        (when (and (not (= r 0)) (> (* x y) 0))
          (setq q (+ q 1) r (- r y)))
        (list q r)))))

(unless (fboundp 'cl-round)
  (defun cl-round (x &optional y)
    "Return (list Q R), Q = X/Y rounded to nearest (ties to even), R = X - Y*Q.
Y defaults to 1.  Float arguments use the runtime's 1-arg `round'."
    (setq y (or y 1))
    (if (or (floatp x) (floatp y))
        (let* ((q (round (/ x y))) (r (- x (* y q)))) (list q r))
      (let ((sx x) (sy y))
        (when (< sy 0) (setq sx (- sx) sy (- sy)))
        (let* ((q (/ sx sy)) (rr (- sx (* sy q))))
          (when (< rr 0) (setq q (- q 1) rr (+ rr sy)))
          (let ((twice (* 2 rr)))
            (cond ((< twice sy) nil)
                  ((> twice sy) (setq q (+ q 1)))
                  (t (when (not (= 0 (% q 2))) (setq q (+ q 1))))))
          (list q (- x (* y q))))))))

(unless (fboundp 'cl-mod)
  (defun cl-mod (x y)
    "Return X modulo Y -- the remainder of `cl-floor' (same sign as Y)."
    (nth 1 (cl-floor x y))))

(unless (fboundp 'cl-rem)
  (defun cl-rem (x y)
    "Return the remainder of X / Y truncated toward zero (same sign as X)."
    (nth 1 (cl-truncate x y))))

;;;; --- Doc 16 breadth round 9: keyword cl sequence helpers (were void) -
;; cl-remove-duplicates / cl-count / cl-count-if / cl-reduce / cl-adjoin /
;; cl-set-exclusive-or / cl-substitute were void.  Each honours the common
;; `:test' / `:key' (and `:from-end' / `:initial-value' where applicable)
;; keywords via `&rest keys' + `plist-get', matching the local `cl-position'
;; convention.  Default `:test' is `eql' (correct for numbers/chars and,
;; like `eq', non-recursive so cyclic structures are safe).  Gated on
;; `unless (fboundp ...)'.

(defun emacs-cl-macros--member-test (item list test key)
  "Return non-nil if key ITEM matches any element of LIST under TEST/KEY."
  (let ((found nil))
    (while (and list (not found))
      (when (funcall test item (if key (funcall key (car list)) (car list)))
        (setq found t))
      (setq list (cdr list)))
    found))

(unless (fboundp 'cl-remove-duplicates)
  (defun cl-remove-duplicates (seq &rest keys)
    "Return a copy of SEQ (as a list) with duplicate elements removed.
By default the last of each set of duplicates is kept; `:from-end t'
keeps the first.  Honours `:test' and `:key'."
    (let* ((test (or (plist-get keys :test) #'eql))
           (key (plist-get keys :key))
           (from-end (plist-get keys :from-end))
           (list (append seq nil))
           (result nil))
      (if from-end
          (dolist (elt list)
            (let ((k (if key (funcall key elt) elt)))
              (unless (emacs-cl-macros--member-test k result test key)
                (push elt result))))
        (let ((tail list))
          (while tail
            (let ((k (if key (funcall key (car tail)) (car tail))))
              (unless (emacs-cl-macros--member-test k (cdr tail) test key)
                (push (car tail) result)))
            (setq tail (cdr tail)))))
      (nreverse result))))

(unless (fboundp 'cl-count)
  (defun cl-count (item seq &rest keys)
    "Return the number of elements of SEQ equal to ITEM.
Honours `:test' and `:key'."
    (let ((test (or (plist-get keys :test) #'eql))
          (key (plist-get keys :key))
          (n 0))
      (dolist (elt (append seq nil) n)
        (when (funcall test item (if key (funcall key elt) elt))
          (setq n (1+ n)))))))

(unless (fboundp 'cl-count-if)
  (defun cl-count-if (pred seq &rest keys)
    "Return the number of elements of SEQ that satisfy PRED.  Honours `:key'."
    (let ((key (plist-get keys :key))
          (n 0))
      (dolist (elt (append seq nil) n)
        (when (funcall pred (if key (funcall key elt) elt))
          (setq n (1+ n)))))))

(unless (fboundp 'cl-reduce)
  (defun cl-reduce (fn seq &rest keys)
    "Reduce SEQ using the binary function FN.
Honours `:initial-value', `:from-end' and `:key'.  With `:from-end t'
FN is applied right-to-left as (FN ELT ACC); otherwise left-to-right as
(FN ACC ELT).  With no elements and no `:initial-value', returns (FN)."
    (let* ((key (plist-get keys :key))
           (from-end (plist-get keys :from-end))
           (has-init (plist-member keys :initial-value))
           (init (plist-get keys :initial-value))
           (list (append seq nil)))
      (when key (setq list (mapcar (lambda (e) (funcall key e)) list)))
      (when from-end (setq list (reverse list)))
      (let (acc rest)
        (cond (has-init (setq acc init rest list))
              (list (setq acc (car list) rest (cdr list)))
              (t (setq acc (funcall fn) rest nil)))
        (dolist (elt rest acc)
          (setq acc (if from-end (funcall fn elt acc) (funcall fn acc elt))))))))

(unless (fboundp 'cl-adjoin)
  (defun cl-adjoin (item list &rest keys)
    "Return LIST, or (cons ITEM LIST) if ITEM is not already a member.
Honours `:test' and `:key'."
    (let ((test (or (plist-get keys :test) #'eql))
          (key (plist-get keys :key)))
      (if (emacs-cl-macros--member-test (if key (funcall key item) item)
                                        list test key)
          list
        (cons item list)))))

(unless (fboundp 'cl-set-exclusive-or)
  (defun cl-set-exclusive-or (list1 list2 &rest keys)
    "Return the symmetric difference of LIST1 and LIST2.
Honours `:test' and `:key'."
    (let ((test (or (plist-get keys :test) #'eql))
          (key (plist-get keys :key))
          (result nil))
      (dolist (e list1)
        (unless (emacs-cl-macros--member-test (if key (funcall key e) e)
                                              list2 test key)
          (push e result)))
      (dolist (e list2)
        (unless (emacs-cl-macros--member-test (if key (funcall key e) e)
                                              list1 test key)
          (push e result)))
      (nreverse result))))

(unless (fboundp 'cl-substitute)
  (defun cl-substitute (new old seq &rest keys)
    "Return a copy of SEQ (as a list) with each OLD replaced by NEW.
Honours `:test' and `:key' (the `:count' keyword is not supported)."
    (let ((test (or (plist-get keys :test) #'eql))
          (key (plist-get keys :key)))
      (mapcar (lambda (elt)
                (if (funcall test old (if key (funcall key elt) elt)) new elt))
              (append seq nil)))))

;;;; --- Doc 16 breadth round 10: remaining cl sequence/list fns ---------
;; Membership / assoc / mapping / tree helpers that were void.  Same
;; `:test'/`:key' convention (default `eql') and `unless (fboundp ...)'
;; gating as round 9.  cl-position-if-not / cl-notany / cl-notevery /
;; cl-mapcan / cl-mapcon / cl-nsubstitute build on already-present cl fns.

(unless (fboundp 'cl-member)
  (defun cl-member (item list &rest keys)
    "Return the sublist of LIST starting at the first element matching ITEM.
Honours `:test' and `:key'."
    (let ((test (or (plist-get keys :test) #'eql))
          (key (plist-get keys :key)))
      (while (and list
                  (not (funcall test item
                                (if key (funcall key (car list)) (car list)))))
        (setq list (cdr list)))
      list)))

(unless (fboundp 'cl-assoc)
  (defun cl-assoc (item alist &rest keys)
    "Return the first cons in ALIST whose car matches ITEM.
Honours `:test' and `:key'."
    (let ((test (or (plist-get keys :test) #'eql))
          (key (plist-get keys :key))
          (found nil))
      (while (and alist (not found))
        (let ((pair (car alist)))
          (when (and (consp pair)
                     (funcall test item (if key (funcall key (car pair)) (car pair))))
            (setq found pair)))
        (setq alist (cdr alist)))
      found)))

(unless (fboundp 'cl-rassoc)
  (defun cl-rassoc (item alist &rest keys)
    "Return the first cons in ALIST whose cdr matches ITEM.
Honours `:test' and `:key'."
    (let ((test (or (plist-get keys :test) #'eql))
          (key (plist-get keys :key))
          (found nil))
      (while (and alist (not found))
        (let ((pair (car alist)))
          (when (and (consp pair)
                     (funcall test item (if key (funcall key (cdr pair)) (cdr pair))))
            (setq found pair)))
        (setq alist (cdr alist)))
      found)))

(unless (fboundp 'cl-assoc-if)
  (defun cl-assoc-if (predicate alist &rest keys)
    "Return the first cons in ALIST whose car satisfies PREDICATE.  Honours `:key'."
    (let ((key (plist-get keys :key))
          (found nil))
      (while (and alist (not found))
        (let ((pair (car alist)))
          (when (and (consp pair)
                     (funcall predicate (if key (funcall key (car pair)) (car pair))))
            (setq found pair)))
        (setq alist (cdr alist)))
      found)))

(unless (fboundp 'cl-rassoc-if)
  (defun cl-rassoc-if (predicate alist &rest keys)
    "Return the first cons in ALIST whose cdr satisfies PREDICATE.  Honours `:key'."
    (let ((key (plist-get keys :key))
          (found nil))
      (while (and alist (not found))
        (let ((pair (car alist)))
          (when (and (consp pair)
                     (funcall predicate (if key (funcall key (cdr pair)) (cdr pair))))
            (setq found pair)))
        (setq alist (cdr alist)))
      found)))

(unless (fboundp 'cl-notany)
  (defun cl-notany (predicate &rest seqs)
    "Return t if PREDICATE is false for every tuple drawn from SEQS."
    (not (apply #'cl-some predicate seqs))))

(unless (fboundp 'cl-notevery)
  (defun cl-notevery (predicate &rest seqs)
    "Return t if PREDICATE is false for at least one tuple from SEQS."
    (not (apply #'cl-every predicate seqs))))

(unless (fboundp 'cl-mapcan)
  (defun cl-mapcan (fn &rest seqs)
    "Map FN over SEQS and `nconc' the resulting lists together."
    ;; `cl-mapcar' is provided by cl-extra at runtime (quote, not #', so the
    ;; host byte-compiler does not warn it is "not known to be defined").
    (apply #'nconc (apply 'cl-mapcar fn seqs))))

(unless (fboundp 'cl-maplist)
  (defun cl-maplist (fn &rest lists)
    "Apply FN to successive sublists (tails) of LISTS, returning the results."
    (let ((result nil))
      (while (not (memq nil lists))
        (push (apply fn lists) result)
        (setq lists (mapcar #'cdr lists)))
      (nreverse result))))

(unless (fboundp 'cl-mapcon)
  (defun cl-mapcon (fn &rest lists)
    "Like `cl-maplist' but `nconc' the results together."
    (apply #'nconc (apply #'cl-maplist fn lists))))

(unless (fboundp 'cl-subst)
  (defun cl-subst (new old tree &rest keys)
    "Return a copy of TREE with each subtree matching OLD replaced by NEW.
Honours `:test' and `:key'."
    (let ((test (or (plist-get keys :test) #'eql))
          (key (plist-get keys :key)))
      (cond
       ((funcall test old (if key (funcall key tree) tree)) new)
       ((consp tree)
        (cons (apply #'cl-subst new old (car tree) keys)
              (apply #'cl-subst new old (cdr tree) keys)))
       (t tree)))))

(unless (fboundp 'cl-position-if-not)
  (defun cl-position-if-not (predicate seq &rest keys)
    "Return the index of the first element of SEQ not satisfying PREDICATE."
    (apply #'cl-position-if (lambda (x) (not (funcall predicate x))) seq keys)))

(unless (fboundp 'cl-subsetp)
  (defun cl-subsetp (list1 list2 &rest keys)
    "Return t if every element of LIST1 is a member of LIST2.
Honours `:test' and `:key'."
    (let ((test (or (plist-get keys :test) #'eql))
          (key (plist-get keys :key))
          (ok t))
      (dolist (e list1 ok)
        (unless (emacs-cl-macros--member-test (if key (funcall key e) e)
                                              list2 test key)
          (setq ok nil))))))

(unless (fboundp 'cl-tailp)
  (defun cl-tailp (sublist list)
    "Return t if SUBLIST is `eq' to one of the cons cells (tails) of LIST."
    (while (and (consp list) (not (eq sublist list)))
      (setq list (cdr list)))
    (eq sublist list)))

(unless (fboundp 'cl-delete)
  (defun cl-delete (item seq &rest keys)
    "Return a copy of SEQ (as a list) with elements matching ITEM removed.
Honours `:test' and `:key'.  (This shim does not destroy SEQ.)"
    (let ((test (or (plist-get keys :test) #'eql))
          (key (plist-get keys :key))
          (result nil))
      (dolist (e (append seq nil) (nreverse result))
        (unless (funcall test item (if key (funcall key e) e))
          (push e result))))))

(unless (fboundp 'cl-nsubstitute)
  (defalias 'cl-nsubstitute #'cl-substitute
    "Substitute NEW for OLD in SEQ (non-destructive shim of `cl-substitute')."))

;;;; --- Doc 16 breadth round 17: cl type-dispatch + value/binding helpers -
;; cl-typep + the type-dispatch macros (cl-typecase / cl-etypecase /
;; cl-ecase / cl-check-type) plus the trivial cl-the / cl-locally /
;; cl-values / cl-values-list / cl-gentemp, all void on the NeLisp runtime.
;; Same `unless (fboundp ...)' gating: host Emacs already supplies these
;; (cl-macs / cl-lib) so the forms are inert there and only fire on the
;; runtime.  NOTE: the runtime lacks `sequencep', so the `sequence' branch
;; below tests `(or (listp ..) (arrayp ..))'.  The dispatch macros build only
;; single-level backquote templates (clause lists are spliced, never built
;; with a nested backquote) to stay clear of the runtime backquote quirks
;; documented in Doc 22 / Doc 16 §4.

(unless (fboundp 'cl-typep)
  (defun cl-typep (val type)
    "Return non-nil if VAL is of TYPE.
Supports the common Common-Lisp type specifiers used by `cl-check-type'
and `cl-typecase': atomic type symbols, the `TYPEp'/`TYPE-p' predicate
convention, and the compound forms (integer LO HI), (float ...),
(member ...), (eql X), (satisfies PRED), (or ...), (and ...), (not ...)."
    (cond
     ((null type) nil)
     ((eq type t) t)
     ((symbolp type)
      (cond
       ((memq type '(integer fixnum bignum)) (integerp val))
       ((memq type '(number real)) (numberp val))
       ((eq type 'float) (floatp val))
       ((eq type 'string) (stringp val))
       ((eq type 'character) (characterp val))
       ((eq type 'keyword) (keywordp val))
       ((eq type 'symbol) (symbolp val))
       ((eq type 'cons) (consp val))
       ((eq type 'list) (listp val))
       ((eq type 'null) (null val))
       ((eq type 'atom) (atom val))
       ((eq type 'vector) (vectorp val))
       ;; NOTE: the runtime's `arrayp' is broken (returns nil for both
       ;; vectors and strings, Doc 22 A10), so test the concrete predicates.
       ((eq type 'array) (or (vectorp val) (stringp val)))
       ((eq type 'sequence) (or (listp val) (vectorp val) (stringp val)))
       ((eq type 'hash-table) (hash-table-p val))
       ((eq type 'function) (functionp val))
       ((eq type 'boolean) (and (memq val '(nil t)) t))
       (t
        ;; Fall back to a `TYPEp' or `TYPE-p' predicate when one exists.
        (let* ((name (symbol-name type))
               (p1 (intern-soft (concat name "p")))
               (p2 (intern-soft (concat name "-p"))))
          (cond
           ((and p1 (fboundp p1)) (and (funcall p1 val) t))
           ((and p2 (fboundp p2)) (and (funcall p2 val) t))
           (t nil))))))
     ((consp type)
      (let ((head (car type)))
        (cond
         ((eq head 'integer)
          (and (integerp val)
               (let ((lo (nth 1 type)) (hi (nth 2 type)))
                 (and (or (null lo) (eq lo '*) (>= val lo))
                      (or (null hi) (eq hi '*) (<= val hi))))))
         ((eq head 'float) (floatp val))
         ((eq head 'member) (and (member val (cdr type)) t))
         ((eq head 'eql) (eql val (nth 1 type)))
         ((eq head 'satisfies) (and (funcall (nth 1 type) val) t))
         ((eq head 'or)
          (let ((r nil))
            (dolist (sub (cdr type) r)
              (when (cl-typep val sub) (setq r t)))))
         ((eq head 'and)
          (let ((r t))
            (dolist (sub (cdr type) r)
              (unless (cl-typep val sub) (setq r nil)))))
         ((eq head 'not) (not (cl-typep val (nth 1 type))))
         (t nil))))
     (t nil))))

(unless (fboundp 'cl-the)
  (defmacro cl-the (_type form)
    "Return FORM; the TYPE declaration is advisory and ignored."
    form))

(unless (fboundp 'cl-locally)
  (defmacro cl-locally (&rest body)
    "Evaluate BODY as a `progn' (declarations are ignored)."
    (cons 'progn body)))

(unless (fboundp 'cl-check-type)
  (defmacro cl-check-type (form type &optional _string)
    "Signal `wrong-type-argument' unless FORM satisfies TYPE.  Return nil."
    (let ((temp (make-symbol "--cl-check--")))
      `(let ((,temp ,form))
         (unless (cl-typep ,temp ',type)
           (signal 'wrong-type-argument (list ',type ,temp ',form)))
         nil))))

(unless (fboundp 'cl-typecase)
  (defmacro cl-typecase (expr &rest clauses)
    "Eval EXPR and run the first CLAUSE whose (TYPE . BODY) TYPE matches.
A TYPE of t or `otherwise' is the default clause."
    (let ((temp (make-symbol "--cl-typecase--")))
      ;; Build the cond clauses with `cons'/`list' (no inner backquote) so the
      ;; template stays a single-level backquote.
      `(let ((,temp ,expr))
         (cond
          ,@(mapcar
             (lambda (clause)
               (let ((type (car clause)) (body (cdr clause)))
                 (if (memq type '(t otherwise))
                     (cons t body)
                   (cons (list 'cl-typep temp (list 'quote type)) body))))
             clauses))))))

(unless (fboundp 'cl-etypecase)
  (defmacro cl-etypecase (expr &rest clauses)
    "Like `cl-typecase' but signal an error if no clause matches."
    `(cl-typecase ,expr
       ,@clauses
       (t (error "cl-etypecase failed")))))

(unless (fboundp 'cl-ecase)
  (defmacro cl-ecase (expr &rest clauses)
    "Like `cl-case' but signal an error if no key matches."
    `(cl-case ,expr
       ,@clauses
       (t (error "cl-ecase failed")))))

(unless (fboundp 'cl-values)
  (defun cl-values (&rest values)
    "Return VALUES as a list (Emacs models multiple values as a list)."
    values))

(unless (fboundp 'cl-values-list)
  (defun cl-values-list (list)
    "Return LIST (the multiple values carried by a list)."
    list))

(defvar emacs-cl-macros--gentemp-counter 0
  "Counter backing the fresh names produced by `cl-gentemp'.")

(unless (fboundp 'cl-gentemp)
  (defun cl-gentemp (&optional prefix)
    "Return a fresh interned symbol named PREFIX followed by a number."
    (let ((pfx (or prefix "T")) sym)
      (while (intern-soft
              (setq sym (format "%s%d" pfx
                                (setq emacs-cl-macros--gentemp-counter
                                      (1+ emacs-cl-macros--gentemp-counter))))))
      (intern sym))))

;;;; --- Doc 16 breadth round 18: cl binding macros -----------------------
;; cl-destructuring-bind / cl-multiple-value-bind / cl-multiple-value-setq,
;; all void on the runtime.  cl-destructuring-bind reuses the existing
;; `emacs-cl-macros--split-arglist' / `--key-bindings' helpers (the same
;; ones backing `cl-defun'); the multiple-value macros are thin layers on
;; top.  Nested destructuring patterns are not supported (matches the
;; cl-defun shim's scope).  Hygienic temporaries via `make-symbol'.  Same
;; `unless (fboundp ...)' gating: host Emacs keeps its real cl-lib versions.

(unless (fboundp 'cl-destructuring-bind)
  (defmacro cl-destructuring-bind (arglist expr &rest body)
    "Bind the variables in ARGLIST to successive elements of the list EXPR.
Supports &optional (with defaults), &rest/&body, &key (with defaults) and
&aux.  Nested destructuring patterns are not supported by this shim."
    (let* ((parts (emacs-cl-macros--split-arglist arglist))
           (positional (nth 0 parts))
           (optionals (nth 1 parts))
           (restsym (nth 2 parts))
           (keys (nth 3 parts))
           (vsym (make-symbol "--cl-ds--"))
           (restvar (or restsym
                        (and keys (make-symbol "--cl-ds-rest--"))))
           (idx 0)
           (bindings (list (list vsym expr))))
      ;; positionals: (POS (nth IDX V))
      (dolist (p positional)
        (setq bindings (cons (list p (list 'nth idx vsym)) bindings))
        (setq idx (1+ idx)))
      ;; optionals: token is VAR or (VAR DEFAULT); present iff the cons exists
      (dolist (o optionals)
        (let ((osym (if (consp o) (car o) o))
              (odef (if (consp o) (car (cdr o)) nil)))
          (setq bindings
                (cons (list osym
                            (list 'if (list 'nthcdr idx vsym)
                                  (list 'nth idx vsym)
                                  odef))
                      bindings))
          (setq idx (1+ idx))))
      ;; &rest / the list scanned for &key values
      (when restvar
        (setq bindings (cons (list restvar (list 'nthcdr idx vsym)) bindings)))
      ;; &key: (SYM (or (cadr (memq :SYM RESTVAR)) DEFAULT))
      (when keys
        (dolist (kb (emacs-cl-macros--key-bindings keys restvar))
          (setq bindings (cons kb bindings))))
      (cons 'let* (cons (nreverse bindings) body)))))

(unless (fboundp 'cl-multiple-value-bind)
  (defmacro cl-multiple-value-bind (vars form &rest body)
    "Bind VARS to successive elements of the list produced by FORM.
Extra values are ignored (Emacs models multiple values as a list)."
    (cons 'cl-destructuring-bind
          (cons (append vars (list '&rest (make-symbol "--cl-mvb-rest--")))
                (cons form body)))))

(unless (fboundp 'cl-multiple-value-setq)
  (defmacro cl-multiple-value-setq (vars form)
    "Set VARS to successive elements of the list produced by FORM.
Return the primary (first) value."
    (let ((tmp (make-symbol "--cl-mvs--"))
          (sets nil)
          (idx 0))
      (dolist (v vars)
        (setq sets (cons (list 'setq v (list 'nth idx tmp)) sets))
        (setq idx (1+ idx)))
      (cons 'let
            (cons (list (list tmp form))
                  (append (nreverse sets) (list (list 'car tmp))))))))

;;;; --- Doc 16 breadth round 19: cl place (setf) macros ------------------
;; cl-psetq / cl-psetf / cl-rotatef / cl-shiftf / cl-callf / cl-callf2, all
;; void on the runtime.  They expand to `setq'/`setf'; the runtime's `setf'
;; already handles symbol / car / nth / gethash places (Doc 16 round 8
;; cl-simple-setter registrations), so these macros are place-agnostic.
;; Parallel forms capture every value into hygienic temporaries first.
;; Same `unless (fboundp ...)' gating: host cl-lib is untouched.

(defun emacs-cl-macros--pairs (args)
  "Split a flat (P1 V1 P2 V2 ...) ARGS list into a list of (PLACE . VALUE)."
  (let ((out nil) (cur args))
    (while cur
      (setq out (cons (cons (car cur) (car (cdr cur))) out))
      (setq cur (cdr (cdr cur))))
    (nreverse out)))

;; NOTE: temporaries below carry an index in their name.  The runtime
;; resolves `make-symbol' (uninterned) symbols by NAME, so several temps
;; sharing one name collide inside a single `let' (Doc 22 A11); unique
;; names sidestep that and remain correct if the runtime later honours
;; uninterned identity.
(unless (fboundp 'cl-psetq)
  (defmacro cl-psetq (&rest args)
    "Like `setq' but evaluate all values before any assignment.  Return nil."
    (let ((binds nil) (sets nil) (i 0))
      (dolist (pr (emacs-cl-macros--pairs args))
        (let ((tmp (make-symbol (format "--cl-psq-%d--" i))))
          (setq binds (cons (list tmp (cdr pr)) binds))
          (setq sets (cons (list 'setq (car pr) tmp) sets))
          (setq i (1+ i))))
      (cons 'let (cons (nreverse binds) (append (nreverse sets) (list nil)))))))

(unless (fboundp 'cl-psetf)
  (defmacro cl-psetf (&rest args)
    "Like `setf' but evaluate all values before any assignment.  Return nil."
    (let ((binds nil) (sets nil) (i 0))
      (dolist (pr (emacs-cl-macros--pairs args))
        (let ((tmp (make-symbol (format "--cl-psf-%d--" i))))
          (setq binds (cons (list tmp (cdr pr)) binds))
          (setq sets (cons (list 'setf (car pr) tmp) sets))
          (setq i (1+ i))))
      (cons 'let (cons (nreverse binds) (append (nreverse sets) (list nil)))))))

(unless (fboundp 'cl-rotatef)
  (defmacro cl-rotatef (&rest places)
    "Rotate the values of PLACES: each gets the next one's value, the last
gets the first one's original value.  Return nil."
    (if (null (cdr places))
        nil
      (let ((temps nil) (binds nil) (sets nil) (i 0))
        (dolist (p places)
          (let ((tmp (make-symbol (format "--cl-rot-%d--" i))))
            (setq temps (cons tmp temps))
            (setq binds (cons (list tmp p) binds))
            (setq i (1+ i))))
        (setq temps (nreverse temps))
        (setq binds (nreverse binds))
        (let ((ps places)
              (ts (append (cdr temps) (list (car temps)))))
          (while ps
            (setq sets (cons (list 'setf (car ps) (car ts)) sets))
            (setq ps (cdr ps))
            (setq ts (cdr ts))))
        (cons 'let (cons binds (append (nreverse sets) (list nil))))))))

(unless (fboundp 'cl-shiftf)
  (defmacro cl-shiftf (&rest args)
    "Shift values leftward through PLACES; the final ARG is the new value
for the last place.  Return the original value of the first place."
    (let* ((places (butlast args))
           (newval (car (last args)))
           (old (make-symbol "--cl-shf--"))
           (sets nil))
      (when (null places)
        (error "cl-shiftf needs at least one place and a value"))
      (let ((ps places))
        (while (cdr ps)
          (setq sets (cons (list 'setf (car ps) (car (cdr ps))) sets))
          (setq ps (cdr ps)))
        (setq sets (cons (list 'setf (car ps) newval) sets)))
      (list 'let (list (list old (car places)))
            (cons 'progn (append (nreverse sets) (list old)))))))

(unless (fboundp 'cl-callf)
  (defmacro cl-callf (func place &rest args)
    "Set PLACE to (FUNC PLACE ARGS...).
FUNC is spliced literally into the call (an unquoted function name or a
lambda form), matching `cl-callf'."
    (let ((call (cons func (cons place args))))
      (if (symbolp place) (list 'setq place call) (list 'setf place call)))))

(unless (fboundp 'cl-callf2)
  (defmacro cl-callf2 (func arg1 place &rest args)
    "Set PLACE to (FUNC ARG1 PLACE ARGS...).
FUNC is spliced literally into the call, matching `cl-callf2'."
    (let ((call (cons func (cons arg1 (cons place args)))))
      (if (symbolp place) (list 'setq place call) (list 'setf place call)))))

;;;; --- Doc 16 breadth round 21: cl iteration macros (cl-do / cl-do*) ----
;; cl-do / cl-do*, both void on the runtime.  Each SPEC is (VAR INIT STEP),
;; (VAR INIT) or VAR; ENDLIST is (END-TEST RESULT...).  cl-do steps in
;; parallel (let + cl-psetq, round 19); cl-do* steps sequentially (let* +
;; setq).  Both wrap in `cl-block nil' so `cl-return' works.  Gated
;; `unless (fboundp ...)' — these live here (feature emacs-cl-macros), not
;; in cl-macs.el, so the host autoload target is unaffected.

(defun emacs-cl-macros--do-parse (specs)
  "Parse cl-do SPECS into (BINDINGS . FLAT-STEPS).
BINDINGS = list of (VAR INIT); FLAT-STEPS = (VAR1 STEP1 VAR2 STEP2 ...)
for the specs that carry a STEP form."
  (let ((binds nil) (steps nil))
    (dolist (s specs)
      (let* ((var (if (consp s) (car s) s))
             (init (if (and (consp s) (cdr s)) (car (cdr s)) nil))
             (has-step (and (consp s) (cdr s) (cdr (cdr s))))
             (step (if has-step (car (cdr (cdr s))) nil)))
        (setq binds (cons (list var init) binds))
        (when has-step
          (setq steps (cons step (cons var steps))))))
    (cons (nreverse binds) (nreverse steps))))

(unless (fboundp 'cl-do)
  (defmacro cl-do (specs endlist &rest body)
    "Iterate with parallel stepping (Common Lisp `do').
SPECS are (VAR INIT STEP); ENDLIST is (END-TEST RESULT...)."
    (let* ((parsed (emacs-cl-macros--do-parse specs))
           (binds (car parsed))
           (steps (cdr parsed))
           (end (car endlist))
           (result (cdr endlist))
           (loop (list 'while (list 'not end))))
      (setq loop (append loop body))
      (when steps (setq loop (append loop (list (cons 'cl-psetq steps)))))
      (list 'cl-block nil
            (append (list 'let binds loop) result)))))

(unless (fboundp 'cl-do*)
  (defmacro cl-do* (specs endlist &rest body)
    "Iterate with sequential stepping (Common Lisp `do*').
Like `cl-do' but bindings init and step left-to-right (let* + setq)."
    (let* ((parsed (emacs-cl-macros--do-parse specs))
           (binds (car parsed))
           (steps (cdr parsed))
           (end (car endlist))
           (result (cdr endlist))
           (loop (list 'while (list 'not end))))
      (setq loop (append loop body))
      (when steps (setq loop (append loop (list (cons 'setq steps)))))
      (list 'cl-block nil
            (append (list 'let* binds loop) result)))))

;;;; --- Doc 16 breadth round 22: cl-tagbody / cl-prog family ------------
;; cl-tagbody / cl-prog / cl-prog* / cl-prog1 / cl-prog2, all void on the
;; runtime.  cl-tagbody is the deep one: it is compiled to a `catch'/`while'
;; state machine driven by a program-counter holding the current label, and
;; `(go LABEL)' is rewritten by a code walk into "(setq PC 'LABEL); throw to
;; restart".  Segments fall through by advancing PC to the next label.
;; Hygienic temporaries use `emacs-cl-macros--gensym' (interned + unique),
;; side-stepping the Doc 22 A11 `make-symbol' name-collision.  Gated;
;; these live in emacs-cl-macros, not cl-macs.el, so host autoloads are safe.

(defvar emacs-cl-macros--gensym-counter 0
  "Counter backing `emacs-cl-macros--gensym'.")

(defun emacs-cl-macros--gensym (prefix)
  "Return a fresh interned symbol named PREFIX followed by a number.
Used in place of `cl-gensym' (which the host byte-compiler does not know
here) for hygienic temporaries; interned + unique names side-step the
Doc 22 A11 `make-symbol' name-collision."
  (intern (format "%s%d" prefix
                  (setq emacs-cl-macros--gensym-counter
                        (1+ emacs-cl-macros--gensym-counter)))))

(defun emacs-cl-macros--tagbody-replace-go (form pcsym gotag)
  "Rewrite every (go TAG) in FORM into a jump that sets PCSYM and throws GOTAG.
Quoted data and nested `cl-tagbody' forms are left untouched."
  (cond
   ((not (consp form)) form)
   ((eq (car form) 'quote) form)
   ((and (eq (car form) 'go) (consp (cdr form)))
    (list 'progn
          (list 'setq pcsym (list 'quote (car (cdr form))))
          (list 'throw (list 'quote gotag) nil)))
   ((eq (car form) 'cl-tagbody) form)
   (t (cons (emacs-cl-macros--tagbody-replace-go (car form) pcsym gotag)
            (emacs-cl-macros--tagbody-replace-go (cdr form) pcsym gotag)))))

(unless (fboundp 'cl-tagbody)
  (defmacro cl-tagbody (&rest body)
    "Common Lisp `tagbody': execute statements in order; atoms are labels
reachable with (go LABEL).  Always returns nil."
    (let ((pcsym (emacs-cl-macros--gensym "--cl-tagbody-pc--"))
          (gotag (emacs-cl-macros--gensym "--cl-tagbody-go--"))
          (contsym (emacs-cl-macros--gensym "--cl-tagbody-cont--"))
          (start (emacs-cl-macros--gensym "--cl-tagbody-start--"))
          (endtag (emacs-cl-macros--gensym "--cl-tagbody-end--"))
          (segments nil)
          (curtag nil)
          (curforms nil))
      (setq curtag start)
      (dolist (item body)
        (if (consp item)
            (setq curforms
                  (cons (emacs-cl-macros--tagbody-replace-go item pcsym gotag)
                        curforms))
          (setq segments (cons (cons curtag (nreverse curforms)) segments))
          (setq curtag item)
          (setq curforms nil)))
      (setq segments
            (nreverse (cons (cons curtag (nreverse curforms)) segments)))
      (let ((clauses nil) (segs segments))
        (while segs
          (let* ((seg (car segs))
                 (tag (car seg))
                 (forms (cdr seg))
                 (next (if (cdr segs) (car (car (cdr segs))) endtag)))
            (setq clauses
                  (cons (append (list (list 'eql pcsym (list 'quote tag)))
                                forms
                                (list (list 'setq pcsym (list 'quote next))))
                        clauses)))
          (setq segs (cdr segs)))
        (setq clauses
              (append (nreverse clauses)
                      (list (list t (list 'setq contsym nil)))))
        (list 'let (list (list pcsym (list 'quote start)) (list contsym t))
              (list 'while contsym
                    (list 'catch (list 'quote gotag) (cons 'cond clauses)))
              nil)))))

(unless (fboundp 'cl-prog)
  (defmacro cl-prog (bindings &rest body)
    "Common Lisp `prog': `let' + `cl-tagbody', wrapped in `cl-block nil'."
    (list 'cl-block nil (list 'let bindings (cons 'cl-tagbody body)))))

(unless (fboundp 'cl-prog*)
  (defmacro cl-prog* (bindings &rest body)
    "Like `cl-prog' but binds with `let*' (sequential)."
    (list 'cl-block nil (list 'let* bindings (cons 'cl-tagbody body)))))

(unless (fboundp 'cl-prog1)
  (defmacro cl-prog1 (first &rest body)
    "Evaluate FIRST then BODY; return the value of FIRST."
    (let ((tmp (emacs-cl-macros--gensym "--cl-prog1--")))
      (list 'let (list (list tmp first)) (cons 'progn body) tmp))))

(unless (fboundp 'cl-prog2)
  (defmacro cl-prog2 (form1 form2 &rest body)
    "Evaluate FORM1, FORM2 and BODY; return the value of FORM2."
    (list 'progn form1 (cons 'cl-prog1 (cons form2 body)))))

(provide 'emacs-cl-macros)

;;; emacs-cl-macros.el ends here
