;;; cl-lib.el --- nelisp-emacs intercepting cl-lib shim  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track O (2026-05-04) — Layer 2 cl-lib intercept shim.
;;
;; Why this exists: the upstream `vendor/emacs-lisp/emacs-lisp/cl-lib.el'
;; uses several reader features (= `\(' string escape on docstring
;; arglist hints, `,' outside backquote, etc.) that nelisp's reader
;; rejects.  Under host Emacs `cl-lib' is preloaded so `(require
;; 'cl-lib)' is a no-op and our shim never executes.  Under nelisp the
;; shim wins because `src/' precedes `vendor/' on the load-path.
;;
;; We deliberately do NOT mirror every cl-lib symbol — only the
;; subset our Layer-2 substrate touches (= the `MISSING' list from
;; the audit script run as part of Track O).  Most of cl-lib is
;; already covered by `emacs-cl-macros.el' (cl-defun, cl-loop,
;; cl-defstruct, …); this file adds the remaining 3-4 helpers and
;; declares the `cl-lib' feature.
;;
;; If a future substrate change pulls in another cl-lib symbol that
;; isn't here, the right fix is to either (a) add a polyfill here,
;; or (b) port the symbol into `emacs-cl-macros.el'.

;;; Code:

(defun cl-lib--define-p (symbol)
  "Return non-nil when SYMBOL should be supplied by this shim."
  (or (not (fboundp symbol))
      (and (fboundp 'autoloadp)
           (autoloadp (symbol-function symbol)))))

;; Pull in the existing prefixed subset (cl-loop / cl-defun /
;; cl-defstruct / cl-letf / cl-flet / cl-block / cl-some / cl-every /
;; cl-position / cl-find / cl-remove-if{,-not} / cl-delete-* /
;; cl-union / cl-intersection / cl-sort / cl-case / cl-pushnew / etc.)
(require 'emacs-cl-macros)

;;;; --- helpers not in emacs-cl-macros --------------------------------

(unless (fboundp 'cl-copy-list)
  (defun cl-copy-list (list)
    "Return a shallow copy of LIST."
    (let (out)
      (while list
        (push (car list) out)
        (setq list (cdr list)))
      (nreverse out))))

(unless (fboundp 'cl-coerce)
  (defun cl-coerce (object type)
    "Coerce OBJECT to TYPE for the sequence shapes used by the shim."
    (cond
     ((eq type 'list)
      (cond
       ((listp object) object)
       ((vectorp object) (append object nil))
       ((stringp object)
        (let ((i 0) (n (length object)) out)
          (while (< i n)
            (push (aref object i) out)
            (setq i (1+ i)))
          (nreverse out)))
       (t (signal 'wrong-type-argument (list 'sequencep object)))))
     ((eq type 'vector)
      (cond
       ((vectorp object) object)
       ((listp object) (apply #'vector object))
       ((stringp object) (apply #'vector (cl-coerce object 'list)))
       (t (signal 'wrong-type-argument (list 'sequencep object)))))
     ((eq type 'string)
      (cond
       ((stringp object) object)
       ((listp object) (apply #'string object))
       ((vectorp object) (apply #'string (append object nil)))
       (t (signal 'wrong-type-argument (list 'sequencep object)))))
     (t (signal 'wrong-type-argument (list 'type-specifier-p type))))))

(unless (fboundp 'cl-find-class)
  (defun cl-find-class (_symbol &optional _errorp _environment)
    "Minimal class lookup stub for code paths that probe EIEIO classes."
    nil))

(unless (fboundp 'cl-assert)
  (defmacro cl-assert (form &optional _show-args string &rest args)
    "Signal an error unless FORM evaluates non-nil.
This minimal shim covers load-time CL assertions in vendored libraries."
    (list 'unless form
          (cons 'error
                (cons (or string "Assertion failed: %S")
                      (if string args (list (list 'quote form))))))))

(unless (fboundp 'cl-subseq)
  (defun cl-subseq (sequence start &optional end)
    "Return the subsequence of SEQUENCE from START to END.
If END is nil, copy SEQUENCE from START to end.  Mirrors the
classic Common Lisp shape used by the Layer-2 substrate (=
`emacs-window.el' tree-rebuild paths)."
    (cond
     ((listp sequence)
      (let* ((rest (nthcdr start sequence))
             (len (if end (- end start) (length rest))))
        (let (out (i 0))
          (while (and rest (< i len))
            (push (car rest) out)
            (setq rest (cdr rest))
            (setq i (1+ i)))
          (nreverse out))))
     ((stringp sequence)
      (substring sequence start end))
     ((vectorp sequence)
      (let* ((len (length sequence))
             (e (or end len))
             (out (make-vector (- e start) nil)))
        (let ((i start) (j 0))
          (while (< i e)
            (aset out j (aref sequence i))
            (setq i (1+ i) j (1+ j))))
        out))
     (t (signal 'wrong-type-argument (list 'sequencep sequence))))))

(unless (fboundp 'cl-remove)
  (defun cl-remove (item sequence)
    "Return SEQUENCE with all occurrences of ITEM removed (`equal' test).
Always returns a fresh list (= callers in `emacs-window.el' rely on
this for sibling-list immutability)."
    (cond
     ((listp sequence)
      (let (out)
        (dolist (x sequence)
          (unless (equal item x) (push x out)))
        (nreverse out)))
     ((stringp sequence)
      (apply #'string
             (cl-loop for c across sequence
                      unless (equal item c) collect c)))
     ((vectorp sequence)
      (apply #'vector
             (cl-loop for x across sequence
                      unless (equal item x) collect x)))
     (t (signal 'wrong-type-argument (list 'sequencep sequence))))))

(unless (fboundp 'cl-find-if)
  (defun cl-find-if (predicate sequence)
    "Return the first element of SEQUENCE for which PREDICATE is non-nil."
    (catch 'found
      (cond
       ((listp sequence)
        (dolist (x sequence)
          (when (funcall predicate x) (throw 'found x))))
       ((stringp sequence)
        (let ((i 0) (n (length sequence)))
          (while (< i n)
            (let ((c (aref sequence i)))
              (when (funcall predicate c) (throw 'found c)))
            (setq i (1+ i)))))
       ((vectorp sequence)
        (let ((i 0) (n (length sequence)))
          (while (< i n)
            (let ((x (aref sequence i)))
              (when (funcall predicate x) (throw 'found x)))
            (setq i (1+ i))))))
      nil)))

(unless (fboundp 'cl-find-if-not)
  (defun cl-find-if-not (predicate sequence)
    "Return the first element of SEQUENCE for which PREDICATE is nil."
    (cl-find-if (lambda (x) (not (funcall predicate x))) sequence)))

(unless (fboundp 'cl-member-if)
  (defun cl-member-if (predicate list &rest _keys)
    "Return the first tail of LIST whose car satisfies PREDICATE."
    (let ((cur list)
          (found nil))
      (while (and cur (not found))
        (if (funcall predicate (car cur))
            (setq found cur)
          (setq cur (cdr cur))))
      found)))

(unless (fboundp 'cl-member-if-not)
  (defun cl-member-if-not (predicate list &rest _keys)
    "Return the first tail of LIST whose car does not satisfy PREDICATE."
    (cl-member-if (lambda (x) (not (funcall predicate x))) list)))

;;;; --- generalized place setter (setf) ---------------------------------
;;
;; nelisp driver では vendor/emacs-lisp/emacs-lisp/gv.el が reader 不
;; 整合で読めないため、setf を最小限ここで polyfill する。host driver
;; では gv.el の `setf' を使う。autoload を local stub で上書きしない
;; よう、この polyfill は standalone 専用にする。

(unless (boundp 'emacs-version)
  (defmacro setf (&rest pairs)
    "Minimal setf — handles common places.
Supported PLACE forms:
  symbol             → setq
  (car X)            → setcar
  (cdr X)            → setcdr
  (nth N L)          → setcar of nthcdr
  (aref V I)         → aset
  (gethash K H)      → puthash
  registered simple setter → calls the setter with PLACE args + value
  (struct-slot OBJ)  → uses property `cl-struct-setter` on slot symbol

For unrecognised places, signals an error at expansion time."
    (when (= (mod (length pairs) 2) 1)
      (error "setf: odd number of arguments"))
    (let ((forms nil))
      (while pairs
        (let ((place (pop pairs))
              (value (pop pairs)))
          (push
           (cond
            ((symbolp place) (list 'setq place value))
            ((not (consp place))
             (error "setf: invalid place: %S" place))
            (t
             (let ((fn (car place))
                   (args (cdr place)))
               (cond
                ((eq fn 'car)     (list 'setcar (car args) value))
                ((eq fn 'cdr)     (list 'setcdr (car args) value))
                ;; Two-level c[ad][ad]r accessors.  Without these, a place
                ;; like `(cddr X)' fell through to the symbol fallback, which
                ;; emitted a call to a VOID `cddr--setter' and aborted.
                ;; `org-element-set-contents' uses `(setf (cddr node) ...)',
                ;; so this was the structural blocker that made
                ;; `org-element-parse-buffer' return nil.
                ((eq fn 'caar) (list 'setcar (list 'car (car args)) value))
                ((eq fn 'cadr) (list 'setcar (list 'cdr (car args)) value))
                ((eq fn 'cdar) (list 'setcdr (list 'car (car args)) value))
                ((eq fn 'cddr) (list 'setcdr (list 'cdr (car args)) value))
                ;; (setf (nthcdr N L) V) -> setcdr of the (N-1)th cdr.
                ;; Assumes N >= 1 (the common case; N = 0 would replace the
                ;; whole list, which is not an in-place mutation).
                ((eq fn 'nthcdr)
                 (list 'setcdr
                       (list 'nthcdr (list '1- (car args)) (cadr args))
                       value))
                ((eq fn 'aref)    (list 'aset (car args) (cadr args) value))
                ((eq fn 'elt)
                 ;; (setf (elt SEQ N) V): `elt' works on lists and arrays, so
                 ;; dispatch at runtime — setcar of nthcdr for a list, aset for
                 ;; an array.  (A void `elt--setter' fallback was an uncatchable
                 ;; abort on the bare reader.)
                 (let ((seqsym (make-symbol "seq")))
                   (list 'let (list (list seqsym (car args)))
                         (list 'if (list 'listp seqsym)
                               (list 'setcar (list 'nthcdr (cadr args) seqsym) value)
                               (list 'aset seqsym (cadr args) value)))))
                ((eq fn 'gethash) (list 'puthash (car args) value (cadr args)))
                ((eq fn 'nth)
                 (list 'setcar
                       (list 'nthcdr (car args) (cadr args))
                       value))
                ((eq fn 'plist-get)
                 (list 'plist-put (car args) (cadr args) value))
                ((eq fn 'alist-get)
                 ;; (setf (alist-get K A) V): assq-update the existing cell or
                 ;; prepend (K . V), recursing on the alist place A via `setf'.
                 ;; cl-generic's dispatch tables rely on this.
                 (let ((cell (make-symbol "cell")))
                   (list 'let (list (list cell (list 'assq (car args) (cadr args))))
                         (list 'if cell (list 'setcdr cell value)
                               (list 'setf (cadr args)
                                     (list 'cons (list 'cons (car args) value)
                                           (cadr args)))))))
                ((and (symbolp fn)
                      (boundp 'nelisp-cl-macros--accessor-info)
                      (assq fn nelisp-cl-macros--accessor-info))
                 ;; cl-defstruct slot accessor place.  The standalone
                 ;; cl-defstruct (stdlib prelude) records accessors in
                 ;; `nelisp-cl-macros--accessor-info' (it does NOT set the
                 ;; `cl-struct-setter' property the generic fallback below
                 ;; looks for), so consult it directly -> `nelisp--record-set'.
                 ;; cl-generic's `(setf (cl--generic-dispatches g) ...)' needs this.
                 (list 'nelisp--record-set (car args)
                       (cdr (assq fn nelisp-cl-macros--accessor-info))
                       value))
                ((and (symbolp fn) (fboundp fn)
                      (eq (car-safe (symbol-function fn)) 'macro))
                 ;; A generalized place defined as a MACRO (e.g. cl-generic's
                 ;; `(cl--generic NAME)' = `(get NAME ...)').  Expand the place
                 ;; and re-dispatch through `setf'.  Without this, cl-generic's
                 ;; `(setf (cl--generic name) ...)' fell through to the symbol
                 ;; fallback and built a `(funcall 'cl--generic--setter ...)'
                 ;; call to a non-existent setter.
                 (list 'setf (macroexpand-1 place) value))
                ((symbolp fn)
                 (let ((simple-setter (get fn 'cl-simple-setter)))
                   (if simple-setter
                       (cons 'funcall
                             (cons (list 'quote simple-setter)
                                   (append args (list value))))
                     (list 'funcall
                           (list 'or
                                 (list 'get (list 'quote fn)
                                       (list 'quote 'cl-struct-setter))
                                 (list 'quote
                                       (intern (concat (symbol-name fn)
                                                       "--setter"))))
                           (car args) value))))
                (t (error "setf: unsupported place form: %S" place))))))
           forms)))
      (cons 'progn (nreverse forms)))))

;;;; --- Doc 16 breadth round 8: extra setf places (standalone) ----------
;; The standalone reader's `setf' (nelisp's stdlib prelude) consults the
;; `cl-simple-setter' property: a place (FN ARGS...) expands to
;; (funcall SETTER ARGS... VALUE).  Register setters for common in-place
;; places the prelude omits.  `gethash' needs an argument reorder (puthash
;; is KEY VALUE TABLE, not KEY TABLE VALUE) so it routes through a wrapper.
;; Only in-place mutators are registered -- `plist-get' is deliberately
;; left out because `plist-put' may return a fresh list without updating
;; the place, which a simple setter cannot reassign.
;; Host Emacs uses gv.el and ignores `cl-simple-setter', so this is gated
;; to the standalone runtime (emacs-version is a sentinel there, not a
;; version string).

(when (not (stringp (and (boundp 'emacs-version) emacs-version)))
  (unless (fboundp 'nelisp-place--set-gethash)
    (defun nelisp-place--set-gethash (key table value)
      "`setf' setter for (gethash KEY TABLE); reorders args for `puthash'."
      (puthash key value table)
      value))
  (put 'gethash 'cl-simple-setter 'nelisp-place--set-gethash)
  (put 'get 'cl-simple-setter 'put)
  (put 'symbol-value 'cl-simple-setter 'set)
  (put 'symbol-function 'cl-simple-setter 'fset)
  (put 'symbol-plist 'cl-simple-setter 'setplist)
  ;; `(setf (cl--find-class NAME) CLASS)' -> `(cl--set-find-class NAME CLASS)'
  ;; (= `(put NAME 'cl--class CLASS)').  cl-preloaded / oclosure / cl-defstruct
  ;; register class objects this way.  `cl--set-find-class' is baked in the
  ;; stdlib prelude; this `put' runs here (a loaded file) because the same `put'
  ;; in the AOT-baked prelude does not persist into the boot image.
  (when (fboundp 'cl--set-find-class)
    (put 'cl--find-class 'cl-simple-setter 'cl--set-find-class)))

;;;; --- list / alist polyfills ------------------------------------------

(unless (fboundp 'assoc-delete-all)
  (defun assoc-delete-all (key alist &optional test)
    "Return ALIST with all entries whose car matches KEY removed.
TEST defaults to `equal'."
    (unless test (setq test (function equal)))
    (let (out)
      (dolist (cell alist)
        (unless (and (consp cell) (funcall test (car cell) key))
          (push cell out)))
      (nreverse out))))

(unless (fboundp 'plist-put)
  (defun plist-put (plist prop val)
    "Change PLIST so PROP maps to VAL.  In-place when possible."
    (let ((cur plist))
      (catch 'done
        (while cur
          (when (eq (car cur) prop)
            (setcar (cdr cur) val)
            (throw 'done plist))
          (setq cur (cddr cur)))
        (append plist (list prop val))))))

;;;; --- error / control-flow macros -------------------------------------

(unless (fboundp 'ignore-errors)
  (defmacro ignore-errors (&rest body)
    "Execute BODY; on error return nil instead of raising."
    (list 'condition-case nil
          (cons 'progn body)
          (list 'error nil))))

(unless (fboundp 'with-no-warnings)
  (defmacro with-no-warnings (&rest body)
    "Like `progn', no compiler-warning suppression in this stub."
    (cons 'progn body)))

(unless (fboundp 'when-let)
  (defmacro when-let (spec &rest body)
    "Evaluate SPEC bindings; if all values are non-nil, execute BODY.
SPEC is either ((VAR EXPR) ...) or (VAR EXPR) for a single binding."
    (let ((bindings (if (and (consp spec)
                             (symbolp (car spec))
                             (not (consp (car-safe (cdr spec)))))
                        (list spec)
                      spec))
          (vars nil)
          (let-bindings nil))
      (dolist (b bindings)
        (push (car b) vars)
        (push b let-bindings))
      (list 'let* (nreverse let-bindings)
            (list 'when (cons 'and (nreverse vars))
                  (cons 'progn body))))))

(unless (fboundp 'if-let)
  (defmacro if-let (spec then &rest else)
    "Evaluate SPEC bindings; on all-non-nil run THEN, else ELSE."
    (let ((bindings (if (and (consp spec)
                             (symbolp (car spec))
                             (not (consp (car-safe (cdr spec)))))
                        (list spec)
                      spec))
          (vars nil)
          (let-bindings nil))
      (dolist (b bindings)
        (push (car b) vars)
        (push b let-bindings))
      (list 'let* (nreverse let-bindings)
            (list 'if (cons 'and (nreverse vars))
                  then
                  (cons 'progn else))))))

(unless (fboundp 'when-let*) (defalias 'when-let* 'when-let))
(unless (fboundp 'if-let*)   (defalias 'if-let* 'if-let))

;;;; --- introspection -------------------------------------------------

(defconst cl-lib-version "1.0-nemacs-shim"
  "Version of the nelisp-emacs cl-lib shim (= NOT upstream cl-lib).")

(provide 'cl-lib)

;;; cl-lib.el ends here
