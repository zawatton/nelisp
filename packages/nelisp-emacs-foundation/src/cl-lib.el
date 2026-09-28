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

;;;; --- defer to a genuine cl-lib when the host has one ---------------

;; The commentary above assumed host Emacs always has `cl-lib' preloaded,
;; so `(require 'cl-lib)' would be a no-op and this shim would only ever
;; run under nelisp.  That is false for `emacs -Q --batch', which starts
;; with `cl-lib' unloaded: `require' then finds THIS file, the genuine
;; library never loads, and the deliberately minimal `cl-loop' in
;; `emacs-cl-macros.el' wins.  That one returns nil for every pattern it
;; does not recognise, so a complete implementation gets replaced by a
;; partial one with no warning at all.
;;
;; So when a genuine CL implementation exists OUTSIDE this repository,
;; load it and let it win.  Searching only outside the repo is what keeps
;; nelisp on the shim: the vendored copy this file exists to avoid lives
;; under the repo and stays invisible here.  `cl-macs' is the probe
;; because only a real Emacs lisp tree carries it -- a sibling scaffolded
;; package would offer another copy of this shim instead.  Loading
;; `cl-macs'/`cl-extra'/`cl-seq' eagerly matters too: left as autoloads
;; they read as "not defined" to the shims' own guards, which would
;; clobber them anyway.  Every definition below is `fboundp'-gated, so
;; they all turn into no-ops once the genuine library is in.

(defvar nelisp-emacs-cl-lib-force-shim
  (and (getenv "NELISP_EMACS_CL_LIB_FORCE_SHIM") t)
  "Non-nil to keep this shim even when a genuine `cl-lib' is available.
Set this for tests that mean to exercise the shim on a host that has
the real library, such as the package load-path smoke.")

(defvar nelisp-emacs-cl-lib-genuine nil
  "Path of the genuine `cl-lib' this shim deferred to, or nil under nelisp.")

(let* ((this (or load-file-name buffer-file-name))
       (dir (and this (file-name-directory this)))
       (root (and dir (file-name-as-directory
                       (file-truename
                        (directory-file-name
                         (file-name-directory
                          (directory-file-name dir)))))))
       (outside nil))
  (when (and root (not nelisp-emacs-cl-lib-force-shim))
    (dolist (entry load-path)
      (when (and (stringp entry) (file-directory-p entry))
        (unless (string-prefix-p
                 root (file-truename (file-name-as-directory entry)) t)
          (setq outside (cons entry outside)))))
    (let ((load-path (nreverse outside)))
      (when (locate-library "cl-macs")
        (setq nelisp-emacs-cl-lib-genuine (locate-library "cl-lib"))
        (dolist (feature '(cl-lib cl-macs cl-extra cl-seq))
          (require feature nil t))))))
(defconst cl-lib--load-directory
  (let ((source-file
         (or (and (boundp 'load-file-name) load-file-name)
             (and (boundp 'buffer-file-name) buffer-file-name))))
    (cond
     (source-file
      (file-name-directory source-file))
     ((and (boundp 'default-directory)
           (stringp default-directory))
      (let ((src (expand-file-name "src/" default-directory)))
        (if (and (fboundp 'file-directory-p)
                 (file-directory-p src))
            src
          default-directory)))
     (t nil)))
  "Directory that contains the cl-lib shim and its sibling features.")

(defun cl-lib--load-feature (feature)
  "Load FEATURE from the cl-lib shim directory."
  (load (expand-file-name (concat (symbol-name feature) ".el")
                          cl-lib--load-directory)
        nil t))

(defun cl-lib--define-p (symbol)
  "Return non-nil when SYMBOL should be supplied by this shim."
  (or (not (fboundp symbol))
      (and (fboundp 'autoloadp)
           (autoloadp (symbol-function symbol)))))

;; Pull in the existing prefixed subset (cl-loop / cl-defun /
;; cl-defstruct / cl-letf / cl-flet / cl-block / cl-some / cl-every /
;; cl-position / cl-find / cl-remove-if{,-not} / cl-delete-* /
;; cl-union / cl-intersection / cl-sort / cl-case / cl-pushnew / etc.)
 (cl-lib--load-feature 'emacs-cl-macros)

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

(defun cl-lib--standalone-p ()
  "Return non-nil under standalone NeLisp.
The NeLisp reader binds `emacs-version' just like host Emacs, so a bare
`(not (boundp 'emacs-version))' test misfires there.  Detect the
standalone path by a NeLisp-only primitive, matching
`emacs-char-table--standalone-p' in `emacs-char-table.el'."
  (or (fboundp 'nl-write-file)
      (not (boundp 'emacs-version))))

(when (cl-lib--standalone-p)
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
                ;; Three-level c[ad][ad][ad]r: first letter picks setcar/setcdr,
                ;; the remaining two letters name the inner accessor (cXXr).
                ((eq fn 'caaar) (list 'setcar (list 'caar (car args)) value))
                ((eq fn 'caadr) (list 'setcar (list 'cadr (car args)) value))
                ((eq fn 'cadar) (list 'setcar (list 'cdar (car args)) value))
                ((eq fn 'caddr) (list 'setcar (list 'cddr (car args)) value))
                ((eq fn 'cdaar) (list 'setcdr (list 'caar (car args)) value))
                ((eq fn 'cdadr) (list 'setcdr (list 'cadr (car args)) value))
                ((eq fn 'cddar) (list 'setcdr (list 'cdar (car args)) value))
                ((eq fn 'cdddr) (list 'setcdr (list 'cddr (car args)) value))
                ;; (setf (cl-getf PLACE KEY [DEFAULT]) V) -> reassign PLACE to
                ;; the plist with KEY set (recurses through `setf' so PLACE may
                ;; itself be a generalized place).  org-element uses this ~10x.
                ((eq fn 'cl-getf)
                 (list 'setf (car args)
                       (list 'plist-put (car args) (cadr args) value)))
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
                ;; Variable-cell places.  Without these the generic fallback
                ;; below builds a call to `default-value--setter' /
                ;; `symbol-value--setter', names nothing defines.  Measured
                ;; 2026-09-12 against doom-modeline's
                ;;   (setf (if default (default-value 'mode-line-format)
                ;;           mode-line-format)
                ;;         ...)
                ;; which is the form behind the real-init audit's one
                ;; remaining defect of this class.
                ((eq fn 'default-value) (list 'set-default (car args) value))
                ((eq fn 'symbol-value) (list 'set (car args) value))
                ((eq fn 'symbol-function) (list 'fset (car args) value))
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
                ((memq fn '(if progn cond))
                 ;; Control-flow places.  `gv' expands these by recursing into
                 ;; the branch that is actually reached; without the clause
                 ;; they fall through to the synthesized-setter fallback below
                 ;; and emit a call to `if--setter', a name nothing defines.
                 ;; Measured 2026-09-12: the real-init audit reported
                 ;; `void-function: if--setter' from doom-modeline's load, the
                 ;; one remaining defect of its class in 306 init forms.
                 ;; VALUE appears once per branch in the expansion but only the
                 ;; reached branch evaluates it, which is stock `gv' behaviour.
                 (cond
                  ((eq fn 'if)
                   (list 'if (car args)
                         (list 'setf (cadr args) value)
                         (list 'setf (caddr args) value)))
                  ((eq fn 'progn)
                   (append (list 'progn)
                           (butlast args)
                           (list (list 'setf (car (last args)) value))))
                  (t
                   (cons 'cond
                         (mapcar (lambda (clause)
                                   (if (cdr clause)
                                       (append (list (car clause))
                                               (butlast (cdr clause))
                                               (list (list 'setf
                                                           (car (last (cdr clause)))
                                                           value)))
                                     (list (list 'setf (car clause) value))))
                                 args)))))
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
                 (let ((gv-setter (get fn 'cl-gv-setter))
                       (simple-setter (get fn 'cl-simple-setter)))
                   (cond
                    (gv-setter
                     ;; `gv-define-setter' keeps the upstream writer arglist:
                     ;; STORE first, followed by the generalized-place args.
                     ;; Emit a direct macro call so its returned setter form is
                     ;; evaluated, instead of funcalling the macro as a
                     ;; runtime function and merely returning source data.
                     (cons gv-setter (cons value args)))
                    (simple-setter
                     (cons 'funcall
                           (cons (list 'quote simple-setter)
                                 (append args (list value)))))
                    (t
                     (list 'funcall
                           (list 'or
                                 (list 'get (list 'quote fn)
                                       (list 'quote 'cl-struct-setter))
                                 (list 'quote
                                       (intern (concat (symbol-name fn)
                                                       "--setter"))))
                           (car args) value)))))
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
;; to the standalone runtime via `cl-lib--standalone-p' (NOT the naive
;; `(not (stringp (and (boundp 'emacs-version) emacs-version)))' test:
;; standalone NeLisp binds `emacs-version' to the real string "30.1" too,
;; for vendor compatibility, so that test never fires there).

(when (cl-lib--standalone-p)
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

;;;; --- cl-extra: sequence/property/random-number helpers --------------

;; S2 coverage batch (2026-09-28): a handful of `cl-extra' names that are
;; pure data/control functions with no dependency on processes, GUI, or
;; markers/overlays -- everything they call (`cl-defstruct', `cl-mapcar',
;; `cl-coerce', `cl-check-type', `cl-flet', `cl-case', `cl-digit-char-p',
;; `symbol-plist'/`setplist', `terpri', `seq-concatenate') is already
;; present.  Ported verbatim from GNU Emacs 31.1
;; lisp/emacs-lisp/cl-extra.el; guarded like the rest of this file so a
;; genuine cl-extra (if one is ever required first) wins.

(when (cl-lib--define-p 'cl-equalp)
  (defun cl-equalp (x y)
    "Return t if two Lisp objects have similar structures and contents.
This is like `equal', except that it accepts numerically equal
numbers of different types (float vs. integer), and also compares
strings case-insensitively."
    (declare (side-effect-free error-free))
    (cond ((eq x y) t)
          ((stringp x)
           (and (stringp y) (string-equal-ignore-case x y)))
          ((numberp x)
           (and (numberp y) (= x y)))
          ((consp x)
           (while (and (consp x) (consp y) (cl-equalp (car x) (car y)))
             (setq x (cdr x) y (cdr y)))
           (and (not (consp x)) (cl-equalp x y)))
          ((vectorp x)
           (and (vectorp y) (= (length x) (length y))
                (let ((i (length x)))
                  (while (and (>= (setq i (1- i)) 0)
                              (cl-equalp (aref x i) (aref y i))))
                  (< i 0))))
          (t (equal x y)))))

(when (cl-lib--define-p 'cl--mapcar-many)
  (defun cl--mapcar-many (func seqs &optional acc)
    (if (cdr (cdr seqs))
        (let* ((res nil)
               (n (apply #'min (mapcar #'length seqs)))
               (i 0)
               (args (copy-sequence seqs))
               p1 p2)
          (setq seqs (copy-sequence seqs))
          (while (< i n)
            (setq p1 seqs p2 args)
            (while p1
              (setcar p2
                      (if (consp (car p1))
                          (prog1 (car (car p1))
                            (setcar p1 (cdr (car p1))))
                        (aref (car p1) i)))
              (setq p1 (cdr p1) p2 (cdr p2)))
            (if acc
                (push (apply func args) res)
              (apply func args))
            (setq i (1+ i)))
          (and acc (nreverse res)))
      (let ((res nil)
            (x (car seqs))
            (y (nth 1 seqs)))
        (let ((n (min (length x) (length y)))
              (i -1))
          (while (< (setq i (1+ i)) n)
            (let ((val (funcall func
                                (if (consp x) (pop x) (aref x i))
                                (if (consp y) (pop y) (aref y i)))))
              (when acc
                (push val res)))))
        (and acc (nreverse res))))))

(when (cl-lib--define-p 'cl-map)
  (defsubst cl-map (type func seq &rest rest)
    "Map a FUNCTION across one or more SEQUENCEs, returning a sequence.
TYPE is the sequence type to return.
\n(fn TYPE FUNCTION SEQUENCE...)"
    (declare (important-return-value t))
    (let ((res (apply 'cl-mapcar func seq rest)))
      (and type (cl-coerce res type)))))

(when (cl-lib--define-p 'cl-mapl)
  (defun cl-mapl (func list &rest rest)
    "Like `cl-maplist', but does not accumulate values returned by the function.
\n(fn FUNCTION LIST...)"
    (if rest
        (let ((args (cons list (copy-sequence rest)))
              p)
          (while (not (memq nil args))
            (apply func args)
            (setq p args)
            (while p (setcar p (cdr (pop p))))))
      (let ((p list))
        (while p (funcall func p) (setq p (cdr p)))))
    list))

(when (cl-lib--define-p 'cl-concatenate)
  (defun cl-concatenate (type &rest sequences)
    "Concatenate, into a sequence of type TYPE, the argument SEQUENCEs.
\n(fn TYPE SEQUENCE...)"
    (apply #'seq-concatenate type sequences)))

(when (cl-lib--define-p 'cl-nreconc)
  (defsubst cl-nreconc (x y)
    "Equivalent to (nconc (nreverse X) Y)."
    (declare (important-return-value t))
    (nconc (nreverse x) y)))

(when (cl-lib--define-p 'cl-list-length)
  (defun cl-list-length (x)
    "Return the length of list X.  Return nil if list is circular."
    (declare (side-effect-free t))
    (cl-check-type x list)
    (condition-case nil
        (length x)
      (circular-list))))

(when (cl-lib--define-p 'cl--do-remf)
  (defun cl--do-remf (plist tag)
    (let ((p (cdr plist)))
      ;; Can't use `plist-member' here because it goes to the cons-cell
      ;; of TAG and we need the one before.
      (while (and (cdr p) (not (eq (car (cdr p)) tag))) (setq p (cdr (cdr p))))
      (and (cdr p) (progn (setcdr p (cdr (cdr (cdr p)))) t)))))

(when (cl-lib--define-p 'cl-remprop)
  (defun cl-remprop (symbol propname)
    "Remove from SYMBOL's plist the property PROPNAME and its value."
    (let ((plist (symbol-plist symbol)))
      (if (and plist (eq propname (car plist)))
          (progn (setplist symbol (cdr (cdr plist))) t)
        (cl--do-remf plist propname)))))

(when (cl-lib--define-p 'cl-get)
  ;; `autoload' is defined by src/emacs-eval.el, which the bootstrap
  ;; bundle may place after this file (cl-lib.el is force-included at an
  ;; early slot via `nelisp-bootstrap-extra-files').  The autoload only
  ;; feeds the byte-compiler's compiler-macro lookup, so skip it when
  ;; `autoload' is not yet defined instead of requiring emacs-eval, which
  ;; fails there with "Cannot open load file".
  (when (fboundp 'autoload)
    (autoload 'cl--compiler-macro-get "cl-macs"))
  (defun cl-get (sym tag &optional def)
    "Return the value of SYMBOL's PROPNAME property, or DEFAULT if none.
\n(fn SYMBOL PROPNAME &optional DEFAULT)"
    (declare (side-effect-free t)
             (compiler-macro cl--compiler-macro-get)
             (gv-setter (lambda (store) (ignore def) `(put ,sym ,tag ,store))))
    (cl-getf (symbol-plist sym) tag def)))

(when (cl-lib--define-p 'cl-fresh-line)
  (defun cl-fresh-line (&optional stream)
    "Output a newline unless already at the beginning of a line."
    (terpri stream 'ensure)))

(when (cl-lib--define-p 'cl-parse-integer)
  (cl-defun cl-parse-integer (string &key start end radix junk-allowed)
    "Parse integer from the substring of STRING from START to END.
STRING may be surrounded by whitespace chars (chars with syntax ` ').
Other non-digit chars are considered junk.
RADIX is an integer between 2 and 36, the default is 10.  Signal
an error if the substring between START and END cannot be parsed
as an integer unless JUNK-ALLOWED is non-nil."
    (declare (side-effect-free t))
    (cl-check-type string string)
    (let* ((start (or start 0))
           (len   (length string))
           (end   (or end len))
           (radix (or radix 10)))
      (or (<= start end len)
          (error "Bad interval: [%d, %d)" start end))
      (cl-flet ((skip-whitespace ()
                  (while (and (< start end)
                              (= 32 (char-syntax (aref string start))))
                    (setq start (1+ start)))))
        (skip-whitespace)
        (let ((sign (cl-case (and (< start end) (aref string start))
                      (?+ (incf start) +1)
                      (?- (incf start) -1)
                      (t  +1)))
              digit sum)
          (while (and (< start end)
                      (setq digit (cl-digit-char-p (aref string start) radix)))
            (setq sum (+ (* (or sum 0) radix) digit)
                  start (1+ start)))
          (skip-whitespace)
          (cond ((and junk-allowed (null sum)) sum)
                (junk-allowed (* sign sum))
                ((or (/= start end) (null sum))
                 (error "Not an integer string: `%s'" string))
                (t (* sign sum))))))))

;; Random numbers.  `cl--random-state' is a `cl-defstruct' (a stdlib
;; prelude primitive here, see the commentary above), so this whole
;; group is guarded on its constructor rather than repeated per name.
(when (cl-lib--define-p 'cl--make-random-state)
  (defun cl--random-time ()
    "Return high-precision timestamp from `time-convert'.

For example, suitable for use as seed by `cl-make-random-state'."
    (car (time-convert nil t)))

  ;;;###autoload (autoload 'cl-random-state-p "cl-extra")
  ;;;###autoload (function-put 'cl-random-state-p 'side-effect-free 'error-free)
  (cl-defstruct (cl--random-state
                 (:copier nil)
                 (:predicate cl-random-state-p)
                 (:constructor nil)
                 (:constructor cl--make-random-state (vec)))
    (i -1) (j 30) vec)

  ;; Upstream initializes this eagerly to `(cl--make-random-state
  ;; (cl--random-time))'.  In this file's own standalone bootstrap bundle,
  ;; `cl-lib.el' concatenates far ahead of `emacs-time.el', which is what
  ;; defines `time-convert' -- an eager call here hits `time-convert' as
  ;; void-function mid-bootstrap.  Deferring the default state's creation
  ;; to first use (via `cl--random-state-ensure', called only from
  ;; `cl-random'/`cl-make-random-state', both invoked well after the full
  ;; bundle has loaded) keeps `cl--random-time' itself byte-for-byte
  ;; faithful to upstream while sidestepping the load-order hazard.
  (defvar cl--random-state nil)

  (defun cl--random-state-ensure ()
    "Return `cl--random-state', creating the default lazily on first use."
    (or cl--random-state
        (setq cl--random-state (cl--make-random-state (cl--random-time)))))

  (defun cl-random (lim &optional state)
    "Return a pseudo-random nonnegative number less than LIM, an integer or float.
Optional second arg STATE is a random-state object."
    (or state (setq state (cl--random-state-ensure)))
    ;; Inspired by "ran3" from Numerical Recipes.  Additive congruential method.
    (let ((vec (cl--random-state-vec state)))
      (if (integerp vec)
          (let ((i 0) (j (- 1357335 (abs (% vec 1357333)))) (k 1))
            (setf (cl--random-state-vec state)
                  (setq vec (make-vector 55 nil)))
            (aset vec 0 j)
            (while (> (setq i (% (+ i 21) 55)) 0)
              (aset vec i (setq j (prog1 k (setq k (- j k))))))
            (while (< (setq i (1+ i)) 200) (cl-random 2 state))))
      (let* ((i (cl-callf (lambda (x) (% (1+ x) 55)) (cl--random-state-i state)))
             (j (cl-callf (lambda (x) (% (1+ x) 55)) (cl--random-state-j state)))
             (n (aset vec i (logand 8388607 (- (aref vec i) (aref vec j))))))
        (cond
         ((natnump lim)
          (if (<= lim 512) (% n lim)
            (if (> lim 8388607) (setq n (+ (ash n 9) (cl-random 512 state))))
            (let ((mask 1023))
              (while (< mask (1- lim)) (setq mask (1+ (+ mask mask))))
              (if (< (setq n (logand n mask)) lim) n (cl-random lim state)))))
         ((< 0 lim 1.0e+INF)
          (* (/ n '8388608e0) lim))
         (t
          (error "Limit %S not supported by cl-random" lim))))))

  (defun cl-make-random-state (&optional state)
    "Return a copy of random-state STATE, or of the internal state if omitted.
If STATE is t, return a new state object seeded from the time of day."
    (unless state (setq state (cl--random-state-ensure)))
    (if (cl-random-state-p state)
        (copy-sequence state)
      (cl--make-random-state (if (integerp state) state (cl--random-time))))))

;;;; --- introspection -------------------------------------------------

(defconst cl-lib-version "1.0-nemacs-shim"
  "Version of the nelisp-emacs cl-lib shim (= NOT upstream cl-lib).")

;;;; --- S2 coverage batch 3 (2026-09-28): pure cl-lib accessors/vars ---

;; The rest of GNU Emacs 31.1 lisp/emacs-lisp/cl-lib.el's public surface
;; that is pure data/control with no cl-generic, advice, GUI, process, or
;; marker/overlay dependency.  Ported verbatim where the body is
;; self-contained; the 3/4-deep car/cdr accessors are written out as
;; direct `car'/`cdr' nests instead of `(defalias 'cl-caaar #'caaar)'
;; because this substrate does not guarantee the unprefixed 3/4-deep
;; combinators (`caaar', `caaaar', ...) exist -- aliasing to a possibly
;; absent target would still satisfy `fboundp' but would not actually
;; work when called.  `declare' clauses (side-effect-free, gv-setter,
;; ...) are dropped: they are compiler/gv hints only, not part of
;; behavior, and gv internals are out of scope for this batch.  Each
;; definition is guarded so a genuine cl-lib occupying this file's slot
;; (see the deferral block at the top of this file) always wins.

(unless (boundp 'cl--optimize-speed) (defvar cl--optimize-speed 1))
(unless (boundp 'cl--optimize-safety) (defvar cl--optimize-safety 1))

(unless (boundp 'cl-custom-print-functions)
  (defvar cl-custom-print-functions nil
    "List of functions that format user objects for printing.
Each function is called in turn with three arguments: the object, the
stream, and the print level (currently ignored).  If it is able to
print the object it returns true; otherwise it returns nil and the
printer proceeds to the next function on the list."))

(when (cl-lib--define-p 'cl--set-buffer-substring)
  (defun cl--set-buffer-substring (start end val &optional inherit)
    "Delete region from START to END and insert VAL."
    (replace-region-contents start end val 0 nil inherit)
    val))

(when (cl-lib--define-p 'cl--set-substring)
  (defun cl--set-substring (str start end val)
    (if end (if (< end 0) (cl-incf end (length str)))
      (setq end (length str)))
    (if (< start 0) (cl-incf start (length str)))
    (concat (and (> start 0) (substring str 0 start))
            val
            (and (< end (length str)) (substring str end)))))

;; Blocks and exits: `cl-block'/`cl-return-from' expand into `catch'/
;; `throw' directly, but the byte-compiler-facing names are still
;; referenced by macro-expanded code from other files.
(when (cl-lib--define-p 'cl--block-wrapper) (defalias 'cl--block-wrapper #'identity))
(when (cl-lib--define-p 'cl--block-throw) (defalias 'cl--block-throw #'throw))

;; Multiple values: not really supported, just list-based stand-ins.
(when (cl-lib--define-p 'cl--defalias)
  (defun cl--defalias (cl-f el-f &optional doc)
    "Define function CL-F as definition EL-F.
Like `defalias' but marks the alias itself as inlinable."
    (defalias cl-f el-f doc)
    (put cl-f 'byte-optimizer 'byte-compile-inline-expand)))

(when (cl-lib--define-p 'cl-multiple-value-list)
  (defsubst cl-multiple-value-list (expression)
    "Return a list of the multiple values produced by EXPRESSION.
This handles multiple values in Common Lisp style, but it does not
work right when EXPRESSION calls an ordinary Emacs Lisp function
that returns just one value."
    expression))

(when (cl-lib--define-p 'cl-multiple-value-apply)
  (defsubst cl-multiple-value-apply (function expression)
    "Evaluate EXPRESSION to get multiple values and apply FUNCTION to them."
    (apply function expression)))

(when (cl-lib--define-p 'cl-multiple-value-call) (defalias 'cl-multiple-value-call #'apply))
(when (cl-lib--define-p 'cl-nth-value) (defalias 'cl-nth-value #'nth))

;; Declarations.
(when (cl-lib--define-p 'cl--compiling-file)
  (defun cl--compiling-file ()
    "Return non-nil if the current file is being byte-compiled."
    (and (boundp 'byte-compile-current-file)
         (symbol-value 'byte-compile-current-file))))

(unless (boundp 'cl--proclaims-deferred) (defvar cl--proclaims-deferred nil))

;; Numbers.
(when (cl-lib--define-p 'cl-floatp-safe) (defalias 'cl-floatp-safe #'floatp))

(unless (boundp 'cl-digit-char-table)
  (defconst cl-digit-char-table
    (let* ((digits (make-vector 256 nil))
           (populate (lambda (start end base)
                       (mapc (lambda (i)
                               (aset digits i (+ base (- i start))))
                             (number-sequence start end)))))
      (funcall populate ?0 ?9 0)
      (funcall populate ?A ?Z 10)
      (funcall populate ?a ?z 10)
      digits)
    "Digit-value table indexed by character code, used by `cl-digit-char-p'."))

(unless (boundp 'cl-most-positive-float)
  (defconst cl-most-positive-float nil
    "The largest value that a Lisp float can hold.  Set by `cl-float-limits'."))
(unless (boundp 'cl-most-negative-float)
  (defconst cl-most-negative-float nil
    "The largest negative value that a Lisp float can hold."))
(unless (boundp 'cl-least-positive-float)
  (defconst cl-least-positive-float nil
    "The smallest value greater than zero that a Lisp float can hold."))
(unless (boundp 'cl-least-negative-float)
  (defconst cl-least-negative-float nil
    "The smallest value less than zero that a Lisp float can hold."))
(unless (boundp 'cl-least-positive-normalized-float)
  (defconst cl-least-positive-normalized-float nil
    "The smallest normalized Lisp float greater than zero."))
(unless (boundp 'cl-least-negative-normalized-float)
  (defconst cl-least-negative-normalized-float nil
    "The smallest normalized Lisp float less than zero."))
(unless (boundp 'cl-float-epsilon)
  (defconst cl-float-epsilon nil
    "The smallest positive float that adds to 1.0 to give a distinct value."))
(unless (boundp 'cl-float-negative-epsilon)
  (defconst cl-float-negative-epsilon nil
    "The smallest positive float that subtracts from 1.0 to give a distinct value."))

;; Sequence functions.
(when (cl-lib--define-p 'cl-copy-seq) (defalias 'cl-copy-seq #'copy-sequence))
(when (cl-lib--define-p 'cl-svref) (defalias 'cl-svref #'aref))

;; List functions: 4th..10th elements, and the 3/4-deep car/cdr nests.
(when (cl-lib--define-p 'cl-fourth) (defsubst cl-fourth (x) (nth 3 x)))
(when (cl-lib--define-p 'cl-fifth) (defsubst cl-fifth (x) (nth 4 x)))
(when (cl-lib--define-p 'cl-sixth) (defsubst cl-sixth (x) (nth 5 x)))
(when (cl-lib--define-p 'cl-seventh) (defsubst cl-seventh (x) (nth 6 x)))
(when (cl-lib--define-p 'cl-eighth) (defsubst cl-eighth (x) (nth 7 x)))
(when (cl-lib--define-p 'cl-ninth) (defsubst cl-ninth (x) (nth 8 x)))
(when (cl-lib--define-p 'cl-tenth) (defsubst cl-tenth (x) (nth 9 x)))

(when (cl-lib--define-p 'cl-caaar) (defsubst cl-caaar (x) (car (car (car x)))))
(when (cl-lib--define-p 'cl-caadr) (defsubst cl-caadr (x) (car (car (cdr x)))))
(when (cl-lib--define-p 'cl-cadar) (defsubst cl-cadar (x) (car (cdr (car x)))))
(when (cl-lib--define-p 'cl-cdaar) (defsubst cl-cdaar (x) (cdr (car (car x)))))
(when (cl-lib--define-p 'cl-cdadr) (defsubst cl-cdadr (x) (cdr (car (cdr x)))))
(when (cl-lib--define-p 'cl-cddar) (defsubst cl-cddar (x) (cdr (cdr (car x)))))
(when (cl-lib--define-p 'cl-cdddr) (defsubst cl-cdddr (x) (cdr (cdr (cdr x)))))

(when (cl-lib--define-p 'cl-caaaar) (defsubst cl-caaaar (x) (car (car (car (car x))))))
(when (cl-lib--define-p 'cl-caaadr) (defsubst cl-caaadr (x) (car (car (car (cdr x))))))
(when (cl-lib--define-p 'cl-caadar) (defsubst cl-caadar (x) (car (car (cdr (car x))))))
(when (cl-lib--define-p 'cl-caaddr) (defsubst cl-caaddr (x) (car (car (cdr (cdr x))))))
(when (cl-lib--define-p 'cl-cadaar) (defsubst cl-cadaar (x) (car (cdr (car (car x))))))
(when (cl-lib--define-p 'cl-cadadr) (defsubst cl-cadadr (x) (car (cdr (car (cdr x))))))
(when (cl-lib--define-p 'cl-caddar) (defsubst cl-caddar (x) (car (cdr (cdr (car x))))))
(when (cl-lib--define-p 'cl-cadddr) (defsubst cl-cadddr (x) (car (cdr (cdr (cdr x))))))
(when (cl-lib--define-p 'cl-cdaaar) (defsubst cl-cdaaar (x) (cdr (car (car (car x))))))
(when (cl-lib--define-p 'cl-cdaadr) (defsubst cl-cdaadr (x) (cdr (car (car (cdr x))))))
(when (cl-lib--define-p 'cl-cdadar) (defsubst cl-cdadar (x) (cdr (car (cdr (car x))))))
(when (cl-lib--define-p 'cl-cdaddr) (defsubst cl-cdaddr (x) (cdr (car (cdr (cdr x))))))
(when (cl-lib--define-p 'cl-cddaar) (defsubst cl-cddaar (x) (cdr (cdr (car (car x))))))
(when (cl-lib--define-p 'cl-cddadr) (defsubst cl-cddadr (x) (cdr (cdr (car (cdr x))))))
(when (cl-lib--define-p 'cl-cdddar) (defsubst cl-cdddar (x) (cdr (cdr (cdr (car x))))))
(when (cl-lib--define-p 'cl-cddddr) (defsubst cl-cddddr (x) (cdr (cdr (cdr (cdr x))))))

(when (cl-lib--define-p 'cl-acons)
  (defsubst cl-acons (key value alist)
    "Add KEY and VALUE to ALIST.
Return a new list with (cons KEY VALUE) as car and ALIST as cdr."
    (cons (cons key value) alist)))

(when (cl-lib--define-p 'cl-pairlis)
  (defun cl-pairlis (keys values &optional alist)
    "Make an alist from KEYS and VALUES.
Return a new alist composed by associating KEYS to corresponding VALUES;
the process stops as soon as KEYS or VALUES run out.
If ALIST is non-nil, the new pairs are prepended to it."
    (if (null alist)
        (cl-mapcar #'cons keys values)
      (while (and keys values)
        (push (cons (pop keys) (pop values)) alist))
      alist)))

(when (cl-lib--define-p 'cl--do-subst)
  (defun cl--do-subst (new old tree)
    (cond ((eq tree old) new)
          ((consp tree)
           (let ((a (cl--do-subst new old (car tree)))
                 (d (cl--do-subst new old (cdr tree))))
             (if (and (eq a (car tree)) (eq d (cdr tree)))
                 tree (cons a d))))
          (t tree))))

(when (cl-lib--define-p 'cl-constantly)
  (defun cl-constantly (value)
    "Return a function that takes any number of arguments, but returns VALUE."
    (lambda (&rest _) value)))

;;;; --- obsolete cl.el compatibility -----------------------------------

;; Old packages still use `(require 'cl)' together with the pre-cl-lib names.
;; The vendored obsolete cl.el depends on substantially more macroexp/gv
;; machinery than the standalone bootstrap needs.  Install only aliases whose
;; prefixed owners are already present, and keep host Emacs untouched.
(when (cl-lib--standalone-p)
  (dolist (pair '((defstruct . cl-defstruct)
                  (defun* . cl-defun)
                  (defsubst* . cl-defsubst)
                  (defmacro* . cl-defmacro)
                  (function* . cl-function)
                  (case . cl-case)
                  (ecase . cl-ecase)
                  (typecase . cl-typecase)
                  (etypecase . cl-etypecase)
                  (loop . cl-loop)
                  (destructuring-bind . cl-destructuring-bind)
                  (multiple-value-bind . cl-multiple-value-bind)
                  (block . cl-block)
                  (return . cl-return)
                  (return-from . cl-return-from)
                  (incf . cl-incf)
                  (decf . cl-decf)
                  (pushnew . cl-pushnew)
                  (rotatef . cl-rotatef)
                  (shiftf . cl-shiftf)))
    (when (and (not (fboundp (car pair)))
               (fboundp (cdr pair)))
      (defalias (car pair) (cdr pair))))
  ;; S2 coverage batch 3 (2026-09-28): the rest of GNU Emacs 31.1's
  ;; lisp/obsolete/cl.el alias table (its `(dolist (fun '(...)) ...)'
  ;; block), minus the 20 pairs already handled above.  Same guard as
  ;; above: an alias is only installed when its `cl-'-prefixed owner is
  ;; genuinely already present, so this never claims a name that would
  ;; not actually work if called.
  (dolist (pair '((get* . cl-get)
                  (random* . cl-random)
                  (rem* . cl-rem)
                  (mod* . cl-mod)
                  (round* . cl-round)
                  (truncate* . cl-truncate)
                  (ceiling* . cl-ceiling)
                  (floor* . cl-floor)
                  (rassoc* . cl-rassoc)
                  (assoc* . cl-assoc)
                  (member* . cl-member)
                  (delete* . cl-delete)
                  (remove* . cl-remove)
                  (sort* . cl-sort)
                  (mapcar* . cl-mapcar)
                  (remprop . cl-remprop)
                  (getf . cl-getf)
                  (tailp . cl-tailp)
                  (list-length . cl-list-length)
                  (nreconc . cl-nreconc)
                  (revappend . cl-revappend)
                  (concatenate . cl-concatenate)
                  (subseq . cl-subseq)
                  (random-state-p . cl-random-state-p)
                  (make-random-state . cl-make-random-state)
                  (signum . cl-signum)
                  (isqrt . cl-isqrt)
                  (lcm . cl-lcm)
                  (gcd . cl-gcd)
                  (notevery . cl-notevery)
                  (notany . cl-notany)
                  (every . cl-every)
                  (some . cl-some)
                  (mapcon . cl-mapcon)
                  (mapl . cl-mapl)
                  (maplist . cl-maplist)
                  (map . cl-map)
                  (equalp . cl-equalp)
                  (coerce . cl-coerce)
                  (tree-equal . cl-tree-equal)
                  (nsublis . cl-nsublis)
                  (sublis . cl-sublis)
                  (nsubst-if-not . cl-nsubst-if-not)
                  (nsubst-if . cl-nsubst-if)
                  (nsubst . cl-nsubst)
                  (subst-if-not . cl-subst-if-not)
                  (subst-if . cl-subst-if)
                  (subsetp . cl-subsetp)
                  (nset-exclusive-or . cl-nset-exclusive-or)
                  (set-exclusive-or . cl-set-exclusive-or)
                  (nset-difference . cl-nset-difference)
                  (set-difference . cl-set-difference)
                  (nintersection . cl-nintersection)
                  (intersection . cl-intersection)
                  (nunion . cl-nunion)
                  (union . cl-union)
                  (rassoc-if-not . cl-rassoc-if-not)
                  (rassoc-if . cl-rassoc-if)
                  (assoc-if-not . cl-assoc-if-not)
                  (assoc-if . cl-assoc-if)
                  (member-if-not . cl-member-if-not)
                  (merge . cl-merge)
                  (stable-sort . cl-stable-sort)
                  (search . cl-search)
                  (mismatch . cl-mismatch)
                  (count-if-not . cl-count-if-not)
                  (count-if . cl-count-if)
                  (count . cl-count)
                  (position-if-not . cl-position-if-not)
                  (position-if . cl-position-if)
                  (position . cl-position)
                  (find-if-not . cl-find-if-not)
                  (find-if . cl-find-if)
                  (find . cl-find)
                  (nsubstitute-if-not . cl-nsubstitute-if-not)
                  (nsubstitute-if . cl-nsubstitute-if)
                  (nsubstitute . cl-nsubstitute)
                  (substitute-if-not . cl-substitute-if-not)
                  (substitute-if . cl-substitute-if)
                  (substitute . cl-substitute)
                  (delete-duplicates . cl-delete-duplicates)
                  (remove-duplicates . cl-remove-duplicates)
                  (delete-if-not . cl-delete-if-not)
                  (delete-if . cl-delete-if)
                  (remove-if-not . cl-remove-if-not)
                  (remove-if . cl-remove-if)
                  (replace . cl-replace)
                  (fill . cl-fill)
                  (reduce . cl-reduce)
                  (compiler-macroexpand . cl-compiler-macroexpand)
                  (define-compiler-macro . cl-define-compiler-macro)
                  (assert . cl-assert)
                  (check-type . cl-check-type)
                  (typep . cl-typep)
                  (deftype . cl-deftype)
                  (callf2 . cl-callf2)
                  (callf . cl-callf)
                  (letf* . cl-letf*)
                  (letf . cl-letf)
                  (remf . cl-remf)
                  (psetf . cl-psetf)
                  (define-setf-method . define-setf-expander)
                  (the . cl-the)
                  (locally . cl-locally)
                  (multiple-value-setq . cl-multiple-value-setq)
                  (symbol-macrolet . cl-symbol-macrolet)
                  (macrolet . cl-macrolet)
                  (progv . cl-progv)
                  (psetq . cl-psetq)
                  (do-all-symbols . cl-do-all-symbols)
                  (do-symbols . cl-do-symbols)
                  (do* . cl-do*)
                  (do . cl-do)
                  (load-time-value . cl-load-time-value)
                  (eval-when . cl-eval-when)
                  (gentemp . cl-gentemp)
                  (pairlis . cl-pairlis)
                  (acons . cl-acons)
                  (subst . cl-subst)
                  (adjoin . cl-adjoin)
                  (copy-list . cl-copy-list)
                  (ldiff . cl-ldiff)
                  (list* . cl-list*)
                  (tenth . cl-tenth)
                  (ninth . cl-ninth)
                  (eighth . cl-eighth)
                  (seventh . cl-seventh)
                  (sixth . cl-sixth)
                  (fifth . cl-fifth)
                  (fourth . cl-fourth)
                  (third . cl-third)
                  (endp . cl-endp)
                  (rest . cl-rest)
                  (second . cl-second)
                  (first . cl-first)
                  (svref . cl-svref)
                  (copy-seq . cl-copy-seq)
                  (floatp-safe . cl-floatp-safe)
                  (declaim . cl-declaim)
                  (proclaim . cl-proclaim)
                  (nth-value . cl-nth-value)
                  (multiple-value-call . cl-multiple-value-call)
                  (multiple-value-apply . cl-multiple-value-apply)
                  (multiple-value-list . cl-multiple-value-list)
                  (values-list . cl-values-list)
                  (values . cl-values)))
    (when (and (not (fboundp (car pair)))
               (fboundp (cdr pair)))
      (defalias (car pair) (cdr pair))))
  ;; The float-limit constants get the same obsolete-name treatment as
  ;; variables (`define-obsolete-variable-alias' upstream); mirror it
  ;; with a guarded `defvaralias' so an unprefixed name only appears
  ;; once its `cl-'-prefixed owner is actually bound.
  (dolist (pair '((float-negative-epsilon . cl-float-negative-epsilon)
                  (float-epsilon . cl-float-epsilon)
                  (least-negative-normalized-float . cl-least-negative-normalized-float)
                  (least-positive-normalized-float . cl-least-positive-normalized-float)
                  (least-negative-float . cl-least-negative-float)
                  (least-positive-float . cl-least-positive-float)
                  (most-negative-float . cl-most-negative-float)
                  (most-positive-float . cl-most-positive-float)))
    (when (and (not (boundp (car pair)))
               (boundp (cdr pair)))
      (defvaralias (car pair) (cdr pair))))
  (unless (featurep 'cl)
    (provide 'cl)))

(provide 'cl-lib)

;;; cl-lib.el ends here
