;;; emacs-parity-eieio.el --- eieio class-system setf-place bootstrap fix -*- lexical-binding: t; -*-
;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- struct/type registry hooks for the library cl-defstruct.

;; This shim repairs the single core defect that collapses the entire eieio
;; class system on the standalone NeLisp substrate (~64 of the 238 caught
;; errors in the real-init audit: every "Given parent class %S is not a
;; class", the whole transient-*/plz-*/emacsql-*/treemacs-scope defclass
;; family, and their downstream void-function constructors).

;;; Root cause (see docs/design/40-nemacs-core-gap-catalog.org §3.1)
;;
;; The active `cl-defstruct' is the prelude version baked into the binary
;; (`vendor/nelisp/lisp/nelisp-cl-macros.el:472').  Its main accessor loop
;; (lines 638-644) emits, for each slot at record index I,
;;
;;     (defun NAME-SLOT (rec) (nelisp--record-ref rec I))
;;
;; but it never records the (accessor . index) pair in the alist
;; `nelisp-cl-macros--accessor-info'.  The prelude `setf' resolves a struct
;; slot place *only* through that alist -- `nelisp-cl-macros.el:1174' turns
;; `(setf (NAME-SLOT rec) v)' into `(nelisp--record-set rec I v)' by looking
;; the accessor up there.  With the alist unpopulated, every
;; `(setf (NAME-SLOT rec) v)' falls through to the "setf: unsupported place"
;; signal.
;;
;; `eieio-defclass-internal' (`vendor/.../eieio-core.el') builds *all* of its
;; class objects by mutating slots through `setf'/`cl-callf'/`cl-pushnew' on
;; `eieio--class-*' (and the inherited `cl--class-parents') accessors.  The
;; very first such mutation -- `(setf (eieio--class-parents newc) ...)' while
;; bootstrapping the root class `eieio-default-superclass' -- aborts, so the
;; root class is never registered as a valid `cl--class', and every later
;; `(defclass ...)' then fails with "Given parent class ... is not a class".
;;
;; The record mutator `nelisp--record-set' exists and the `setf' resolution
;; path is correct; the *only* thing missing is the accessor->index
;; registration.  This is a real repair of the missing registration, not a
;; stub or an error swallow: after it, `(setf (eieio--class-parents X) v)'
;; performs the genuine record write.

;;; Fix
;;
;; Register the accessor->index entries the prelude `cl-defstruct' omitted, so
;; the prelude `setf' resolves them to real `nelisp--record-set' writes.  Two
;; struct types matter for the eieio bootstrap:
;;
;;   cl--class      (bootstrap stand-in, nemacs-bootstrap.repl:1021)
;;                  slots: name docstring parents slots index-table   -> 0..4
;;   eieio--class   (:include cl--class, eieio-core.el:85)
;;                  parent slots 0..4 then own slots
;;                  children initarg-tuples class-slots
;;                  class-allocation-values default-object-cache options -> 5..10
;;
;; Record index == `nelisp--record-ref' index (the accessor and the setter
;; share the same I), so no type-tag offset adjustment is required.
;;
;; This runs at loader-start (via the nemacs-next-session dolist), before the
;; runtime `(require 'eieio)' that pulls in eieio-core, so the entries are in
;; place when `eieio-defclass-internal's `setf' forms are expanded.

(defvar emacs-parity-eieio--standalone-p
  (and (boundp 'nelisp-cl-macros--accessor-info)
       (fboundp 'nelisp--record-set))
  "Non-nil only on the standalone NeLisp substrate.
Guards the registration so a host Emacs (which has real gv/eieio) is
left untouched.")

(defconst emacs-parity-eieio--accessor-index
  '(;; cl--class stand-in (indices 0..4)
    (cl--class-name . 0)
    (cl--class-docstring . 1)
    (cl--class-parents . 2)
    (cl--class-slots . 3)
    (cl--class-index-table . 4)
    ;; eieio--class = (:include cl--class): parent slots 0..4 ...
    (eieio--class-name . 0)
    (eieio--class-docstring . 1)
    (eieio--class-parents . 2)
    (eieio--class-slots . 3)
    (eieio--class-index-table . 4)
    ;; ... then eieio--class own slots 5..10
    (eieio--class-children . 5)
    (eieio--class-initarg-tuples . 6)
    (eieio--class-class-slots . 7)
    (eieio--class-class-allocation-values . 8)
    (eieio--class-default-object-cache . 9)
    (eieio--class-options . 10))
  "Accessor -> record slot index for the eieio bootstrap struct types.
Mirrors the positional slot order of `cl--class' and its `:include'
child `eieio--class' as declared in eieio-core.el.")

(when emacs-parity-eieio--standalone-p
  (dolist (entry emacs-parity-eieio--accessor-index)
    ;; Idempotent: only add if not already present with the same index.
    (let ((cur (assq (car entry) nelisp-cl-macros--accessor-info)))
      (unless (and cur (eq (cdr cur) (cdr entry)))
        (when cur
          (setq nelisp-cl-macros--accessor-info
                (delq cur nelisp-cl-macros--accessor-info)))
        (setq nelisp-cl-macros--accessor-info
              (cons entry nelisp-cl-macros--accessor-info)))))
  ;; The prelude `cl-defstruct' emits an accessor
  ;;   (defun NAME-SLOT (rec) (nelisp--record-ref rec I))
  ;; only for a struct's OWN slots -- never for slots inherited through
  ;; `:include'.  So `eieio--class's inherited cl--class slots
  ;; (name/docstring/parents/slots/index-table at indices 0..4) get no reader
  ;; function, and every `(eieio--class-slots X)' / `(eieio--class-parents X)'
  ;; call signals `void-function' (only the own-slot readers such as
  ;; `eieio--class-class-slots' at index 7 exist).  Define each missing
  ;; accessor as the exact positional record reader the prelude would have
  ;; emitted, over the very index the setf place above already trusts -- a
  ;; genuine accessor, not a stub or an error swallow.  `fboundp'-guarded so a
  ;; prelude- or eieio-core-generated accessor is never clobbered, and the
  ;; index is inlined (no free-variable capture) to mirror the prelude form.
  (dolist (entry emacs-parity-eieio--accessor-index)
    (let ((accessor (car entry))
          (index (cdr entry)))
      (unless (fboundp accessor)
        (fset accessor `(lambda (rec) (nelisp--record-ref rec ,index))))))
  ;; `eieio-defclass-internal' STORES each class object with
  ;; `(setf (cl--find-class CNAME) NEWC)' (eieio-core.el:221,382).  That setf
  ;; place resolves through cl--find-class's `cl-simple-setter' property, which
  ;; `src/cl-lib.el:391' installs -- but only if that file's top-level `put'
  ;; has already run when the storing form is *expanded*.  Any `(setf
  ;; (cl--find-class ...) ...)' whose macro-expansion precedes that `put'
  ;; (e.g. the eieio-default-superclass root store, or oclosure) bakes in the
  ;; "unsupported place" error branch and the class is never registered ->
  ;; residual "Given parent class ... is not a class".  Register it here too,
  ;; at loader-start, before any runtime `(require 'eieio)'.
  (when (and (fboundp 'cl--set-find-class)
             (not (get 'cl--find-class 'cl-simple-setter)))
    (put 'cl--find-class 'cl-simple-setter 'cl--set-find-class))
  (message "emacs-parity-eieio: registered %d struct accessor setf places%s"
           (length emacs-parity-eieio--accessor-index)
           (if (get 'cl--find-class 'cl-simple-setter) " + cl--find-class" "")))

;; `cl--struct-name-p'/`cl--builtin-type-p'/`cl-struct-define' are real
;; Emacs's `cl-preloaded.el' functions (dumped before any Lisp loads, so no
;; GNU source ever defines them itself); NeLisp core's own prelude
;; (nelisp-stdlib-prelude.el) already ships correct `unless fboundp'
;; fallbacks for exactly these three (verbatim GNU semantics, adapted for a
;; runtime with no real EIEIO class registry).  But `emacs-stub-bulk.el''s
;; generic "unknown name -> safe no-op" bulk list ALSO names all three
;; (plus `cl--struct-class-p'/`-named'/`-print'/`-slots'/`-type' and
;; `cl--struct-get-class', which core has no fallback for at all), and it
;; wins the race: on this checkout, `emacs-stub-bulk.el' installs before
;; whatever makes core's own guarded fallback apply, so `cl--struct-name-p'
;; ends up bound to `(lambda (&rest _) nil)' -- NOT a safe no-op here,
;; since genuine `cl-macs.el''s `cl-defstruct' (unconditionally active once
;; the base bundle finishes loading; see `src/emacs-cl-macros.el' and Doc
;; 40 §3.1's "prelude cl-defstruct" note) calls it unconditionally as its
;; very first step and aborts the whole struct definition when it answers
;; nil for a perfectly valid name.  Reproduces with zero magit content: the
;; base bundle's own bundled `cl-macs.el' self-applying
;; `(cl-define-compiler-macro cl--block-wrapper ...)' does not trip this
;; (a compiler-macro definition, not a struct), but `eieio-core.el''s own
;; `(cl-defstruct cl--class ...)' bootstrap does, observed via S5.4 as
;; `wrong-type-argument: (cl-struct-name-p cl--class name)'.  Re-supply
;; core's own correct logic here, unconditionally (matching this file's
;; existing unconditional-override pattern), so it does not depend on
;; winning that load-order race a second time.  `cl--struct-get-class' is
;; ALSO in `emacs-stub-bulk.el''s list with no core fallback at all; supply
;; it too (consistent with `cl-struct-define''s own vector shape just
;; above: `(vector 'nelisp--cl-struct-class name slots children-sym tag)',
;; matching `nelisp-stdlib-prelude.el:18749') since `:include'-based
;; `cl-defstruct' forms (`eieio--class' includes `cl--class') read the
;; parent class through it.
(defun cl--builtin-type-p (name)
  "Verbatim GNU `cl-preloaded.el' early-bootstrap fallback: this substrate
has no `built-in-class-p'/EIEIO-style built-in-type registry, so every
name correctly reads as \"not a builtin type\"."
  (if (not (fboundp 'built-in-class-p))
      nil
    (let ((class (and (symbolp name) (get name 'cl--class))))
      (and class (built-in-class-p class)))))

(defun cl--struct-name-p (name)
  "Return t if NAME is a valid structure name for `cl-defstruct'."
  (and name (symbolp name) (not (keywordp name))
       (not (cl--builtin-type-p name))))

(unless (fboundp 'cl-struct-define)
  (defun cl-struct-define (name _docstring _parent _type named slots
                                 children-sym tag _print)
    (if (boundp children-sym)
        (add-to-list children-sym tag)
      (set children-sym (list tag)))
    (let ((class (vector 'nelisp--cl-struct-class name slots children-sym tag)))
      (unless (or (eq named t) (eq tag name))
        (set tag class)
        (fset tag :quick-object-witness-check))
      (setf (cl--find-class name) class))))

(defun emacs-parity-eieio--struct-class-p (object)
  "Non-nil if OBJECT is one of `cl-struct-define''s registered class
vectors (`[nelisp--cl-struct-class NAME SLOTS CHILDREN-SYM TAG]', see
`nelisp-stdlib-prelude.el:18749')."
  (and (vectorp object) (> (length object) 0)
       (eq (aref object 0) 'nelisp--cl-struct-class)))

(unless (fboundp 'cl--struct-get-class)
  (defun cl--struct-get-class (name)
    "Return NAME's registered struct class object, or nil.
NAME is usually a symbol to look up via `cl--find-class', but genuine
`cl-macs.el' callers (e.g. `cl-struct-slot-info', via `cl-defstruct''s
own `:include' handling: `(cl-struct-slot-info include)' where `include'
is already `(cl--struct-get-class include-name)''s RESULT, not a name)
also pass an already-resolved class object straight through -- real
`cl-preloaded.el''s C implementation accepts both; this substrate-side
port must too, or `(get VECTOR 'cl--class)' aborts with
`wrong-type-argument: symbolp' on the second, already-resolved call."
    (if (emacs-parity-eieio--struct-class-p name)
        name
      (let ((class (and (symbolp name) (fboundp 'cl--find-class)
                         (cl--find-class name))))
        (and (emacs-parity-eieio--struct-class-p class) class)))))

;; `:include' layout fix (S5.4, eieio accessor index mix-up).  GNU
;; `cl-defstruct' resolves `(:include PARENT)' through
;; `(cl-struct-slot-info PARENT)', i.e. `(cl--struct-get-class PARENT)'.  The
;; `cl--class' stand-in is defined by the PRELUDE `cl-defstruct', which never
;; calls `cl-struct-define', so its class object was nil and `eieio--class'
;; (`:include cl--class') was laid out with ZERO inherited slots: its own
;; `children' landed at record index 1, `class-slots' at 3, and so on, while
;; the accessor table above (and GNU) put parents/slots at 2/3.  That made
;; `eieio--class-slots' read the `parents' list ("wrong-type-argument arrayp
;; (#s(built-in-class))" in `eieio-defclass-internal').  Register the
;; stand-in through `cl-struct-define' with GNU's slot order so the include
;; contributes indices 1..5 and eieio--class's own slots start at 6 (record
;; indices; the tag is index 0).
(defun emacs-parity-eieio--raw-desc-class-p (class)
  "Non-nil if CLASS is a `cl-struct-define' vector holding raw slot descs."
  (and (emacs-parity-eieio--struct-class-p class)
       (listp (aref class 2))))

;; The substrate `cl-struct-define' (prelude/fallback above) ignored PARENT, so
;; a child struct's tag never reached its ancestors' `cl-struct-NAME-tags'
;; lists: `(cl--class-p (eieio--class-make ...))' answered nil and every
;; `cl--class-*' accessor signalled `(wrong-type-argument cl--class OBJ)' on
;; an `eieio--class' record (transient-child in the magit bundle).  Real
;; `cl-struct-define' pushes the tag onto every ancestor's children list.
(when emacs-parity-eieio--standalone-p
  (defun cl-struct-define (name _docstring parent _type named slots
                                children-sym tag _print)
    (if (boundp children-sym)
        (add-to-list children-sym tag)
      (set children-sym (list tag)))
    (let ((class (vector 'nelisp--cl-struct-class name slots children-sym tag
                         parent)))
      (unless (or (eq named t) (eq tag name))
        (set tag class)
        (fset tag :quick-object-witness-check))
      (let ((p parent))
        (while p
          (let ((pc (cl--struct-get-class p)))
            (if (emacs-parity-eieio--struct-class-p pc)
                (progn (add-to-list (aref pc 3) tag)
                       (setq p (and (> (length pc) 5) (aref pc 5))))
              (setq p nil)))))
      (setf (cl--find-class name) class))))

(when emacs-parity-eieio--standalone-p
  (when (and (fboundp 'cl--class-p) (fboundp 'cl-struct-define)
             (not (cl--struct-get-class 'cl--class)))
    (cl-struct-define 'cl--class nil nil nil t
                      '((cl-tag-slot) (name) (docstring) (parents)
                        (slots) (index-table))
                      'cl-struct-cl--class-tags 'cl--class nil))
  ;; `cl-struct-slot-info' (genuine cl-macs.el) decodes a class through
  ;; `cl--struct-class-slots'/`-type', which `emacs-stub-bulk.el' otherwise
  ;; leaves as `(lambda (&rest _) nil)'.  The substrate `cl-struct-define'
  ;; keeps the RAW descs `(NAME DEFAULT . OPTS)' (tag slot first); decode them
  ;; into the descriptor vector real `cl-preloaded.el' would have stored.
  (defun cl--struct-class-slots (class)
    "Return CLASS's slots as a vector of `cl-slot-descriptor' objects."
    (if (and (emacs-parity-eieio--raw-desc-class-p class)
             (fboundp 'cl--make-slot-descriptor))
        (let (out)
          (dolist (d (aref class 2))
            (unless (eq (car d) 'cl-tag-slot)
              (let ((opts (cddr d)))
                (push (cl--make-slot-descriptor
                       (car d) (cadr d)
                       (if (plist-member opts :type) (plist-get opts :type) t)
                       nil)
                      out))))
          (vconcat (nreverse out)))
      (vector)))
  (defun cl--struct-class-type (_class)
    "Substrate structs are always `record'-typed (GNU stores nil)."
    nil))

;; cl-macs.el declares its arglist-destructuring state with BARE `(defvar V)'
;; forms, which the standalone bootstrap drops; `cl--transform-lambda' then
;; `let*'-binds them lexically and `cl--do-arglist' (a separate function)
;; hits "void-variable cl--bind-lets".  Make them genuinely special.
(when emacs-parity-eieio--standalone-p
  (defvar cl--bind-block nil)
  (defvar cl--bind-defs nil)
  (defvar cl--bind-enquote nil)
  (defvar cl--bind-lets nil)
  (defvar cl--bind-forms nil)
  ;; Same bare-`defvar' loss for cl-seq.el's keyword-parsing state
  ;; ("void-variable cl-test" in the magit bundle) and cl-macs.el's loop
  ;; and optimize state.
  (defvar cl--alist nil)
  (defvar cl-if nil)
  (defvar cl-if-not nil)
  (defvar cl-key nil)
  (defvar cl-test nil)
  (defvar cl-test-not nil)
  (defvar cl--loop-accum-var nil)
  (defvar cl--loop-accum-vars nil)
  (defvar cl--loop-args nil)
  (defvar cl--loop-bindings nil)
  (defvar cl--loop-body nil)
  (defvar cl--loop-conditions nil)
  (defvar cl--loop-finally nil)
  (defvar cl--loop-finish-flag nil)
  (defvar cl--loop-first-flag nil)
  (defvar cl--loop-initially nil)
  (defvar cl--loop-iterator-function nil)
  (defvar cl--loop-name nil)
  (defvar cl--loop-result nil)
  (defvar cl--loop-result-explicit nil)
  (defvar cl--loop-result-var nil)
  (defvar cl--loop-steps nil)
  (defvar cl--loop-symbol-macs nil)
  (defvar cl--optimize-safety nil)
  (defvar cl--optimize-speed nil))

;; NeLisp-core gap (minimal repro: `(type-of (record (record 'cl--class 'bar) 1))'
;; answers the tag RECORD, GNU answers its name `bar'): EIEIO objects carry
;; their class object as record tag, and `cl--class-p'/cl-generic dispatch
;; test `(memq (type-of obj) TAGS)', so every method call on an instance
;; ("cl-no-applicable-method initialize-instance") and `make-instance' failed.
;; Library-side shim until core follows GNU: resolve a record tag to its
;; class name (slot 1 of the class record) like `Ftype_of' does.
(when (and emacs-parity-eieio--standalone-p
           (fboundp 'type-of)
           (recordp (type-of (record (record 'emacs-parity-eieio--probe 'name) 1))))
  (let ((orig (symbol-function 'type-of)))
    (fset 'type-of
          (lambda (object)
            (let ((type (funcall orig object)))
              (if (and (recordp type) (> (length type) 1))
                  (aref type 1)
                type))))))

;; `emacs-stub-bulk.el' otherwise leaves `cl-type-of' as `(lambda (&rest _) nil)',
;; which is what cl-generic's typeof generalizer dispatches on in GNU 30+.
;; Same semantics as the GNU C primitive on the value classes this runtime
;; distinguishes; records answer their tag / class name via `type-of'.
(when (and emacs-parity-eieio--standalone-p (not (fboundp 'cl-type-of)))
  (defun cl-type-of (object)
    "Return OBJECT's most specific type symbol (GNU `cl-type-of' subset)."
    (cond ((null object) 'null)
          ((integerp object) 'fixnum)
          ((floatp object) 'float)
          ((symbolp object) 'symbol)
          ((stringp object) 'string)
          ((consp object) 'cons)
          ((recordp object) (let ((ty (type-of object))) (if (symbolp ty) ty 'record)))
          ((vectorp object) 'vector)
          ((and (fboundp 'hash-table-p) (hash-table-p object)) 'hash-table)
          ((functionp object) 'function)
          (t 'atom))))

;; Core `cl-defmethod' dispatch (nelisp-cl-macros.el) matches a record argument
;; through `nelisp--record-type' and the `:include' registry
;; `nelisp-cl-macros--struct-info'.  An EIEIO instance's tag is its class
;; RECORD, absent from that registry, so `(cl-defmethod F ((x SOME-CLASS)))'
;; never applied to `make-instance' results ("cl-no-applicable-method
;; initialize-instance").  Bridge it library-side: report the class NAME as
;; the record type and let the ancestry walks fall back to the class's first
;; EIEIO parent.  (Core gap: EIEIO classes are not struct types for
;; `nelisp-cl-generic'; repro in the S5.4 report.)
(defun emacs-parity-eieio--class-parent-name (tag)
  "Return the first EIEIO parent class name of class symbol TAG, or nil."
  (let ((c (and (symbolp tag) tag (get tag 'cl--class))))
    (when (and c (fboundp 'eieio--class-p) (eieio--class-p c))
      (let (r)
        (dolist (p (eieio--class-parents c))
          (when (and (null r) (eieio--class-p p))
            (setq r (eieio--class-name p))))
        r))))

(when (and emacs-parity-eieio--standalone-p
           (fboundp 'nelisp--record-type)
           (fboundp 'nelisp-cl-generic--struct-parent)
           (fboundp 'nelisp-cl-macros--struct-isa))
  (let ((orig-type (symbol-function 'nelisp--record-type))
        (orig-parent (symbol-function 'nelisp-cl-generic--struct-parent))
        (orig-isa (symbol-function 'nelisp-cl-macros--struct-isa)))
    (fset 'nelisp--record-type
          (lambda (record)
            (let ((ty (funcall orig-type record)))
              (if (and (recordp ty) (> (length ty) 1)) (aref ty 1) ty))))
    (fset 'nelisp-cl-generic--struct-parent
          (lambda (tag)
            (or (funcall orig-parent tag)
                (emacs-parity-eieio--class-parent-name tag))))
    (fset 'nelisp-cl-macros--struct-isa
          (lambda (tag target)
            (or (funcall orig-isa tag target)
                (let ((p (emacs-parity-eieio--class-parent-name tag)))
                  (and p (funcall 'nelisp-cl-macros--struct-isa p target))))))))

(provide 'emacs-parity-eieio)
;;; emacs-parity-eieio.el ends here
