;;; emacs-parity-eieio.el --- eieio class-system setf-place bootstrap fix -*- lexical-binding: t; -*-

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

(provide 'emacs-parity-eieio)
;;; emacs-parity-eieio.el ends here
