;;; nelisp-sys-adapter-nelisp.el --- NeLisp toolchain adapter for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The ONLY module in nelisp-sys allowed to call private NeLisp internals
;; (Doc 130 boundary rule + extraction criterion 6).  Every backend
;; operation the rest of the package needs is funnelled through a small,
;; named adapter contract so the package can later be extracted with this
;; file replaced by a standalone backend.
;;
;; Adapter contract (kept under ~20 public calls, extraction criterion 5):
;;
;;   (nelisp-sys-adapter-available-p)            -> non-nil if toolchain loadable
;;   (nelisp-sys-adapter-compile-to-object MODULE TARGET OUTPUT-PATH)
;;   (nelisp-sys-adapter-link-executable OBJECTS TARGET OUTPUT-PATH &optional OPTS)
;;   (nelisp-sys-adapter-archive-static-lib OBJECTS TARGET OUTPUT-PATH)
;;   (nelisp-sys-adapter-target-triple)          -> host triple string
;;   (nelisp-sys-adapter-asm-buffer TARGET)      -> fresh asm buffer
;;   ... (added incrementally as backend phases land)
;;
;; The contract intentionally hides `nelisp-phase47-*', `nelisp-asm-*',
;; `nelisp-elf-write-*', `nelisp-link-*', and Sexp layout details.  No
;; symbol from those backends may leak through the adapter's return values
;; as a raw internal structure that callers must understand.

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)
(require 'nelisp-sys-target)

;; The private Phase 47 backend is loaded lazily by the codegen calls below.
(declare-function nelisp-phase47-compile-to-object "nelisp-phase47-compiler"
                  (sexp file-path &rest keys))

(define-error 'nelisp-sys-adapter-error
  "nelisp-sys backend adapter error" 'nelisp-sys-error)

(defun nelisp-sys-adapter-available-p ()
  "Return non-nil when the underlying NeLisp toolchain can be loaded.

Pure-analysis workflows (type/borrow checking) do not need the
backend; only codegen does.  Callers gate native operations on
this predicate so the package degrades cleanly when run as a
front-end-only checker."
  (and (locate-library "nelisp-phase47-compiler")
       (locate-library "nelisp-static-linker")
       t))

(defun nelisp-sys-adapter--arch-format (target)
  "Return the backend (ARCH . FORMAT) keys for TARGET (triple or descriptor).
Maps the target's object format to the Phase 47 backend's `format' arg."
  (let ((arch (nelisp-sys-target-arch target))
        (obj (nelisp-sys-target-object-format target)))
    (cons arch
          (cond ((eq obj 'elf64) 'elf)
                ((eq obj 'mach-o64) 'mach-o)
                ((eq obj 'pe-coff) 'coff)
                (t (signal 'nelisp-sys-adapter-error
                           (list (format "unsupported object format: %S" obj))))))))

(defun nelisp-sys-adapter-compile-to-object (sexp output-path target)
  "Compile lowered SEXP to a native object at OUTPUT-PATH for TARGET.
SEXP must be a plain Phase 47 integer-subset form (a `defun' or a
`seq' of `defun's) — the backend lowers the typed AST to this shape
before calling here.  TARGET is a triple string or descriptor.  This
is the ONLY adapter call that drives the private Phase 47 compiler
(Doc 130 extraction criterion 6).  Returns OUTPUT-PATH."
  (unless (nelisp-sys-adapter-available-p)
    (signal 'nelisp-sys-adapter-error
            (list "NeLisp toolchain not available for codegen")))
  (require 'nelisp-phase47-compiler)
  (let* ((af (nelisp-sys-adapter--arch-format target))
         (arch (car af))
         (fmt (cdr af)))
    (nelisp-phase47-compile-to-object sexp output-path :arch arch :format fmt)))

(provide 'nelisp-sys-adapter-nelisp)

;;; nelisp-sys-adapter-nelisp.el ends here
