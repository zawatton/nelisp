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
;; The contract intentionally hides `nelisp-aot-*', `nelisp-asm-*',
;; `nelisp-elf-write-*', `nelisp-link-*', and Sexp layout details.  No
;; symbol from those backends may leak through the adapter's return values
;; as a raw internal structure that callers must understand.

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)
(require 'nelisp-sys-target)

;; The private AOT backend is loaded lazily by the codegen calls below.
(declare-function nelisp-aot-compile-to-object "nelisp-aot-compiler"
                  (sexp file-path &rest keys))
(declare-function nelisp-aot-compile-sexp "nelisp-aot-compiler"
                  (sexp file-path &rest keys))

(define-error 'nelisp-sys-adapter-error
  "nelisp-sys backend adapter error" 'nelisp-sys-error)

(defun nelisp-sys-adapter-available-p ()
  "Return non-nil when the underlying NeLisp toolchain can be loaded.

Pure-analysis workflows (type/borrow checking) do not need the
backend; only codegen does.  Callers gate native operations on
this predicate so the package degrades cleanly when run as a
front-end-only checker."
  (and (locate-library "nelisp-aot-compiler")
       (locate-library "nelisp-static-linker")
       t))

(defun nelisp-sys-adapter--arch-format (target)
  "Return the backend (ARCH . FORMAT) keys for TARGET (triple or descriptor).
Maps the target's object format to the AOT backend's `format' arg."
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
SEXP must be a plain AOT integer-subset form (a `defun' or a
`seq' of `defun's) — the backend lowers the typed AST to this shape
before calling here.  TARGET is a triple string or descriptor.  This
is the ONLY adapter call that drives the private AOT compiler
(Doc 130 extraction criterion 6).  Returns OUTPUT-PATH."
  (unless (nelisp-sys-adapter-available-p)
    (signal 'nelisp-sys-adapter-error
            (list "NeLisp toolchain not available for codegen")))
  (require 'nelisp-aot-compiler)
  (let* ((af (nelisp-sys-adapter--arch-format target))
         (arch (car af))
         (fmt (cdr af)))
    (nelisp-aot-compile-to-object sexp output-path :arch arch :format fmt)))

(defun nelisp-sys-adapter-compile-executable (program output-path target)
  "Compile AOT PROGRAM to a static-linked native executable.
PROGRAM is a single top-level AOT form (typically a `seq' of
helper `defun's ending in an `exit'); the standalone-binary emitter
generates `_start' and links a freestanding ELF64.  Drives the private
AOT compiler (Doc 130 extraction criterion 6).  Returns OUTPUT-PATH.

Doc 133 Phase 7: this is the freestanding-executable adapter call that
lets cutover work be e2e-verified by emitting + running standalone
binaries (the self-host verification model), with no cargo/Rust build."
  (unless (nelisp-sys-adapter-available-p)
    (signal 'nelisp-sys-adapter-error
            (list "NeLisp toolchain not available for executable codegen")))
  (require 'nelisp-aot-compiler)
  (nelisp-aot-compile-sexp
   program output-path :arch (nelisp-sys-target-arch target))
  output-path)

(defun nelisp-sys-adapter-archive-static-lib (objects output-path)
  "Archive OBJECTS (a list of object-file paths) into a static library.
Uses the host `ar'.  Returns OUTPUT-PATH; signals `nelisp-sys-adapter-error'
when `ar' is missing or fails."
  (let ((ar (executable-find "ar")))
    (unless ar
      (signal 'nelisp-sys-adapter-error (list "ar not found for static-lib archive")))
    (let ((rc (apply #'call-process ar nil nil nil "rcs" output-path objects)))
      (unless (eq rc 0)
        (signal 'nelisp-sys-adapter-error
                (list (format "ar failed with status %S" rc)))))
    output-path))

(provide 'nelisp-sys-adapter-nelisp)

;;; nelisp-sys-adapter-nelisp.el ends here
