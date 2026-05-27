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

(provide 'nelisp-sys-adapter-nelisp)

;;; nelisp-sys-adapter-nelisp.el ends here
