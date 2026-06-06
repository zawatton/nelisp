;;; nelisp-cc-mirror-clear-function.el --- Doc 111 §111.E #10 mirror_clear_function  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group B helper #10 — `mirror_clear_function'.  Phase
;; 47 reimplementation of the env-mirror entry function-slot clearer
;; (= `fmakunbound' fast path).  Compose-on-8 with slot 1 + unbound-
;; marker.  Mirrors
;; `build-tool/src/eval/env_mirror.rs::mirror_clear_function'.
;;
;; The unbound-marker is passed as an explicit `*const Sexp' argument
;; (= same convention as helper #9 `mirror_clear_value'; see that
;; file's commentary for the rationale).

;;; Code:

(defconst nelisp-cc-mirror-clear-function--source
  '(seq
    (defun nelisp_mirror_clear_function_apply (entry-ptr unbound-ptr)
      ;; Inner: dispatch on entry-ptr.  Slot 1 = function cell.
      ;; `record-slot-set' returns rax=1 (Doc 111 §111.E enabler) so
      ;; the `(and X 1)' chain reliably returns 1 after the write.
      (if (= entry-ptr 0)
          0
        (and (record-slot-set entry-ptr 1 unbound-ptr) 1)))
    (defun nelisp_mirror_clear_function (mirror-ptr sym-ptr unbound-ptr)
      ;; mirror-ptr:  *const Sexp pointing at the env-mirror Record.
      ;; sym-ptr:     *const Sexp pointing at the Sexp::Symbol / Str.
      ;; unbound-ptr: *const Sexp pointing at the unbound-marker sentinel.
      ;;
      ;; Returns: i64.  1 on hit (slot 1 set to unbound), 0 on miss.
      ;; Production callers (= `fmakunbound') ignore the return.
      (nelisp_mirror_clear_function_apply
       (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
       unbound-ptr)))
  "AOT source for Doc 111 §111.E #10 `mirror_clear_function'.

Compose-on-8 with the unbound-marker pointer passed as the new slot
value.  Returns i64 1 on hit / 0 on miss.")

(provide 'nelisp-cc-mirror-clear-function)

;;; nelisp-cc-mirror-clear-function.el ends here
