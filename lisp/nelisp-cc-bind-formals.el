;;; nelisp-cc-bind-formals.el --- Phase 47 nl_bind_formals ABI stub  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 ABI stub for the `nl_bind_formals' extern defined in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body that was
;; in `bind_formals' (eval/mod.rs, ~85 LOC) has been physically deleted
;; and replaced by `nl_bind_formals' which is called from elisp by
;; `nelisp-cc-apply-lambda-inner.el'.
;;
;; ABI: nl_bind_formals(formals_ptr, args_list_ptr, env) -> i64
;;   formals_ptr:   *const Sexp — cons list of formal param symbols
;;                               (may include &optional / &rest markers).
;;   args_list_ptr: *const Sexp — cons list of evaluated argument values.
;;   env:           *mut c_void — &mut Env cast.
;;   Returns: 0=Ok (all formals bound in current frame), 1=Err.
;;
;; No source form is emitted from this file; the stub feature is required
;; by `nelisp-cc-apply-lambda-inner' only to ensure this file is loaded
;; and its provide runs before `nl_apply_lambda_inner' is compiled.

;;; Code:

;; No compiled functions — nl_bind_formals is a pure Rust ABI extern.

(provide 'nelisp-cc-bind-formals)

;;; nelisp-cc-bind-formals.el ends here
