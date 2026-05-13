;;; nelisp-stdlib-format.el --- Doc 86 §86.1.e Tier 2 simple wrappers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 86 §86.1.e (2026-05-10) — Tier 2 simple `bi_*' arms migrated
;; from Rust to elisp on top of three new trampolines in
;; `build-tool/src/jit/strings.rs':
;;
;;   `nelisp--concat-ints'      — `nl_jit_concat_ints'    (`:trampoline-unary')
;;   `nelisp--make-mut-string'  — `nl_jit_make_mut_str'   (`:trampoline-unary')
;;   `nelisp--format-float-body' — `nl_jit_format_float' (`:trampoline-format-float')
;;
;; The first two ride the existing `nl-jit-call-out-1' bridge
;; primitive (= `*const Sexp, *mut Sexp -> i64' shape).  The third
;; uses the new `nl-jit-call-format-float' bridge added in the same
;; commit, which marshals the f64 magnitude in xmm0 + i64 conv-
;; codepoint + i64 precision + *mut Sexp out-buffer.
;;
;; Wrappers are installed via `fset' (= same Tier-1 substrate
;; pattern as `lisp/nelisp-jit-strategy.el') and validate
;; arity / type up-front so the trampolines can trust their inputs.
;; A trampoline ERR (= dotted-tail / shape mismatch surviving the
;; up-front checks) is re-signalled as `wrong-type-argument' on the
;; canonical predicate so existing call-site `condition-case'
;; handlers see the same shape as pre-§86.1.e Rust dispatch.

;;; Code:

;; `nl-jit-call-out-1' / `nl-jit-call-format-float' are Rust-side
;; bridge primitives installed by `install_builtins'.  No
;; `declare-function' is needed (= the NeLisp boot loader has no host
;; Emacs byte-compiler; the host-Emacs ELisp linter at compile-time
;; does flag the unbound names but `make compile' is run with
;; `byte-compile-error-on-warn t', so this file is intentionally
;; *not* in the host-Emacs `make compile' source set — the bake-image
;; pipeline reads it directly.)

;; ---------- nelisp--concat-ints -----------------------------------

(fset 'nelisp--concat-ints
      (lambda (lst)
        (cond
         ((null lst) "")
         ((consp lst)
          (condition-case _err
              (nl-jit-call-out-1 "nl_jit_concat_ints" lst)
            (error (signal 'wrong-type-argument (cons 'listp (cons lst nil))))))
         (t (signal 'wrong-type-argument (cons 'listp (cons lst nil)))))))

;; ---------- nelisp--make-mut-string -------------------------------

(fset 'nelisp--make-mut-string
      (lambda (n ch)
        (cond
         ((not (integerp n))
          (signal 'wrong-type-argument (cons 'integerp (cons n nil))))
         ((< n 0)
          (signal 'wrong-type-argument (cons 'natnump (cons n nil))))
         ((not (integerp ch))
          (signal 'wrong-type-argument (cons 'characterp (cons ch nil))))
         (t
          (condition-case _err
              (nl-jit-call-out-1 "nl_jit_make_mut_str" (cons n ch))
            (error (signal 'wrong-type-argument
                           (cons 'integerp (cons n nil)))))))))

;; ---------- nelisp--format-float-body -----------------------------

(fset 'nelisp--format-float-body
      (lambda (conv prec x)
        (cond
         ((not (integerp conv))
          (signal 'wrong-type-argument (cons 'integerp (cons conv nil))))
         ((not (integerp prec))
          (signal 'wrong-type-argument (cons 'integerp (cons prec nil))))
         ((< prec 0)
          (signal 'wrong-type-argument (cons 'natnump (cons prec nil))))
         ((not (or (floatp x) (integerp x)))
          (signal 'wrong-type-argument (cons 'numberp (cons x nil))))
         (t
          (let ((xf (if (integerp x) (+ x 0.0) x)))
            (condition-case _err
                (nl-jit-call-format-float "nl_jit_format_float" xf conv prec)
              (error (signal 'wrong-type-argument
                             (cons 'numberp (cons x nil))))))))))

;; No `(provide 'nelisp-stdlib-format)' — `provide' itself is defined
;; in `nelisp-stdlib-misc.el' which loads AFTER this file (= the load
;; order is established in `build-tool/src/eval/env.rs').  Other
;; pre-misc stdlib slices follow the same convention.

;;; nelisp-stdlib-format.el ends here
