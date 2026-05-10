;;; nelisp-stdlib-math.el --- Doc 87 §86.1.f Tier 2 math wrappers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; Doc 87 §86.1.f Tier 2 wrappers — pure-elisp re-implementations of
;; `float' / `exp' / `log' on top of the new `:trampoline-unary-float'
;; ABI mode (= `nl-jit-call-float-unary' bridge primitive, Rust
;; trampolines in `build-tool/src/jit/math.rs').  Replaces the deleted
;; `bi_float' / `bi_exp' / `bi_log' helpers in `eval/builtins.rs'.
;;
;; The 2-arg form of `log' (= `(log X BASE)') is decomposed at the
;; elisp level: `(log X BASE) = (/ (log X) (log BASE))', so the Rust
;; ABI stays at exactly `fn(f64) -> f64' which keeps the encoder
;; footprint zero.

;;; Code:

;; Doc 87 §86.1.f Tier 2 wrapper.  Uses new `:trampoline-unary-float'
;; ABI mode (= xmm0 → xmm0 calling convention).
(fset 'float
      (lambda (x)
        (nl-jit-call-float-unary "nl_jit_float_float" x)))

(fset 'exp
      (lambda (x)
        (nl-jit-call-float-unary "nl_jit_float_exp" x)))

;; `log' optionally takes a 2nd BASE arg (defaults to e).  The
;; trampoline is 1-arg only, so we compute `(/ (log X) (log BASE))'
;; in elisp when BASE is supplied.
(fset 'log
      (lambda (x &optional base)
        (let ((lx (nl-jit-call-float-unary "nl_jit_float_log" x)))
          (if base
              (/ lx (nl-jit-call-float-unary "nl_jit_float_log" base))
            lx))))

;;; nelisp-stdlib-math.el ends here
