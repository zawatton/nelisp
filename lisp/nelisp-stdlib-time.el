;;; nelisp-stdlib-time.el --- Doc 87 §86.1.f Tier 2 time wrappers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; Doc 87 §86.1.f Tier 2 wrappers — pure-elisp re-implementations of
;; `nl-current-unix-time' and `nl-format-unix-time' on top of the
;; `nl_jit_current_unix_time' / `nl_jit_format_unix_time' trampolines
;; in `build-tool/src/jit/time.rs'.  Replaces the deleted
;; `bi_nl_current_unix_time' / `bi_nl_format_unix_time' helpers in
;; `eval/builtins.rs'.
;;
;; Bridge primitives:
;;   - `nl-jit-call-i64-i64' (= `extern "C" fn(i64, i64) -> i64')
;;     used for the 0-arg current-unix-time entry; padding args
;;     ignored on the Rust side.
;;   - `nl-jit-call-out-2'  (= `extern "C" fn(*const Sexp, *const
;;     Sexp, *mut Sexp) -> i64') used for format-unix-time.

;;; Code:

;; Doc 87 §86.1.f Tier 2 wrapper.  0-arg primitive call — passes 2 i64
;; padding through `nl-jit-call-i64-i64' which the Rust trampoline
;; ignores.
(fset 'nl-current-unix-time
      (lambda ()
        (nl-jit-call-i64-i64 "nl_jit_current_unix_time" 0 0)))

;; Doc 87 §86.1.f Tier 2 wrapper.  2-arg out-Sexp primitive call.  The
;; trampoline writes the formatted Sexp::Str directly into the
;; out-slot; ERR surfaces as a generic `error' which we re-signal as
;; `wrong-type-argument' per the pre-§86.1.f contract.
(fset 'nl-format-unix-time
      (lambda (fmt epoch)
        (condition-case _err
            (nl-jit-call-out-2 "nl_jit_format_unix_time" fmt epoch)
          (error
           (nelisp--signal-wrong-type
            'string-or-integer
            (cons fmt epoch))))))

;;; nelisp-stdlib-time.el ends here
