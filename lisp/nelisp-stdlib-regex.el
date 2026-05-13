;;; nelisp-stdlib-regex.el --- Doc 87 §86.1.f Tier 2 regex wrapper  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; Doc 87 §86.1.f Tier 2 wrapper — pure-elisp re-implementation of
;; `string-match-p' on top of the `nl_jit_string_match_p' trampoline
;; in `build-tool/src/jit/regex.rs'.  Replaces the deleted
;; `bi_string_match_p' helper in `eval/builtins.rs'.
;;
;; The trampoline returns Sexp::T / Sexp::Nil directly, so the wrapper
;; only re-signals `wrong-type-argument' on the narrow ERR case
;; (= non-string PATTERN or TEXT).

;;; Code:

;; Doc 87 §86.1.f Tier 2 wrapper.  2-arg out-Sexp primitive call.
(fset 'string-match-p
      (lambda (pat text)
        (condition-case _err
            (nl-jit-call-out-2 "nl_jit_string_match_p" pat text)
          (error
           (nelisp--signal-wrong-type
            'stringp (cons pat text))))))

;;; nelisp-stdlib-regex.el ends here
