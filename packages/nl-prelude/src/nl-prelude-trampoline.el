;;; nl-prelude-trampoline.el --- Explicit mutual-recursion trampoline -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 198 Phase 2.  `nl-loop'/`nl-recur' already provide a compile-time
;; self-tail rewrite.  This file covers the separate mutual-recursion gap:
;; cooperating functions return an `nl-bounce' request and one outer
;; `nl-trampoline' loop dispatches each request without growing the stack.
;;
;; This is deliberately opt-in and syntactic.  A direct call still consumes
;; a normal stack frame, and calling a bounce-producing function without
;; `nl-trampoline' returns the raw tagged request.

;;; Code:

(defmacro nl-bounce (target &rest args)
  "Return a tagged request for TARGET to be called with ARGS.
Use this in tail position in a cooperating function reached through
`nl-trampoline'.  TARGET and every argument are evaluated once when the
request is made; the call itself happens in `nl-trampoline''s driver frame."
  `(list 'nl--bounce ,target (list ,@args)))

(defun nl-trampoline (target &rest args)
  "Call TARGET with ARGS, dispatching every returned `nl-bounce' request.
Return the first value that is not a tagged bounce.  Cooperating direct and
mutually recursive functions therefore run in constant evaluator/native
stack, provided every recursive hop is explicitly rewritten to `nl-bounce'."
  (let ((result (apply target args)))
    (while (and (consp result) (eq (car result) 'nl--bounce))
      (setq result (apply (cadr result) (caddr result))))
    result))

(provide 'nl-prelude-trampoline)

;;; nl-prelude-trampoline.el ends here
