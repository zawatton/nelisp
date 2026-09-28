;; S6.14 corpus for the byte-compile-funcall.wrapper.el entry point.
;; Args are (FORM): a normal call, and the zero-argument arity-error path
;; (rewritten to a `signal' call rather than a Lisp error, see README.md).
(((funcall 'foo 1 2))
 ((funcall)))
