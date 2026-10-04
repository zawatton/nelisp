;;; -*- lexical-binding: t; -*-
;; Special-form reflection is an evaluator contract, independent of the
;; builtin metadata accessors when passed an actual builtin object.
(symbol-function
 (subr-name (symbol-function 'if)))
