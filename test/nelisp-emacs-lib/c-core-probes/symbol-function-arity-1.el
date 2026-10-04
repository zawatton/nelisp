;;; symbol-function-arity-1.el --- Keep evaluator fast-path arity failures visible -*- lexical-binding: t; -*-

(symbol-function
 (condition-case err (symbol-function) (error err))
 (condition-case err (symbol-function nil t) (error err))
 (not (null (symbol-function 'car)))
 (symbol-function nil)
 (symbol-function t)
 (condition-case err (symbol-function 42) (error err))
 (condition-case err (symbol-function "car") (error err)))
