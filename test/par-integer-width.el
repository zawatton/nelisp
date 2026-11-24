;;; -*- lexical-binding: t; -*-
;; The startup value must allow exponentiation in package initializers.
(prin1 (list integer-width
             (condition-case err (expt 2 28) (error err))
             (condition-case err (expt 2 63) (error err))))
(terpri)
