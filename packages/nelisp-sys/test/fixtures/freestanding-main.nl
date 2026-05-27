;;; freestanding-main.nl --- nelisp-sys fixture: freestanding entry (deferred) -*- lexical-binding: t; -*-
;; Freestanding executable mode is deferred (Doc 132 stage 132.7).  This
;; fixture documents the intended shape; it is exercised only by the
;; experimental freestanding gate, not the MVP suite.
(sys:defun _start () void
  (:abi nelisp-internal :alloc none :panic abort :syscall may)
  (sys:exit 0))
