;;; standalone-abi-intern-thread-check.el -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(load (expand-file-name "tools/nelisp-thread-mirror-guard-smoke.el") nil t)
(unless (= (nl-thread-mirror-guard--run
            '(if (and (eq (intern "nil") nil) (eq (intern "t") t)) 31 -31)) 31)
  (error "worker canonical-name intern changed"))
(princ "ABI-INTERN-THREAD checked=2 PASS\n")
