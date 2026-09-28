;;; nelisp-eln-objects-numeric-standalone-smoke.el --- numeric graph smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(let* ((directory (file-name-directory load-file-name))
       (root (or (getenv "NELISP_ROOT")
                 (expand-file-name ".." directory)))
       (objects-file (expand-file-name "../lisp/nelisp-eln-objects.el"
                                        directory)))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (load objects-file nil t))

(let* ((large (ash 1 130))
       (fraction 1.25)
       (cell (cons large nil))
       (first (nelisp-eln-objects-create))
       (second (nelisp-eln-objects-create))
       (cons-word (nelisp-eln-objects-encode first cell))
       (float-word (nelisp-eln-objects-encode first fraction))
       (shared-word (nelisp-eln-objects-encode second large))
       (activation (nelisp-eln-objects-activation-acquire first)))
  (unwind-protect
      (progn
        (unless (eq (nelisp-eln-objects-decode first cons-word) cell)
          (error "cons identity mismatch"))
        (unless (eq (nelisp-eln-objects-decode
                     first (nelisp-eln-objects--read-word (- cons-word 3) 0))
                    large)
          (error "nested bignum identity mismatch"))
        (unless (= (nelisp-eln-objects--read-word (- cons-word 3) 0)
                   shared-word)
          (error "cross-owner bignum identity mismatch"))
        (unless (eq (nelisp-eln-objects-decode first float-word) fraction)
          (error "float identity mismatch"))
        (garbage-collect)
        (nelisp-eln-objects-release first)
        (unless (eq (nelisp-eln-objects-activation-decode activation shared-word)
                    large)
          (error "activation bignum lease mismatch"))
        (unless (eq (nelisp-eln-objects-activation-decode activation float-word)
                    fraction)
          (error "activation float lease mismatch"))
        (nelisp-eln-objects-release second)
        (nelisp-eln-objects-activation-release activation)
        (condition-case nil
            (progn
              (nelisp-eln-objects-activation-decode activation shared-word)
              (error "closed activation unexpectedly decoded a number"))
          (nelisp-eln-objects-error nil))
        (princ "numeric-object-graph standalone smoke PASS\n"))
    (when (assq activation nelisp-eln-objects--activations)
      (nelisp-eln-objects-activation-release activation))
    (when (assq first nelisp-eln-objects--live-units)
      (nelisp-eln-objects-release first))
    (when (assq second nelisp-eln-objects--live-units)
      (nelisp-eln-objects-release second))))
