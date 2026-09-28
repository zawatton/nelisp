;;; nelisp-eln-symbol-base-lease-smoke.el --- Symbol-base lease smoke -*- lexical-binding: t; -*-

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root (or (getenv "NELISP_ROOT")
                 (file-name-directory test-dir))))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (add-to-list 'load-path (expand-file-name "packages/nl-ffi/src" root))
  (require 'nelisp-eln-objects))

(let* ((unit (nelisp-eln-objects-create))
       (first-token (nelisp-eln-objects-symbol-base-acquire))
       (second-token (nelisp-eln-objects-symbol-base-acquire))
       (base nelisp-eln-objects--symbol-base)
       (pair (nelisp-eln-objects-make-uninterned-symbol
              unit "base-lease-smoke-symbol")))
  (unless (and (symbolp (car pair)) (integerp (cdr pair))
               (aref (nelisp-eln-objects--resolve unit) 6))
    (error "smoke did not create a real unit symbol record"))
  (nelisp-eln-objects-release unit)
  (unless (eq base nelisp-eln-objects--symbol-base)
    (error "unit cleanup released a base pinned by an explicit lease"))
  (unless (eq nelisp-eln-objects--registry-state 'open)
    (error "unit cleanup poisoned the object registry"))
  (nelisp-eln-objects-symbol-base-release first-token)
  (unless (eq base nelisp-eln-objects--symbol-base)
    (error "releasing one of two leases released the shared base"))
  (nelisp-eln-objects-symbol-base-release second-token)
  (when nelisp-eln-objects--symbol-base
    (error "last explicit lease did not release the unused base"))
  (unless (condition-case nil
              (progn (nelisp-eln-objects-symbol-base-release second-token) nil)
            (nelisp-eln-objects-error t))
    (error "double release was not rejected with the typed condition")))

(princ "symbol-base-lease-smoke: PASS\n")
;;; nelisp-eln-symbol-base-lease-smoke.el ends here
