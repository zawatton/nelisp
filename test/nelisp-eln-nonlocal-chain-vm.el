;;; nelisp-eln-nonlocal-chain-vm.el --- Doc 207 VM frames for S4.6 -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Ledger S4.6 (Doc 207).  Source of the VM frames interleaved with the
;; genuine GNU native frames by test/nelisp-eln-nonlocal-chain-driver.el.
;; The smoke compiles every function here with host GNU Emacs 31.1 and
;; hands the resulting byte-code objects to both runtimes, so the frames
;; above and below each native frame run in a byte-code VM on each side.
;;
;; Each frame binds the special variable `s46-depth', which must still be
;; bound while its own cleanup runs and restored once the frame is left, and
;; records its cleanup in `s46-log'.  `s46-vm-outer' puts its handler and
;; its cleanup in the same function, which is the arrangement a VM must
;; unwind in GNU order: handler found first, enclosing cleanup run once.

;;; Code:

(defvar s46-depth 0)
(defvar s46-log nil)
(defvar s46-mode nil)
(defvar s46-tag nil)
(defvar s46-inner-g nil)
(defvar s46-leaf-calls 0)

(defun s46-vm-leaf (y)
  "Innermost VM frame, reached from a native `Ffuncall' hop with Y."
  (setq s46-leaf-calls (1+ s46-leaf-calls))
  (let ((s46-depth (1+ s46-depth)))
    (unwind-protect
        (cond ((eq s46-mode 'return) (* y 10))
              ((eq s46-mode 'return-gc)
               (garbage-collect)
               (* y 10))
              ((eq s46-mode 'error) (signal 's46-error (list y s46-depth)))
              ((eq s46-mode 'throw) (throw s46-tag (list 'thrown y s46-depth)))
              ((eq s46-mode 'quit) (signal 'quit (list y)))
              (t (error "Unknown s46 mode %S" s46-mode)))
      (push (list 'leaf s46-depth) s46-log))))

(defun s46-vm-mid (y)
  "VM frame between two native frames; call the inner native chain with Y.
Its handler names a condition nothing raises, so every exit passes it."
  (let ((s46-depth (1+ s46-depth)))
    (unwind-protect
        (condition-case err
            (1+ (nelisp-gnu-chain s46-inner-g y))
          (s46-unraised (list 'mid-caught err)))
      (push (list 'mid s46-depth) s46-log))))

(defun s46-vm-outer (g x)
  "Outermost VM frame: call the native chain with G and X.
The handler and the cleanup share this function."
  (let ((s46-depth (1+ s46-depth)))
    (unwind-protect
        (condition-case err
            (list 'value (nelisp-gnu-chain g x))
          (s46-error (list 'caught-in-outer err s46-depth)))
      (push (list 'outer s46-depth) s46-log))))

(provide 'nelisp-eln-nonlocal-chain-vm)

;;; nelisp-eln-nonlocal-chain-vm.el ends here
