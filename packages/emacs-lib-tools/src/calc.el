;;; calc.el --- Tier 3 Calc facade loader  -*- lexical-binding: t; -*-

;;; Code:

(declare-function emacs-calc-install "emacs-calc")

(defun calc--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader.
`emacs-version' is bound under nemacs too, so also probe for reader
primitives (mirrors `files--standalone-runtime-p')."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(if (calc--standalone-runtime-p)
    ;; Standalone reader: install the minimal RPN calculator.
    (progn
      (require 'emacs-calc)
      (emacs-calc-install))
  ;; Host Emacs / ERT: keep the Tier 3 `unsupported' stub surface so
  ;; `(require 'calc)' satisfies the facade contract.
  (require 'emacs-tier3-facades))

(provide 'calc)

;;; calc.el ends here
