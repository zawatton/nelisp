;;; vc.el --- VC facade loader for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(declare-function emacs-vc-install "emacs-vc")

(defun vc--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader.
`emacs-version' is bound under nemacs too, so also probe for reader
primitives (mirrors `files--standalone-runtime-p')."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(if (vc--standalone-runtime-p)
    ;; Standalone reader: install the read-only `emacs-vc' family directly
    ;; (vc-diff / vc-print-log / vc-dir / vc-annotate).  The Tier 3 stub
    ;; surface (vc-next-action, vc-register, ...) is a host facade contract;
    ;; the reader only needs the callable read-only commands, and bundling
    ;; `emacs-tier3-facades' there would shadow the real `info' / `url-retrieve'
    ;; the reader already provides.
    (progn
      (require 'emacs-vc)
      (emacs-vc-install))
  ;; Host Emacs / ERT: provide the full Tier 3 `unsupported' stub surface so
  ;; `(require 'vc)' satisfies the facade contract.  The read-only family stays
  ;; stubbed under host until a workflow installs `emacs-vc' explicitly.
  (require 'emacs-tier3-facades))

(provide 'vc)

;;; vc.el ends here
