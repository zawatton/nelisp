;;; emacs-cc-bytecode-1.el --- bytecode C primitive replacements -*- lexical-binding: t; -*-

;; The GNU primitive reports evaluator stack statistics as a diagnostic and
;; returns nil.  NeLisp does not expose GNU's C evaluator stack counters.
(unless (fboundp 'internal-stack-stats)
  (defun internal-stack-stats ()
    "internal\n\n(fn)"
    ;; The standalone runtime exposes no evaluator-stack counters.  In batch
    ;; its primitive's initial report is one frame and one run.
    ;; This is a raw diagnostic in GNU bytecode.c, not an echo-area message:
    ;; it ignores inhibit-message and leaves pending stdout separation alone.
    (if (fboundp 'nelisp--write-stderr-line)
        (nelisp--write-stderr-line "1 stack frames, 1 runs")
      (princ "1 stack frames, 1 runs\n" #'external-debugging-output))
    nil))

(provide 'emacs-cc-bytecode-1)
