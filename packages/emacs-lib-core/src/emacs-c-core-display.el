;;; emacs-c-core-display.el --- GNU Emacs C primitives in Lisp (display)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Lisp implementations of GNU Emacs 31.1 C primitives for the NeLisp
;; runtime, area "display" of tools/ai/c-core-progress.org.  Each definition is
;; guarded so a native or earlier implementation is never shadowed, and
;; follows GNU batch semantics (checked by test/c-core-*-parity-smoke.sh
;; against host `emacs --batch').

;;; Code:

(provide 'emacs-c-core-display)

;;; emacs-c-core-display.el ends here
