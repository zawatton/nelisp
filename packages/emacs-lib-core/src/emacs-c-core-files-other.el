;;; emacs-c-core-files-other.el --- GNU Emacs C primitives in Lisp (files-other)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Lisp implementations of GNU Emacs 31.1 C primitives for the NeLisp
;; runtime, area "files-other" of tools/ai/c-core-progress.org.  Each definition is
;; guarded so a native or earlier implementation is never shadowed, and
;; follows GNU batch semantics (checked by test/c-core-*-parity-smoke.sh
;; against host `emacs --batch').

;;; Code:

(provide 'emacs-c-core-files-other)

;;; emacs-c-core-files-other.el ends here
