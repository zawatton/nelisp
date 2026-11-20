;;; nelisp-elisp-mode-standalone.el --- genuine emacs-lisp-mode -*- lexical-binding: t; -*-
;; Runs unchanged on GNU Emacs 31.1 and on the NeLisp runtime image (load it
;; with `exec-runtime-image'); the stdout streams must be byte-identical.
;; Covers what the stub `emacs-lisp-mode' lacked: GNU lisp-mode.el's indent
;; functions and font-lock defaults (via GNU elisp-mode.el).
(defmacro elm-case (name form)
  `(princ (format "ELM %s %S\n" ,name (condition-case e ,form (error (list 'err e))))))
(defmacro elm-in-mode (&rest body)
  `(with-temp-buffer (emacs-lisp-mode) ,@body))
(elm-case "major-mode" (elm-in-mode major-mode))
(elm-case "indent-line-function" (elm-in-mode indent-line-function))
(elm-case "parent-mode" (get 'emacs-lisp-mode 'derived-mode-parent))
(elm-case "calculate-lisp-indent"
  (elm-in-mode (insert "(defun f ()\n(+ 1 2))") (goto-char 13) (calculate-lisp-indent)))
(elm-case "indent-region"
  (elm-in-mode (insert "(defun f ()\n(+ 1 2))")
               (let ((inhibit-message t)) (indent-region (point-min) (point-max)))
               (buffer-string)))
(elm-case "indent-region-let"
  (elm-in-mode (insert "(let ((a 1)\n(b 2))\n(list a\nb))")
               (let ((inhibit-message t)) (indent-region (point-min) (point-max)))
               (buffer-string)))
(elm-case "indent-funcall-nil"
  (with-temp-buffer (insert "x") (let ((indent-line-function nil)) (indent-according-to-mode t))))
(elm-case "font-lock-keyword"
  (elm-in-mode (insert "(defun foo () \"doc\")") (font-lock-ensure)
               (list (get-text-property 2 'face) (get-text-property 8 'face)
                     (get-text-property 15 'face))))
(elm-case "font-lock-comment"
  (elm-in-mode (insert "; note\n(setq x 1)") (font-lock-ensure)
               (list (get-text-property 1 'face) (get-text-property 9 'face))))
(princ "ELM-DONE\n")
