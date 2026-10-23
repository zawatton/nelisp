;;; dispatch-probe-compile.el --- compile the dispatch probes with the tree's own x86-64 emitter  -*- lexical-binding: t; -*-
;; usage, from a worktree root:
;;   emacs --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
;;         -l tools/ai/dispatch-probe-compile.el SRC BLOB SYMS
;; SRC holds one form (seq (defun ...) ...) as tools/ai/dispatch-probe-gen.py writes it; BLOB gets the
;; unit's text bytes (position independent: the probes call nothing and a jump table is rip-relative),
;; SYMS "name offset" lines.  The compiler is the tree's own, so a tree with the dense-equality-chain
;; jump-table lowering (nelisp-aot-compiler--x86-dense-if-plan) builds the table and one without does not.
(let* ((file (nth 0 command-line-args-left))
       (out (nth 1 command-line-args-left))
       (symout (nth 2 command-line-args-left))
       (form (with-temp-buffer (insert-file-contents file) (goto-char (point-min)) (read (current-buffer))))
       (unit (nelisp-standalone--compile-to-unit "probe" form))
       (text (cdr (assq 'text (plist-get unit :sections))))
       (syms (plist-get unit :symbols)))
  (setq command-line-args-left nil)
  (let ((coding-system-for-write 'binary))
    (with-temp-file out
      (set-buffer-multibyte nil)
      (insert (if (stringp text) text (apply #'unibyte-string (append text nil))))))
  (with-temp-file symout
    (dolist (s syms)
      (insert (format "%s %d\n" (plist-get s :name) (plist-get s :value)))))
  (message "PROBE text=%d bytes, %d symbols, jump-tables=%s" (length text) (length syms)
           nelisp-aot-compiler--x86-jump-tables-enabled))
