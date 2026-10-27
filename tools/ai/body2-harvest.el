;;; body2-harvest.el --- harvest fxir rows for the expand-file-name family  -*- lexical-binding: t; -*-
;; usage (repository root): NC1_NAMES=FILE NC1_OUT=FILE NC1_ID0=N BINARY --load tools/ai/body2-harvest.el
;; The binary holds these defuns as byte code (prelude byte-code pass); `nelisp-prelude-bytecode-fx-names' keeps them as
;; source in the body2 build.  Here they are defined again from scripts/nelisp-stdlib-prelude.el as lexical closures,
;; exactly as tools/ai/nc4-harvest.el does for the regexp entry points, then tools/ai/nc1-harvest.el writes the rows.
(load (expand-file-name "test/nelisp-fxir-eval.el") nil t)
(let ((want (with-temp-buffer (insert-file-contents (getenv "NC1_NAMES"))
                              (mapcar #'intern (split-string (buffer-string) "\n" t)))))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "scripts/nelisp-stdlib-prelude.el"))
    (goto-char (point-min))
    (let ((form t))
      (while (setq form (condition-case nil (read (current-buffer)) (end-of-file nil)))
        (when (and (eq (car-safe form) 'defun) (memq (nth 1 form) want))
          (eval form t))))))
(load (expand-file-name "tools/ai/nc1-harvest.el") nil t)
