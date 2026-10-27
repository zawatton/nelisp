;;; nc4-harvest.el --- harvest nc4 rows on a pre-nc4 binary  -*- lexical-binding: t; -*-
;; usage (repository root): NC1_NAMES=FILE NC1_OUT=FILE NC1_ID0=N [NC1_CONST=NAME] BINARY --load tools/ai/nc4-harvest.el
;; The nc4 compiler hot patch (tools/ai/nc4-hotpatch.el) for the listed
;; names; the regexp entry points `nelisp-prelude-bytecode-fx-names' defined
;; from lisp/nelisp-stdlib-regexp.el as the lexical closures the nc4 build
;; keeps (the binary holds them as byte code); test/nelisp-nc4-fx.el loaded
;; for the corpus rows.  Then tools/ai/nc1-harvest.el.
(load (expand-file-name "tools/ai/nc4-hotpatch.el") nil t)
(load (expand-file-name "test/nelisp-nc4-fx.el") nil t)
(let ((want '(nlre--string-match-entry nlre--string-match-p-entry nlre-string-match
              nlre--nat-sm nlre--nat-program nlre--nat-fold-default-p nlre--nat-run
              nlre--nat-publish nlre-match-beginning nlre-match-end nlre--match-string-entry)))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "lisp/nelisp-stdlib-regexp.el"))
    (goto-char (point-min))
    (let ((form t))
      (while (setq form (condition-case nil (read (current-buffer)) (end-of-file nil)))
        (when (and (eq (car-safe form) 'defun) (memq (nth 1 form) want))
          (eval form t))))))
;; `current-buffer' (kept as source too) from the prelude, where it sits
;; under `(unless (fboundp ...))'.
(with-temp-buffer
  (insert-file-contents (expand-file-name "scripts/nelisp-stdlib-prelude.el"))
  (goto-char (point-min))
  (let ((form t) (done nil))
    (while (and (not done) (setq form (condition-case nil (read (current-buffer)) (end-of-file nil))))
      (when (and (eq (car-safe form) 'unless) (eq (car-safe (nth 2 form)) 'defun)
                 (eq (nth 1 (nth 2 form)) 'current-buffer))
        (eval (nth 2 form) t) (setq done t)))))
(setq nc4--hot (with-temp-buffer (insert-file-contents (getenv "NC1_NAMES"))
                                 (mapcar #'intern (split-string (buffer-string) "\n" t))))
;; A record the image's compiler declined (`none') is compiled again.
(dolist (n nc4--hot)
  (let ((f (and (fboundp n) (symbol-function n))))
    (when (and (eq (car-safe f) 'closure) (eq (gethash (nthcdr 3 f) nelisp--fx-table) 'none))
      (remhash (nthcdr 3 f) nelisp--fx-table))))
(load (expand-file-name "tools/ai/nc1-harvest.el") nil t)
