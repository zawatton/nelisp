;;; weak-word-probe.el --- Extract the existing weak oracle slice -*- lexical-binding: t; -*-

;; Generate a standalone script without editing or duplicating the oracle:
;; emacs -Q --batch -l tools/ai/weak-word-probe.el > target/weak-word-probe.el
;; Run that script with GNU Emacs or NeLisp. All 24 weak rows, including
;; forward/reverse and mixed chains, retain their original assertions.

(let* ((directory (file-name-directory (or load-file-name buffer-file-name)))
       (source (expand-file-name "eqhash-semantics.el" directory))
       (helpers '(eqhash--emit eqhash--case eqhash--entries eqhash--collect))
       (count 0) (blocks 0) forms)
  (with-temp-buffer
    (insert-file-contents source)
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (cond
             ((and (memq (car-safe form) '(defun defmacro))
                   (memq (cadr form) helpers))
              (push form forms) (setq count (1+ count)))
             ((and (eq (car-safe form) 'unless)
                   (eq (cadr form) 'eqhash--ordinary-only))
              (push form forms) (setq blocks (1+ blocks))))))
      (end-of-file nil)))
  (unless (and (= count 4) (= blocks 1))
    (error "Oracle structure changed: %s helpers, %s weak blocks" count blocks))
  (princ ";;; Generated weak oracle slice -*- lexical-binding: t; -*-\n")
  (prin1 '(setq eqhash--count 0 eqhash--weak-state nil eqhash--ordinary-only nil))
  (terpri)
  (dolist (form (nreverse forms)) (prin1 form) (terpri))
  (prin1 '(progn (prin1 (list 'eqhash-complete eqhash--count)) (terpri)))
  (terpri)
  (prin1 '(kill-emacs 0))
  (terpri))

;;; weak-word-probe.el ends here
