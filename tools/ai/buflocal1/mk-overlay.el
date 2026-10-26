;;; mk-overlay.el --- extract the buflocal1 definitions from the prelude into an overlay  -*- lexical-binding: t; -*-
;; usage: emacs --batch -l mk-overlay.el --eval '(buflocal1-make-overlay "OUT.el")'
(defvar buflocal1-names
  '(setq-default setq-local defvar-local with-current-buffer save-restriction with-restriction
    without-restriction nelisp--hook-alist-set nelisp--buffer-arg nelisp--live-buffer-arg
    nelisp--buffer-string-whole nelisp--buffer-narrowed-p nelisp--save-restriction
    nelisp--restore-restriction nelisp--insert-buffer-substring nelisp-buffer--detach-markers
    nelisp-kill-buffer kill-buffer-hook buffer-list-update-hook
    nelisp-buffer-local--serial nelisp-buffer-local--free-cells nelisp-buffer-local--owned
    nelisp-buffer-local--new-cell nelisp-buffer-local--note nelisp-buffer-local--free
    nelisp-buffer-local--release nelisp-buffer-local--kill-buffer-locals
    kill-all-local-variables make-variable-buffer-local local-variable-if-set-p
    buffer-local-variables buffer-local-value buffer-local-boundp add-hook remove-hook
    nelisp--hook-list-p nelisp--run-hook-call nelisp--run-hook-value run-hooks
    run-hook-with-args run-hook-with-args-until-success run-hook-with-args-until-failure
    buffer-string char-before delete-region kill-buffer other-buffer set-buffer count-lines
    buffer-narrowed-p buffer-end pos-bol pos-eol backward-char backward-word
    delete-and-extract-region filter-buffer-substring insert-buffer-substring
    insert-buffer-substring-no-properties buffer-list rename-buffer line-end-position narrow-to-region widen bufferp buffer-live-p nelisp-buffer--new-mode nelisp-buffer--new-fast nelisp-buffer--new nelisp-generate-new-buffer nelisp-get-buffer-create))
(defun buflocal1--read-forms (file)
  (with-temp-buffer
    (insert-file-contents (expand-file-name file default-directory))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil (while t (push (read (current-buffer)) forms)) (end-of-file nil))
      (nreverse forms))))
(defun buflocal1-make-overlay (out)
  (let ((seen nil) (forms nil) (prov nil))
    (dolist (form (buflocal1--read-forms "scripts/nelisp-stdlib-prelude.el"))
      (let ((head (car-safe form)) name guarded)
        (cond
         ((and (eq head 'unless) (equal (car-safe (nth 1 form)) 'fboundp))
          (setq name (cadr (cadr (nth 1 form))) guarded t))
         ((memq head '(defun defmacro defvar)) (setq name (nth 1 form))))
        (when (and name (memq name buflocal1-names) (not (memq name seen)))
          (push name seen)
          (push (if guarded `(progn (fmakunbound ',name) ,(nth 2 form)) form) forms))))
    (dolist (form (buflocal1--read-forms "lisp/nelisp-buffer-local.el"))
      (when (and (eq (car form) 'defun)
                 (memq (cadr form) '(nelisp-buffer-local-make nelisp-buffer-local-kill)))
        (push form prov)))
    (with-temp-file out
      (insert ";;; -*- lexical-binding: t; -*-\n")
      (let ((print-length nil) (print-level nil))
        (dolist (f (append (nreverse forms) (nreverse prov)))
          (prin1 f (current-buffer)) (insert "\n"))))
    (message "overlay: %d forms; missing: %S" (length forms)
             (seq-remove (lambda (n) (memq n seen)) buflocal1-names))))
