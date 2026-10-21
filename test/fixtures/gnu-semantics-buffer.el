;;; gnu-semantics-buffer.el --- GNU buffer unwind fixture -*- lexical-binding: t; -*-
(defun gs-buffer-row (name thunk)
  (prin1 (list name (condition-case e (funcall thunk) (error e)))) (terpri))
(let ((original (current-buffer))
      (a (get-buffer-create "gs-buffer-a"))
      (b (get-buffer-create "gs-buffer-b")))
  (unwind-protect
      (progn
        (set-buffer a)
        (gs-buffer-row 'empty (lambda () (save-current-buffer)))
        (gs-buffer-row 'value (lambda () (save-current-buffer 1 2)))
        (gs-buffer-row 'normal
          (lambda () (list (save-current-buffer (set-buffer b) 'body)
                           (buffer-name (current-buffer)))))
        (set-buffer a)
        (gs-buffer-row 'throw
          (lambda () (list (catch 'done (save-current-buffer (set-buffer b) (throw 'done 7)))
                           (buffer-name (current-buffer)))))
        (set-buffer a)
        (gs-buffer-row 'signal
          (lambda () (list (condition-case e
                              (save-current-buffer (set-buffer b) (signal 'error '("unwind")))
                            (error e))
                           (buffer-name (current-buffer)))))
        (set-buffer a)
        (gs-buffer-row 'nested
          (lambda () (list (save-current-buffer
                            (set-buffer b)
                            (save-current-buffer (set-buffer a))
                            (buffer-name (current-buffer)))
                           (buffer-name (current-buffer)))))
        (set-buffer a)
        (erase-buffer) (insert "abcdef") (goto-char 2)
        (gs-buffer-row 'point-not-restored
          (lambda () (save-current-buffer (goto-char 5)) (point)))
        (gs-buffer-row 'alias
          (lambda () (fset 'gs-buffer-alias (symbol-function 'save-current-buffer))
            (list (special-form-p 'gs-buffer-alias)
                  (gs-buffer-alias (set-buffer b) 9)
                  (buffer-name (current-buffer)))))
        (set-buffer a)
        (gs-buffer-row 'killed
          (lambda () (list (save-current-buffer (set-buffer b) (kill-buffer a) 'killed)
                           (buffer-name (current-buffer))))))
    (set-buffer original)
    (when (buffer-live-p a) (kill-buffer a))
    (when (buffer-live-p b) (kill-buffer b))))
(kill-emacs 0)
