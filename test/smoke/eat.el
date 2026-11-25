;;; eat.el --- Shell output via the Eat terminal -*- lexical-binding: t; -*-

(defun smoke-terminal-result (buffer)
  (let* ((process (get-buffer-process buffer))
         (status (and process (smoke-await-process process))))
    (with-current-buffer buffer
      (let ((lines (smoke-matching-lines "^smoke-terminal")))
        (unless lines
          (error "Terminal output missing"))
        (when (and status (/= status 0))
          (error "Terminal exited with status %d" status))
        (list major-mode lines)))))

(defun smoke-eat ()
  (unless (and (smoke-step "eat-load" (require 'eat))
               (smoke-step "eat-output"
                 (let ((eat-kill-buffer-on-exit nil)
                       (eat-query-before-killing-running-terminal nil))
                   (smoke-terminal-result (eat "echo smoke-terminal" t)))))
    (smoke-step "term-fallback"
      (require 'term)
      (let ((buffer (make-term "smoke-term" "/bin/sh" nil "-c" "echo smoke-terminal")))
        (with-current-buffer buffer (term-mode))
        (smoke-terminal-result buffer)))))
