;;; search.el --- Incremental search, query replace and Occur -*- lexical-binding: t; -*-

(defun smoke-search ()
  (let ((source (generate-new-buffer " *smoke-search*")))
    (switch-to-buffer source)
    (insert "foo x12\nneedle x34\nneedle end\n")
    (smoke-step "isearch"
      (require 'isearch)
      (goto-char (point-min))
      (execute-kbd-macro (kbd "C-s n e e d l e C-s RET"))
      (list (key-binding (kbd "C-s")) (point) isearch-string))
    (smoke-step "query-replace"
      (require 'replace)
      (with-current-buffer source
        (goto-char (point-min))
        (let ((unread-command-events '(?!)))
          (query-replace-regexp "x\\([0-9]+\\)" "id-\\1"))
        (smoke-state)))
    (smoke-step "occur"
      (with-current-buffer source
        (occur "needle"))
      (with-current-buffer "*Occur*"
        (list major-mode (smoke-matching-lines "^[[:space:]]*[0-9]+:"))))))
