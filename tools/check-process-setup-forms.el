;;; check-process-setup-forms.el --- Check emitted process setup forms -*- lexical-binding: t; -*-

(let* ((tool-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory (expand-file-name tool-file)))))
       (source-override (getenv "PROCESS_SETUP_SOURCE"))
       (source (expand-file-name
                (if (and source-override (not (string= source-override "")))
                    source-override
                  "scripts/nelisp-standalone-build.el")
                repo-root))
       (expected '(nl_bi_process_redirect_input
                   nl_bi_process_redirect_output
                   nl_bi_process_setup_child_fds
                   nl_bi_process_redirect_fd_path
                   nl_bi_process_setup_redirect_child))
       (form nil))
  (unless (file-readable-p source)
    (error "process setup source is not readable: %s" source))
  (with-temp-buffer
    (insert-file-contents source)
    (goto-char (point-min))
    (unless (search-forward
             "(defun nelisp-standalone--fileio-process-setup-forms" nil t)
      (error "setup forms generator not found in %s" source))
    (goto-char (match-beginning 0))
    (setq form (read (current-buffer))))
  (unless (and (listp form)
               (eq (car form) 'defun)
               (eq (cadr form) 'nelisp-standalone--fileio-process-setup-forms))
    (error "located expression is not the setup forms generator"))
  (eval form)
  (fset 'nelisp-standalone-arena-rewrite-target
        (lambda () 'linux-x86_64))
  (let* ((forms (nelisp-standalone--fileio-process-setup-forms))
         (names (mapcar (lambda (definition)
                          (unless (and (listp definition)
                                       (eq (car definition) 'defun))
                            (error "top-level setup item is not a defun: %S"
                                   (car-safe definition)))
                          (cadr definition))
                        forms)))
    (unless (equal names expected)
      (error "wrong top-level helper definitions: %S" names))
    (dolist (definition forms)
      (let ((todo (list (cdddr definition))))
        (while todo
          (let ((node (pop todo)))
            (when (consp node)
              (when (eq (car node) 'defun)
                (error "nested helper defun: %S in %S"
                       (cadr node) (cadr definition)))
              (when (memq (car node) '(let let*))
                (unless (and (>= (length node) 3)
                             (listp (cadr node)))
                  (error "malformed let binding/body in %S: %S"
                         (cadr definition) node)))
              (when (eq (car node) 'if)
                (unless (= (length node) 4)
                  (error "malformed if arity in %S: %S"
                         (cadr definition) node)))
              (push (car node) todo)
              (push (cdr node) todo))))))
    (princ (format "PROCESS-SETUP-FORMS-PASS count=%d names=%S source=%s\n"
                   (length names) names source))))

;;; check-process-setup-forms.el ends here
