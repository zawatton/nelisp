;;; wasm-target-init-oracle.el --- Isolated seed generator -*- lexical-binding: t; -*-
(require 'cl-lib)
(defvar nelisp-standalone--target)
(defun wasm-target-init-oracle (file size)
  "Call the genuine generator under the explicit Wasm target."
  (condition-case err
      (progn
        (load (file-name-sans-extension file) nil t)
        (let* ((nelisp-standalone--target 'wasm32)
               (forms (nelisp-standalone--arena-init-metadata-forms-dynamic 'base size))
               (names '(nelisp-standalone--arena-init-metadata-forms-dynamic
                        nl_freelist_small_mask_ptr))
               definitions data-start-definition)
          (with-temp-buffer
            (emacs-lisp-mode)
            (insert-file-contents file)
            (goto-char (point-min))
            (while (progn (forward-comment (point-max)) (not (eobp)))
              (let ((form (read (current-buffer))))
                (when (and (eq (car-safe form) 'defconst)
                           (eq (cadr form) 'nelisp-standalone--arena-data-start-offset))
                  (setq data-start-definition form))
                (when (and (eq (car-safe form) 'defun) (memq (cadr form) names))
                  (push form definitions))
                ;; The pointer helper is inside a quoted runtime source.
                (cl-labels ((walk (tree)
                              (when (consp tree)
                                (when (and (eq (car tree) 'defun)
                                           (eq (cadr tree) 'nl_freelist_small_mask_ptr))
                                  (push tree definitions))
                                (walk (car tree)) (walk (cdr tree)))))
                  (unless (eq (cadr form) 'nl_freelist_small_mask_ptr) (walk form))))))
          (unless (and (= (length definitions) 2)
                       (member '(defun nl_freelist_small_mask_ptr ()
                                  (data-addr nl_freelist_small_mask)) definitions))
            (error "Unresolved mask provider"))
          (unless (and (integerp (nth 2 data-start-definition))
                       (= (nth 2 data-start-definition) nelisp-standalone--arena-data-start-offset))
            (error "Unresolved data-start provider"))
          (let ((print-length nil) (print-level nil) (print-circle nil)
                (files (sort (delete-dups (delq nil (mapcar #'car load-history))) #'string<)))
            (princ "WASM-TARGET-INIT=")
            (prin1 (list :forms forms :definitions definitions
                         :data-start nelisp-standalone--arena-data-start-offset
                         :data-start-definition data-start-definition
                         :files (mapcar
                                 (lambda (path)
                                   (cons path (with-temp-buffer
                                                (set-buffer-multibyte nil)
                                                (insert-file-contents-literally path)
                                                (secure-hash 'sha256 (current-buffer))))) files)))
            (terpri))))
    (error (princ (format "target-init-oracle-error=%s\n" (truncate-string-to-width (error-message-string err) 400)))
           (kill-emacs 1))))
(provide 'wasm-target-init-oracle)
