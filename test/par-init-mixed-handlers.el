;;; -*- lexical-binding: t; -*-
(defun par-init-mixed-row (label op &rest paths)
  (prin1 (cons label (mapcar (lambda (path) (find-file-name-handler path op)) paths)))
  (terpri))

(let* ((remote (copy-sequence "\\`/[^/:]+:[^/:]*:"))
       (compression (copy-sequence "\\.gz\\'"))
       (entry (cons remote 'par-init-remote))
       (file-name-handler-alist
        (list entry (cons compression 'jka-compr-handler)
              '("\\`/:" . file-name-non-special)))
       (operations (get 'jka-compr-handler 'operations)))
  (unwind-protect
      (progn
        (put 'jka-compr-handler 'operations
             '(byte-compiler-base-file-name write-region
               insert-file-contents file-local-copy load))
        (string-match "\\(b\\)" "abc")
        (par-init-mixed-row 'register 'load "/local/item.gz")
        (par-init-mixed-row 'cold 'file-exists-p "/local/item.gz")
        (par-init-mixed-row 'warm 'file-exists-p "/local/item.gz" "/local/a:b:"
                            "/ssh:host:/item.gz" "/:/local/item.gz")
        (prin1 (list 'match-data (match-data)))
        (terpri)
        ;; A snapshot proves only the regexp spellings. Operations stay live.
        (put 'jka-compr-handler 'operations '(file-exists-p))
        (par-init-mixed-row 'changed-operations 'file-exists-p "/local/item.gz")
        (let ((inhibit-file-name-operation 'file-exists-p)
              (inhibit-file-name-handlers '(jka-compr-handler)))
          (par-init-mixed-row 'inhibited 'file-exists-p "/local/item.gz"))
        (put 'jka-compr-handler 'operations
             '(byte-compiler-base-file-name write-region
               insert-file-contents file-local-copy load))
        (par-init-mixed-row 'rewarm 'file-exists-p "/local/item.gz")
        (aset remote 5 ?x)
        (par-init-mixed-row 'changed-remote 'file-exists-p "/local/a:b:")
        (setcar entry "[")
        (prin1 (list 'invalid
                     (condition-case err
                         (find-file-name-handler "/local/item" 'file-exists-p)
                       (invalid-regexp (car err)))))
        (terpri))
    (put 'jka-compr-handler 'operations operations)))
