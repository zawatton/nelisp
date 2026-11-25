;;; -*- lexical-binding: t; -*-

(defun par-init-remote-handler-row (label &rest files)
  (prin1
   (cons label
         (mapcar (lambda (file)
                   (find-file-name-handler file 'file-exists-p))
                 files)))
  (terpri))

(let* ((pattern (copy-sequence "\\`/[^/:]+:[^/:]*:"))
       (entry (cons pattern 'par-init-remote))
       (file-name-handler-alist (list entry)))
  (string-match "\\(b\\)" "abc")
  (par-init-remote-handler-row 'warm "/local/item" "/local/a:b:"
                               "/ssh:host:/folder" "relative/ssh:host:/")
  (let ((inhibit-file-name-operation 'file-exists-p)
        (inhibit-file-name-handlers '(par-init-remote)))
    (par-init-remote-handler-row 'inhibited "/ssh:host:/folder"))
  (prin1 (list 'match-data (match-data)))
  (terpri)
  ;; Let the first component of the regexp consume slash.
  (aset pattern 5 ?x)
  (par-init-remote-handler-row 'mutated-pattern "/local/a:b:")
  ;; Replace the live entry's pattern and next cons after the warm calls.
  (setcar entry "item\\'")
  (setcdr file-name-handler-alist '(("second" . par-init-second)))
  (par-init-remote-handler-row 'changed-alist "/local/item" "/local/second")
  (setcar entry "[")
  (prin1 (list 'invalid
               (condition-case err
                   (find-file-name-handler "/local/item" 'file-exists-p)
                 (invalid-regexp (car err)))))
  (terpri))

(require 'tramp)
(let* ((copy (copy-sequence file-name-handler-alist))
       (file-name-handler-alist
        (delq nil (mapcar (lambda (entry)
                            (and (memq (cdr entry)
                                       '(tramp-file-name-handler
                                         tramp-completion-file-name-handler))
                                 entry))
                          copy))))
  (par-init-remote-handler-row 'gnu-tramp "/local/item" "/local/a:b:"
                               "/ssh" "/ssh:host:/folder" "/ssh:host:/folder:"))
