;;; par-bytecode-image-after-load.el --- GNU preload callbacks -*- lexical-binding: t; -*-

(unless (fboundp 'nelisp--write-stdout-bytes)
  (load (expand-file-name "register.elc" (getenv "NELISP_GNU_LISP")) nil t t t))

(let* ((name 'register-read-with-preview)
       (owner (symbol-file name 'defun))
       (caller load-file-name)
       (features (remq 'register (copy-sequence features)))
       (load-history
        (let (entries)
          (dolist (entry load-history)
            (unless (or (equal (car entry) owner)
                        (member '(provide . register) entry))
              (push entry entries)))
          (nreverse entries)))
       (after-load-alist nil)
       (events nil)
       (after-load-functions
        (list (lambda (file)
                (when (equal file owner)
                  (push (list 'hook (equal load-file-name caller)) events))))))
  (eval-after-load 'register
    (lambda ()
      (push (list 'feature (featurep 'register)
                  (byte-code-function-p (symbol-function name))) events)))
  (eval-after-load owner
    (lambda ()
      (push (list 'file (equal load-file-name caller)
                  (equal (symbol-file name 'defun) owner)) events)))
  ;; Any immediate notification from the host's dumped feature predates
  ;; the load being measured.
  (setq events nil)
  (load owner nil t t t)
  (prin1 (cons 'events (nreverse events)))
  (terpri)
  (prin1 (list 'owner (file-name-nondirectory owner)
               (and (member '(provide . register) (assoc owner load-history)) t)))
  (terpri))
