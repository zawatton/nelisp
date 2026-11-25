;;; par-alloc-io-load.el --- Live file and load lookup -*- lexical-binding: t; -*-

(defvar par-alloc-io--loaded nil)
(defvar par-alloc-io--retime-file nil)

(defun par-alloc-io--retime (operation &rest _args)
  (when (eq operation 'file-readable-p)
    (set-file-times par-alloc-io--retime-file '(30000 0 0 0)))
  nil)

(put 'par-alloc-io--retime 'operations '(file-exists-p file-readable-p))

(let* ((root (make-temp-file "par-alloc-io-" t))
       (default-directory (file-name-as-directory root))
       (load-path (list default-directory))
       (file-name-handler-alist nil)
       (load-file-rep-suffixes '(""))
       (first (expand-file-name "entry.one"))
       (second (expand-file-name "entry.two"))
       (link (expand-file-name "entry.link")))
  (unwind-protect
      (progn
        (make-symbolic-link first link)
        (prin1 (list :dangling (file-exists-p link) (file-readable-p link)
                     (locate-file "entry" load-path '(".link"))))
        (terpri)
        (write-region ";; -*- lexical-binding: t; -*-\n(setq par-alloc-io--loaded 'one)" nil first nil 'silent)
        (prin1 (list :created (file-exists-p link) (file-readable-p link)
                     (file-readable-p default-directory)
                     (file-name-nondirectory
                      (locate-file "entry" load-path '(".link")))))
        (terpri)
        (set-file-modes first 0)
        (prin1 (list :unreadable (file-exists-p link) (file-readable-p link)
                     (and (locate-file "entry" load-path '(".link")) t)))
        (terpri)
        (set-file-modes first #o600)
        (rename-file first second)
        (make-directory first)
        (prin1 (list :directory (file-exists-p link) (file-readable-p link)
                     (file-name-nondirectory
                      (locate-file "entry" load-path '(".one" ".two")))))
        (terpri)
        (delete-directory first)
        (write-region ";; -*- lexical-binding: t; -*-\n(setq par-alloc-io--loaded 'two)"
                      nil second nil 'silent)
        (write-region ";; -*- lexical-binding: t; -*-\n(setq par-alloc-io--loaded 'one)" nil first nil 'silent)
        (let ((load-suffixes '(".one" ".retime" ".two"))
              (load-prefer-newer t)
              (par-alloc-io--retime-file first)
              (file-name-handler-alist
               '(("\\.retime\\'" . par-alloc-io--retime))))
          (set-file-times first '(10000 0 0 0))
          (set-file-times second '(20000 0 0 0))
          ;; A later handler can change the first candidate's pathname.
          ;; GNU compares mtimes observed when it opened the candidates.
          (load "entry" nil t nil t)
          (prin1 (list :observed-mtime par-alloc-io--loaded))
          (terpri)
          (let ((file-name-handler-alist nil)
                (load-suffixes (if (getenv "PAR_ALLOC_IO_REVERSE")
                                   '(".two" ".one")
                                 '(".one" ".two"))))
            (set-file-times first '(20000 0 0 0))
            (load "entry" nil t nil t)
            (prin1 (list :tie par-alloc-io--loaded))
            (terpri))))
    (delete-directory root t)))
