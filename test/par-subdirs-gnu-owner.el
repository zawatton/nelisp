;;; par-subdirs-gnu-owner.el --- startup walk state and predicate dispatch -*- lexical-binding: t; -*-

(let* ((root (make-temp-file "par-subdirs-gnu-" t))
       (default-directory (file-name-as-directory root))
       (file-name-handler-alist nil)
       (normal-top-level-add-subdirs-inode-list nil)
       (match (symbol-function 'string-match)))
  (unwind-protect
      (progn
        (make-directory "a/b" t)
        ;; replaced-predicate (fset of string-match) is omitted: native-compiled GNU
        ;; calls the primitive directly and ignores the redefinition, bytecode GNU
        ;; follows it, so the expected transcript depends on how GNU was built.
        (dolist (mode '(first repeat))
          (let ((load-path (list root "/keep")))
            (when (eq mode 'replaced-predicate)
              (setq normal-top-level-add-subdirs-inode-list nil)
              (fset 'string-match
                    (lambda (regexp string &optional start inhibit-modify)
                      (unless (and (equal regexp "\\`[[:alnum:]]")
                                   (equal string "a"))
                        (funcall match regexp string start inhibit-modify)))))
            (normal-top-level-add-subdirs-to-load-path)
            (prin1 (cons mode
                         (mapcar (lambda (path)
                                   (if (equal path root) :root
                                     (file-name-nondirectory path)))
                                 load-path)))
            (terpri)))
        (fset 'string-match match))
    (fset 'string-match match)
    (delete-directory root t)))
