;;; standalone-filesystem-access.el --- Actual directory search parity -*- lexical-binding: t; -*-
(require 'nelisp-stdlib-filesystem-access)
(unless (fboundp 'file-accessible-directory-p)
  (unwind-protect
      (progn
        (fset 'file-accessible-directory-p (lambda (&rest _) t))
        (unless (condition-case nil
                    (progn (nelisp-stdlib-filesystem-access-install
                            (getenv "NELISP_GNU_BUILD_SOURCE_ROOT")) nil) (error t))
          (error "Foreign filesystem provider accepted")))
    (fmakunbound 'file-accessible-directory-p)))
(let* ((existing (and (fboundp 'file-accessible-directory-p)
                      (symbol-function 'file-accessible-directory-p)))
       (installation (nelisp-stdlib-filesystem-access-install
                      (getenv "NELISP_GNU_BUILD_SOURCE_ROOT")))
       (root (getenv "NELISP_FILESYSTEM_ACCESS_FIXTURE")))
  (when existing
    (unless (and (eq installation 'preserved)
                 (eq existing (symbol-function 'file-accessible-directory-p)))
      (error "GNU filesystem provider was not preserved")))
  (dolist (case '(("readable" . t) ("search-only" . t) ("no-search" . nil)
                  ("link" . t) ("regular" . nil) ("missing" . nil)))
    (unless (eq (file-accessible-directory-p (expand-file-name (car case) root)) (cdr case))
      (error "Filesystem search case differs: %s" (car case))))
  (unless (condition-case nil (progn (file-accessible-directory-p 1) nil)
            (wrong-type-argument t))
    (error "Filesystem filename type error differs"))
  (let ((file-name-handler-alist '(("/nelisp-handler-probe" . nelisp-filesystem-test-handler))))
    (fset 'nelisp-filesystem-test-handler (lambda (operation filename) 'handled))
    (put 'nelisp-filesystem-test-handler 'operations '(file-accessible-directory-p))
    (unwind-protect
        (unless (eq (file-accessible-directory-p "/nelisp-handler-probe") 'handled)
          (error "Genuine filesystem handler dispatch differs"))
      (fmakunbound 'nelisp-filesystem-test-handler)
      (put 'nelisp-filesystem-test-handler 'operations nil)))
  (when (eq installation 'installed)
    (let ((owner (symbol-function 'syscall-direct)) (entered nil))
      (unwind-protect
          (progn
            (fset 'syscall-direct (lambda (&rest _) (setq entered t) 0))
            (unless (condition-case nil
                        (progn (file-accessible-directory-p root) nil) (error t))
              (error "Changed syscall owner accepted"))
            (when entered (error "Changed syscall owner executed")))
        (fset 'syscall-direct owner)))))
(princ "FILESYSTEM-ACCESS-PASS\n")
