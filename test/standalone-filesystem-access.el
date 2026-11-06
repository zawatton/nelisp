;;; standalone-filesystem-access.el --- Actual directory search parity -*- lexical-binding: t; -*-
(require 'nelisp-stdlib-filesystem-access)
(defvar nelisp-filesystem-test-prelude-provider nil)
(unless (and (fboundp 'file-accessible-directory-p)
             (not (eq (symbol-function 'file-accessible-directory-p)
                      (get 'file-accessible-directory-p 'nelisp-prelude-provider))))
  (unwind-protect
      (progn
        (setq nelisp-filesystem-test-prelude-provider
              (and (fboundp 'file-accessible-directory-p)
                   (symbol-function 'file-accessible-directory-p)))
        (fset 'file-accessible-directory-p (lambda (&rest _) t))
        (unless (condition-case nil
                    (progn (nelisp-stdlib-filesystem-access-install
                            (getenv "NELISP_GNU_BUILD_SOURCE_ROOT")) nil) (error t))
          (error "Foreign filesystem provider accepted")))
    (if nelisp-filesystem-test-prelude-provider
        (fset 'file-accessible-directory-p nelisp-filesystem-test-prelude-provider)
      (fmakunbound 'file-accessible-directory-p))))
(let* ((existing (and (fboundp 'file-accessible-directory-p)
                      (symbol-function 'file-accessible-directory-p)))
       (installation (nelisp-stdlib-filesystem-access-install
                      (getenv "NELISP_GNU_BUILD_SOURCE_ROOT")))
       (root (getenv "NELISP_FILESYSTEM_ACCESS_FIXTURE")))
  (when (and existing
             (not (eq existing (get 'file-accessible-directory-p
                                    'nelisp-prelude-provider))))
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
