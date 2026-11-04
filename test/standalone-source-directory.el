;;; standalone-source-directory.el --- Genuine source corpus controls -*- lexical-binding: t; -*-
(require 'nelisp-stdlib-source-directory)
(if (boundp 'source-directory)
    (let ((original source-directory))
      (unless (and (eq (nelisp-stdlib-source-directory-install) 'preserved)
                   (eq original source-directory))
        (error "GNU build source-directory owner changed")))
  (unless (condition-case nil
              (progn (nelisp-stdlib-source-directory-install) nil) (error t))
    (error "Unconfigured source corpus accepted"))
  (unless (condition-case nil
              (progn (nelisp-stdlib-source-directory-install
                      (getenv "NELISP_GNU_DATA_ROOT")) nil) (error t))
    (error "Resource directory accepted as source installation"))
  (when (boundp 'source-directory) (error "Refused source installation published value"))
  (let ((original nelisp-stdlib-source-directory--manifest))
    (unwind-protect
        (progn
          (setq nelisp-stdlib-source-directory--manifest nil)
          (unless (condition-case nil
                      (progn (nelisp-stdlib-source-directory-install
                              (getenv "NELISP_GNU_BUILD_SOURCE_ROOT")) nil) (error t))
            (error "Changed source manifest accepted")))
      (setq nelisp-stdlib-source-directory--manifest original)))
  (let ((owner (symbol-function 'secure-hash)) (entered nil))
    (unwind-protect
        (progn
          (fset 'secure-hash (lambda (&rest _) (setq entered t) "forged"))
          (unless (condition-case nil
                      (progn (nelisp-stdlib-source-directory-install
                              (getenv "NELISP_GNU_BUILD_SOURCE_ROOT")) nil) (error t))
            (error "Changed source owner accepted"))
          (when entered (error "Changed source owner executed")))
      (fset 'secure-hash owner)))
  (unless (eq (nelisp-stdlib-source-directory-install
               (getenv "NELISP_GNU_BUILD_SOURCE_ROOT")) 'installed)
    (error "Genuine source corpus not installed"))
  (unless (equal source-directory
                 (file-name-as-directory (file-truename (getenv "NELISP_GNU_BUILD_SOURCE_ROOT"))))
    (error "Published source corpus differs")))
(princ "SOURCE-DIRECTORY-PASS\n")
