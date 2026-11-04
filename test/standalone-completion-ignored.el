;;; standalone-completion-ignored.el --- Genuine completion initializer controls -*- lexical-binding: t; -*-
(require 'nelisp-stdlib-completion-ignored)
(if (boundp 'completion-ignored-extensions)
    (let ((value completion-ignored-extensions))
      (unless (eq (nelisp-stdlib-completion-ignored-install) 'preserved)
        (error "Existing completion value was not preserved"))
      (unless (eq value completion-ignored-extensions)
        (error "Existing completion value changed")))
  (unless (condition-case nil
              (progn (nelisp-stdlib-completion-ignored-install) nil)
            (error t))
    (error "Unconfigured completion initializer accepted"))
  (when (boundp 'completion-ignored-extensions)
    (error "Refused initializer published value"))
  (let ((owner (symbol-function 'secure-hash)) (entered nil))
    (unwind-protect
        (progn
          (fset 'secure-hash (lambda (&rest _) (setq entered t) "forged"))
          (unless (condition-case nil
                      (progn (nelisp-stdlib-completion-ignored-install
                              (getenv "NELISP_GNU_DATA_SOURCE_ROOT")) nil)
                    (error t))
            (error "Changed initializer owner accepted"))
          (when entered (error "Changed initializer owner executed")))
      (fset 'secure-hash owner)))
  (let ((original nelisp-stdlib-completion-ignored--form))
    (unwind-protect
        (progn
          (setq nelisp-stdlib-completion-ignored--form '(setq completion-ignored-extensions nil))
          (unless (condition-case nil
                      (progn (nelisp-stdlib-completion-ignored-install
                              (getenv "NELISP_GNU_DATA_SOURCE_ROOT")) nil)
                    (error t))
            (error "Changed initializer payload accepted")))
      (setq nelisp-stdlib-completion-ignored--form original)))
  (unless (eq (nelisp-stdlib-completion-ignored-install
               (getenv "NELISP_GNU_DATA_SOURCE_ROOT")) 'installed)
    (error "Genuine completion initializer not installed"))
  (unless (and (member ".elc" completion-ignored-extensions)
               (member ".git/" completion-ignored-extensions)
               (member ".pyc" completion-ignored-extensions)
               (not (member ".log" completion-ignored-extensions)))
    (error "Completion defaults differ from genuine source"))
  (let ((value completion-ignored-extensions))
    (unless (and (eq (nelisp-stdlib-completion-ignored-install) 'preserved)
                 (eq value completion-ignored-extensions))
      (error "Installed completion value was overwritten"))))
(princ "COMPLETION-IGNORED-PASS\n")
