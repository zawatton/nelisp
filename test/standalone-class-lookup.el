;;; standalone-class-lookup.el --- Genuine class metadata lookup/autoload controls -*- lexical-binding: t; -*-
(require 'nelisp-stdlib-class-lookup)
(unless (fboundp 'cl-find-class)
  (unwind-protect
      (progn
        (fset 'cl-find-class (lambda (&rest _) nil))
        (unless (condition-case nil
                    (progn (nelisp-stdlib-class-lookup-install
                            (getenv "NELISP_GNU_BUILD_SOURCE_ROOT")) nil) (error t))
          (error "Foreign class lookup accepted")))
    (fmakunbound 'cl-find-class)))
(let* ((existing (and (fboundp 'cl-find-class) (symbol-function 'cl-find-class)))
       (installation (nelisp-stdlib-class-lookup-install
                      (getenv "NELISP_GNU_BUILD_SOURCE_ROOT"))))
  (when existing
    (unless (and (eq installation 'preserved) (eq existing (symbol-function 'cl-find-class)))
      (error "GNU class lookup changed")))
  (when (cl-find-class (make-symbol "missing-class")) (error "Missing class was fabricated"))
  (unless (condition-case nil (progn (cl-find-class 1) nil)
            (wrong-type-argument t))
    (error "Class name type refusal differs"))
  (require 'eieio)
  (let* ((parent (cl-find-class 'eieio-default-superclass))
         (name (make-symbol "class-autoload-probe")))
    (unless (and parent (eq parent (get 'eieio-default-superclass 'cl--class)))
      (error "Existing genuine EIEIO class lookup differs"))
    (eieio-defclass-autoload name '(eieio-default-superclass) "class-probe-not-loaded" "Probe class")
    (let ((class (cl-find-class name)))
      (unless (and (eieio--class-p class) (eq class (get name 'cl--class))
                   (equal (eieio--class-parents class) (list parent))
                   (autoloadp (symbol-function name)))
        (error "Genuine EIEIO autoload class metadata differs"))))
  (when (eq installation 'installed)
    (let ((owner (symbol-function 'get)) (entered nil))
      (unwind-protect
          (progn
            (fset 'get (lambda (&rest _) (setq entered t) nil))
            (unless (condition-case nil
                        (progn (nelisp-stdlib-class-lookup-install
                                (getenv "NELISP_GNU_BUILD_SOURCE_ROOT")) nil) (error t))
              (error "Changed class metadata owner accepted"))
            (when entered (error "Changed class metadata owner executed")))
        (fset 'get owner)))))
(princ "CLASS-LOOKUP-PASS\n")
