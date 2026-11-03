;;; standalone-display-images.el --- Public image capability controls -*- lexical-binding: t; -*-
(require 'nelisp-stdlib-display-images)
(let ((existing (and (fboundp 'display-images-p) (symbol-function 'display-images-p))))
  (if existing
      (progn
        (unless (eq (nelisp-stdlib-display-images-install) 'preserved)
          (error "Existing image owner was not preserved"))
        (unless (eq existing (symbol-function 'display-images-p))
          (error "Existing image owner changed")))
    (unwind-protect
        (progn
          (fset 'display-images-p (lambda (&rest _) t))
          (unless (condition-case nil
                      (progn (nelisp-stdlib-display-images-install
                              (getenv "NELISP_GNU_DATA_SOURCE_ROOT")) nil)
                    (error t))
            (error "Foreign standalone image predicate accepted")))
      (fmakunbound 'display-images-p))
    (let ((owner (symbol-function 'display-graphic-p)) (entered nil))
      (unwind-protect
          (progn
            (fset 'display-graphic-p (lambda (&rest _) (setq entered t) t))
            (unless (condition-case nil
                        (progn (nelisp-stdlib-display-images-install
                                (getenv "NELISP_GNU_DATA_SOURCE_ROOT")) nil)
                      (error t))
              (error "Mutated graphics owner accepted"))
            (when entered (error "Mutated graphics owner executed")))
        (fset 'display-graphic-p owner)))
    (unless (eq (nelisp-stdlib-display-images-install
                 (getenv "NELISP_GNU_DATA_SOURCE_ROOT")) 'installed)
      (error "Genuine image predicate was not installed")))
  (when (display-images-p) (error "Non-graphical image capability fabricated"))
  (when (display-images-p nil) (error "Explicit default image capability differs"))
  (princ "DISPLAY-IMAGES-PASS\n"))
