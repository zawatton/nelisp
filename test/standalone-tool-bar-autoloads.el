;;; standalone-tool-bar-autoloads.el --- Genuine autoload closure controls -*- lexical-binding: t; -*-
(require 'nelisp-stdlib-tool-bar-autoloads)
(let ((before (and (fboundp 'tool-bar-local-item-from-menu)
                   (symbol-function 'tool-bar-local-item-from-menu)))
      (feature-before (featurep 'tool-bar)))
  (when (eq (nelisp-stdlib-tool-bar-autoloads-install) 'preserved)
    (unless (and (eq before (symbol-function 'tool-bar-local-item-from-menu))
                 (eq feature-before (featurep 'tool-bar)))
      (error "GNU toolbar owner changed"))))
(let ((owner (symbol-function 'autoload)) refused reached)
  (unwind-protect
      (progn
        (fset 'autoload (lambda (&rest ignored) (setq reached t)))
        (condition-case nil (nelisp-stdlib-tool-bar-autoloads-install)
          (error (setq refused t)))
        (unless (and refused (not reached)) (error "Mutated autoload owner accepted")))
    (fset 'autoload owner)))
(let ((original nelisp-stdlib-tool-bar-autoloads--forms) refused)
  (unwind-protect
      (progn
        (setq nelisp-stdlib-tool-bar-autoloads--forms nil)
        (condition-case nil (nelisp-stdlib-tool-bar-autoloads-install)
          (error (setq refused t)))
        (unless refused (error "Mutated source forms accepted")))
    (setq nelisp-stdlib-tool-bar-autoloads--forms original)))
(princ "TOOLBAR-AUTOLOAD-OWNERS-PASS\n")
