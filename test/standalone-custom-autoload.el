;;; standalone-custom-autoload.el --- Actual custom dependency registration -*- lexical-binding: t; -*-
(require 'nelisp-stdlib-custom-autoload)
(unless (fboundp 'custom-autoload)
  (unwind-protect
      (progn
        (fset 'custom-autoload (lambda (&rest _) nil))
        (unless (condition-case nil
                    (progn (nelisp-stdlib-custom-autoload-install
                            (getenv "NELISP_GNU_BUILD_SOURCE_ROOT")) nil) (error t))
          (error "Foreign custom registration accepted")))
    (fmakunbound 'custom-autoload)))
(let* ((existing (and (fboundp 'custom-autoload) (symbol-function 'custom-autoload)))
       (installation (nelisp-stdlib-custom-autoload-install
                      (getenv "NELISP_GNU_BUILD_SOURCE_ROOT")))
       (symbol (make-symbol "custom-registration-probe")) (effects 0))
  (when existing
    (unless (and (eq installation 'preserved) (eq existing (symbol-function 'custom-autoload)))
      (error "GNU custom provider changed")))
  (custom-autoload (progn (setq effects (1+ effects)) symbol)
                   (progn (setq effects (1+ effects)) "probe-library")
                   (progn (setq effects (1+ effects)) t))
  (unless (and (= effects 3) (eq (get symbol 'custom-autoload) 'noset)
               (equal (get symbol 'custom-loads) '("probe-library"))
               (not (boundp symbol)))
    (error "Custom autoload registration/no-evaluation semantics differ"))
  (custom-autoload symbol "probe-library" nil)
  (custom-add-load symbol 'second-feature)
  (custom-add-load symbol 'second-feature)
  (unless (and (eq (get symbol 'custom-autoload) t)
               (equal (get symbol 'custom-loads) '(second-feature "probe-library")))
    (error "Custom dependency deduplication/order differs"))
  (unless (condition-case nil (progn (custom-autoload 1 "probe") nil)
            (wrong-type-argument t))
    (error "Custom symbol type refusal differs"))
  (when (eq installation 'installed)
    (let ((owner (symbol-function 'put)) (entered nil))
      (unwind-protect
          (progn
            (fset 'put (lambda (&rest _) (setq entered t)))
            (unless (condition-case nil
                        (progn (nelisp-stdlib-custom-autoload-install
                                (getenv "NELISP_GNU_BUILD_SOURCE_ROOT")) nil) (error t))
              (error "Changed registration primitive accepted"))
            (when entered (error "Changed registration primitive executed")))
        (fset 'put owner)))))
(princ "CUSTOM-AUTOLOAD-PASS\n")
