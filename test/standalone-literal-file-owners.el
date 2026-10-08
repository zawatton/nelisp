;;; standalone-literal-file-owners.el --- Refusal before public state mutation -*- lexical-binding: t; -*-
(require 'nelisp-stdlib-literal-file)
(let ((installed (eq (nelisp-stdlib-literal-file-install) 'installed))
      (file (getenv "NELISP_LITERAL_FIXTURE")))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "preserved")
    (let ((was-bound (boundp 'buffer-read-only))
          (previous (and (boundp 'buffer-read-only) (symbol-value 'buffer-read-only)))
          refused)
      (unwind-protect
          (progn
            ;; An explicit public assignment is the test input, not a startup initializer.
            (set 'buffer-read-only t)
            (condition-case condition (insert-file-contents-literally file)
              (buffer-read-only (setq refused (car condition))))
            (unless (and (eq refused 'buffer-read-only)
                         (equal (buffer-string) "preserved"))
              (error "Readonly insertion changed buffer or wrong condition")))
        (if was-bound (set 'buffer-read-only previous) (makunbound 'buffer-read-only)))))
  (when installed
    (dolist (owner '(nelisp-native-raw-file-read file-attributes file-truename))
      (let ((original (symbol-function owner)) refused)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert "preserved")
          (unwind-protect
              (progn
                (fset owner (lambda (&rest _args) (error "Changed owner executed")))
                (condition-case nil (insert-file-contents-literally file)
                  (error (setq refused t)))
                (unless (and refused (equal (buffer-string) "preserved"))
                  (error "Changed reader/metadata owner accepted")))
            (fset owner original)))))
    (let ((original (symbol-function 'nelisp-bytecode-compiler-input-dialect)) refused)
      (unwind-protect
          (progn
            (fset 'nelisp-bytecode-compiler-input-dialect (lambda () '(:status unknown)))
            (condition-case nil (nelisp-stdlib-literal-file-install)
              (error (setq refused t)))
            (unless refused (error "Changed evidence owner accepted")))
        (fset 'nelisp-bytecode-compiler-input-dialect original)))))
(princ "PUBLIC-LITERAL-OWNERS-PASS\n")
