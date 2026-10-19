;;; standalone-boot-library-loader-check.el --- Overrides and autoload rollback -*- lexical-binding: t; -*-
(let* ((directory (make-temp-file "boot-library-loader-" t))
       (file (expand-file-name "rx.el" directory))
       (original (symbol-function 'rx))
       (original-features features)
       (checks 0) (failures 0)
       (check (lambda (label value)
                (setq checks (1+ checks))
                (unless value
                  (setq failures (1+ failures))
                  (princ (format "BOOT-LOADER FAIL %S\n" label))))))
  (unwind-protect
      (let ((load-path (list directory)))
        (setq features (remq 'rx features))
        (fset 'rx '(autoload "rx" nil nil macro))
        (write-region ";;; -*- lexical-binding: t; -*-\n(defmacro rx (&rest ignored) (list 'quote \"OVERRIDE\"))\n(provide 'rx)\n"
                      nil file nil 'silent)
        (funcall check 'load-path-override (equal (eval '(rx "a")) "OVERRIDE"))
        (funcall check 'override-feature (featurep 'rx))
        ;; A failed autoload must restore the exact function cell and features.
        (setq features (remq 'rx features))
        (fset 'rx '(autoload "rx" nil nil macro))
        (let ((before (symbol-function 'rx)) (caught nil))
          (write-region ";;; -*- lexical-binding: t; -*-\n(defmacro rx (&rest ignored) (list 'quote \"BROKEN\"))\n(error \"boot-loader-broken\")\n(provide 'rx)\n"
                        nil file nil 'silent)
          (condition-case err
              (eval '(rx "a"))
            (error (setq caught (string-match-p "boot-loader-broken" (format "%S" err)))))
          (funcall check 'expected-error caught)
          (funcall check 'autoload-restored (equal before (symbol-function 'rx)))
          (funcall check 'feature-rolled-back (not (featurep 'rx))))
        (write-region ";;; -*- lexical-binding: t; -*-\n(defmacro rx (&rest ignored) (list 'quote \"REPAIRED\"))\n(provide 'rx)\n"
                      nil file nil 'silent)
        (funcall check 'explicit-retry (equal (eval '(rx "a")) "REPAIRED"))
        (funcall check 'retry-feature (featurep 'rx))
        (funcall check 'unrelated-explicit-path
                 (null (load (expand-file-name "absent/rx.el" directory) t))))
    (fset 'rx original)
    (setq features original-features)
    (delete-file file)
    (delete-directory directory))
  (princ (format "BOOT-LOADER checks=%d failures=%d %s\n"
                 checks failures (if (= failures 0) "PASS" "FAIL")))
  (kill-emacs (if (= failures 0) 0 1)))
