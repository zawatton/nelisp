;;; nelisp-prelude-bytecode-autoload-test.el --- Portable host autoloads -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-prelude-bytecode)

(defun nelisp-autoload-test--query (file symbols)
  "Query SYMBOLS after GNU loads FILE, as a Windows build host does."
  (let ((original (symbol-function 'call-process)))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program _infile _destination _display &rest arguments)
                 (apply original program nil '(t nil) nil
                        "--batch" "-Q" "--eval" "(setq inhibit-message t)"
                        "--load" file arguments))))
      (nelisp-prelude-bytecode--host-autoloads symbols))))

(ert-deftest nelisp-host-autoload-resolves-compiled-docstrings ()
  (let* ((directory (make-temp-file "nelisp-autoload-" t))
         (source (expand-file-name "loaddefs.el" directory)))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(autoload 'nelisp-test-lazy-autoload \"missing-library\" \"Portable doc.\" t)\n"
                    "(autoload 'nelisp-test-nil-autoload \"missing-library\" nil nil 'macro)\n"))
          (let ((byte-compile-dynamic-docstrings t))
            (should (byte-compile-file source)))
          (let* ((symbols '(nelisp-test-lazy-autoload nelisp-test-nil-autoload))
                 (entries (nelisp-autoload-test--query (concat source "c") symbols)))
            ;; Source autoloads have inline docs, as this Linux host does.
            (should (equal entries (nelisp-autoload-test--query source symbols)))
            (should (equal (car entries)
                           '(nelisp-test-lazy-autoload "missing-library" "Portable doc." t nil)))
            (should (equal (cadr entries)
                           '(nelisp-test-nil-autoload "missing-library" nil nil macro)))))
      (delete-directory directory t))))

(ert-deftest nelisp-host-autoload-official-windows-loaddefs ()
  (let ((file (getenv "NELISP_TEST_WINDOWS_LOADDEFS")))
    (skip-unless file)
    (let ((entry (car (nelisp-autoload-test--query file '(compilation-mode)))))
      (should (equal (car entry) 'compilation-mode))
      (should (equal (nth 1 entry) "compile"))
      (should (stringp (nth 2 entry)))
      (should (string-prefix-p "Major mode for compilation log buffers." (nth 2 entry)))
      (should (eq (nth 3 entry) t)))))

(ert-deftest nelisp-vendor-autoload-fold-quotes-metadata ()
  (let* ((directory (make-temp-file "nelisp-fold-autoload-" t))
         (symbol (make-symbol "nelisp-test-folded-autoload"))
         (source (format "(eval-when-compile (require 'compile))\n(defun use-it () (%s))\n" symbol))
         (entry (list symbol "missing-library" "Portable doc." nil 'macro)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "compile.el" directory)
            (insert (format "(defun %s () nil)\n(provide 'compile)\n" symbol)))
          (cl-letf (((symbol-function 'nelisp-prelude-bytecode--host-autoloads)
                     (lambda (_) (list entry))))
            (let* ((output (car (nelisp-prelude-bytecode--vendor-fold
                                 source (list directory) nil)))
                   (form (car (read-from-string output))))
              ;; Generated autoload arguments must be data, including TYPE.
              (eval form t)
              (should (equal (cdr (symbol-function (intern (symbol-name symbol))))
                             (cdr entry))))))
      (delete-directory directory t)
      (fmakunbound (intern (symbol-name symbol))))))

(provide 'nelisp-prelude-bytecode-autoload-test)
;;; nelisp-prelude-bytecode-autoload-test.el ends here
