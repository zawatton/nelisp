;;; external-packages-test.el --- M18 external package surface -*- lexical-binding: t; -*-

;;; Commentary:

;; M18 external-packages lane gate: real MELPA-style packages copied
;; under ~/.nemacs.d load on the standalone reader and their functions
;; run.  Skipped when the reader or the user's package copies are
;; absent (machine-local lane).

;;; Code:

(require 'ert)

(defconst external-packages-test--root
  (expand-file-name
   ".."
   (file-name-directory (or load-file-name buffer-file-name))))

(defun external-packages-test--reader ()
  (let ((c (or (getenv "NEMACS_NELISP")
               (expand-file-name "vendor/nelisp/target/nelisp"
                                 external-packages-test--root))))
    (and (file-executable-p c) c)))

(ert-deftest external-packages-test/dash-s-load-and-run ()
  "dash.el and s.el load from ~/.nemacs.d and their functions run."
  (let ((reader (external-packages-test--reader))
        (dash (expand-file-name "~/.nemacs.d/external-packages/dash.el/dash.el"))
        (s (expand-file-name "~/.nemacs.d/external-packages/s.el/s.el")))
    (unless (and reader (file-readable-p dash) (file-readable-p s))
      (ert-skip "reader or ~/.nemacs.d package copies not available"))
    (let ((driver (make-temp-file "m18-pkg" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file driver
              (insert
               (format "(load %S nil t)\n"
                       (expand-file-name "src/emacs-stub.el"
                                         external-packages-test--root))
               (format "(load %S nil t)\n"
                       (expand-file-name "src/emacs-stub-bulk.el"
                                         external-packages-test--root))
               (format "(load %S nil t)\n"
                       (expand-file-name "src/emacs-network-syscall-shim.el"
                                         external-packages-test--root))
               (format "(load %S nil t)\n" dash)
               (format "(load %S nil t)\n" s)
               ;; the server's eval path: read-from-string + eval
               "(nelisp--write-stderr-line (format \"R1=%S\" (eval (car (read-from-string \"(s-upcase \\\"abc\\\")\")) t)))\n"
               "(nelisp--write-stderr-line (format \"R2=%S\" (eval (car (read-from-string \"(-map (lambda (x) (* x x)) (list 1 2 3))\")) t)))\n"
               ;; identity must never be a nil no-op (s-join pipeline)
               "(nelisp--write-stderr-line (format \"R3=%S\" (s-join \"-\" (-map (lambda (x) (number-to-string x)) (list 1 2 3)))))\n"))
            (with-temp-buffer
              (let ((status (call-process reader nil t nil
                                          "--eval"
                                          (format "(load %S nil t)" driver))))
                (should (equal 0 status))
                (let ((out (buffer-string)))
                  ;; the reader's %S prints strings without quotes
                  (should (string-match-p "R1=ABC" out))
                  (should (string-match-p "R2=(1 4 9)" out))
                  (should (string-match-p "R3=1-2-3" out))))))
        (when (file-exists-p driver)
          (delete-file driver))))))

(ert-deftest external-packages-test/eat-require-password-prompt-default ()
  "Eat's Tramp dependency must see the password prompt word default."
  (let* ((reader (external-packages-test--reader))
         (root external-packages-test--root)
         (launcher (expand-file-name "bin/nemacs" root))
         (bootstrap (or (getenv "NEMACS_BOOTSTRAP_REPL")
                        (expand-file-name "build/nemacs-bootstrap.repl" root)))
         (eat (expand-file-name "~/.emacs.d/external-packages/emacs-eat/eat.el"))
         (compat (expand-file-name "~/.emacs.d/external-packages/compat/compat.el"))
         (core-vars (expand-file-name "src/emacs-parity-core-vars.el" root)))
    (unless (and reader (file-executable-p launcher) (file-readable-p bootstrap)
                 (file-readable-p eat) (file-readable-p compat))
      (ert-skip "standalone launcher/bootstrap or Eat/compat unavailable"))
    (let ((driver (make-temp-file "m18-eat-password" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file driver
              (insert (format "(setq load-path (cons %S load-path))\n"
                              (file-name-directory compat)))
              (insert
               (format "(setq load-path (cons %S load-path))\n"
                       (file-name-directory eat)))
              (insert (format "(load %S nil t)\n" core-vars))
              (insert (format "(makunbound 'password-word-equivalents)\n"))
              (insert (format "(load %S nil t)\n" core-vars))
              (insert (format "(require 'eat)\n"))
              (insert
               "(nelisp--write-stderr-line (format \"EAT_PASSWORD_WORDS=%S\" (and (boundp 'password-word-equivalents) (if (member \"password\" password-word-equivalents) t nil))))\n"))
            (with-temp-buffer
              (let ((process-environment (copy-sequence process-environment)))
                (setenv "NEMACS_HOME" root)
                (setenv "NEMACS_NELISP" reader)
                (setenv "NEMACS_BOOTSTRAP_REPL" bootstrap)
                (setenv "NEMACS_BOOTSTRAP_BUNDLE" "")
                (setenv "NEMACS_DISABLE_COLD_CACHE" "1")
                (let ((status (call-process launcher nil t nil
                                            "--driver=nelisp" "--batch"
                                            "--no-site-file" "--no-init-file"
                                            "--load" driver)))
                (should (equal 0 status))
                (should (string-match-p "EAT_PASSWORD_WORDS=t"
                                        (buffer-string)))))))
        (when (file-exists-p driver)
          (delete-file driver))))))



;;; external-packages-test.el ends here

(ert-deftest external-packages-test/numeric-stubs-not-noop ()
  "floor/float/ceiling/round are real implementations on the reader."
  (let ((reader (external-packages-test--reader)))
    (unless reader (ert-skip "no reader"))
    (let ((driver (make-temp-file "m194-num" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file driver
              (insert
               (format "(load %S nil t)\n"
                       (expand-file-name "src/emacs-stub.el" external-packages-test--root))
               (format "(load %S nil t)\n"
                       (expand-file-name "src/emacs-stub-bulk.el" external-packages-test--root))
               (format "(load %S nil t)\n"
                       (expand-file-name "src/emacs-network-syscall-shim.el" external-packages-test--root))
               "(nelisp--write-stderr-line (format \"NUM=%S\" (list (floor 7 2) (floor -7 2) (ceiling 3.2) (float 3) (floor 3.7))))\n"))
            (with-temp-buffer
              (let ((status (call-process reader nil t nil "--eval"
                                          (format "(load %S nil t)" driver))))
                (should (equal 0 status))
                (should (string-match-p "NUM=(3 -4 4 3 3)" (buffer-string))))))
        (when (file-exists-p driver) (delete-file driver))))))
(provide 'external-packages-test)
