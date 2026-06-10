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
  (let ((c (expand-file-name "vendor/nelisp/target/nelisp"
                             external-packages-test--root)))
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
               "(nelisp--write-stderr-line (format \"R2=%S\" (eval (car (read-from-string \"(-map (lambda (x) (* x x)) (list 1 2 3))\")) t)))\n"))
            (with-temp-buffer
              (let ((status (call-process reader nil t nil
                                          "--eval"
                                          (format "(load %S nil t)" driver))))
                (should (equal 0 status))
                (let ((out (buffer-string)))
                  ;; the reader's %S prints strings without quotes
                  (should (string-match-p "R1=ABC" out))
                  (should (string-match-p "R2=(1 4 9)" out))))))
        (when (file-exists-p driver)
          (delete-file driver))))))

(provide 'external-packages-test)

;;; external-packages-test.el ends here
