;;; emacs-files-gnu-file-local-copy-test.el --- focused file-local-copy tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-files-gnu)

(ert-deftest emacs-files-gnu-file-local-copy/local-files-return-nil ()
  (should-not (file-local-copy "ordinary-local-file"))
  (should-not (file-local-copy "/path/that/does/not/exist")))

(ert-deftest emacs-files-gnu-file-local-copy/delegates-exact-operation-and-file ()
  (let ((calls 0)
        (handler-result "handler-copy"))
    (let ((file-name-handler-alist
           `(("\\`remote:" .
              ,(lambda (operation file)
                 (setq calls (1+ calls))
                 (should (eq operation 'file-local-copy))
                 (should (equal file "remote:host:/x"))
                 handler-result)))))
      (should (equal (file-local-copy "remote:host:/x") handler-result)))
    (should (= calls 1))))

(ert-deftest emacs-files-gnu-file-local-copy/propagates-handler-errors ()
  (let ((file-name-handler-alist
         `(("\\`remote-error:" .
            ,(lambda (&rest _arguments)
               (error "file-local-copy handler failure"))))))
    (should-error (file-local-copy "remote-error:/x")
                  :type 'error)))

(provide 'emacs-files-gnu-file-local-copy-test)
;;; emacs-files-gnu-file-local-copy-test.el ends here
