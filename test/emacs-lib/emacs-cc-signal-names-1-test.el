;;; emacs-cc-signal-names-1-test.el --- signal-names fallback tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defvar emacs-network-ffi-libc-path nil)

(unless (fboundp 'ffi:library)
  (fset 'ffi:library (lambda (&rest _) (error "Unmocked ffi:library"))))
(unless (fboundp 'nl-ffi--invoke)
  (fset 'nl-ffi--invoke (lambda (&rest _) (error "Unmocked nl-ffi--invoke"))))
(unless (fboundp 'nl-ffi-get-string)
  (fset 'nl-ffi-get-string
        (lambda (&rest _) (error "Unmocked nl-ffi-get-string"))))

(defconst emacs-cc-signal-names-1-test--source
  (expand-file-name "../../packages/nelisp-emacs-foundation/src/emacs-cc-signal-names-1.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest emacs-cc-signal-names-1-test/preserves-host-binding ()
  (let ((before (symbol-function 'signal-names)))
    (load emacs-cc-signal-names-1-test--source nil t)
    (should (eq before (symbol-function 'signal-names)))))

(ert-deftest emacs-cc-signal-names-1-test/derives-order-and-filters-unsupported-numbers ()
  (let ((saved (symbol-function 'signal-names))
        (emacs-network-ffi-libc-path "test-libc.so"))
    (unwind-protect
        (progn
          (fmakunbound 'signal-names)
          (load emacs-cc-signal-names-1-test--source nil t)
          (provide 'emacs-network-ffi)
          (provide 'nl-ffi)
          (cl-letf
              (((symbol-function 'ffi:library)
                (lambda (soname) (should (equal soname "test-libc.so"))))
               ((symbol-function 'nl-ffi--invoke)
                (lambda (function name arguments result args)
                  (should (eq function 'signal-names))
                  (cond
                   ((member name '("__libc_current_sigrtmin"
                                   "__libc_current_sigrtmax"))
                    (should (null arguments))
                    (should (eq result :sint32))
                    (should (null args))
                    (if (equal name "__libc_current_sigrtmin") 4 6))
                   (t
                    (should (equal arguments '(:sint32)))
                    (should (eq result :pointer))
                    (cdr (assq (car args) '((3 . 33) (1 . 11))))))))
               ((symbol-function 'nl-ffi-get-string)
                (lambda (pointer)
                  (cdr (assq pointer
                             '((55 . "TRAP") (33 . "QUIT") (11 . "HUP")))))))
            (should (equal '("RTMAX" "RTMIN+1" "RTMIN" "QUIT" "HUP" "EXIT")
                           (signal-names)))
            (should (equal '(wrong-number-of-arguments signal-names 1)
                           (condition-case err
                               (signal-names 'extra)
                             (error err))))))
      (fset 'signal-names saved))))

(provide 'emacs-cc-signal-names-1-test)
;;; emacs-cc-signal-names-1-test.el ends here
