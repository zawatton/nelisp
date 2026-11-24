;;; nelisp-native-load-windows-require-test.el --- Windows mapping owner is loaded on demand -*- lexical-binding: t; -*-

;; `nelisp-native-load--mmap' requires `nelisp-native-windows' before
;; calling into it, but `--protect' and `--unmap' did not, so a release
;; or protection change reached through a path that never mapped first
;; (as Windows CI ERT did) signalled (void-function
;; nelisp-native-windows-unmap).  Run in a fresh Emacs so no earlier
;; test has loaded the module already.

(require 'ert)

(defun nelisp-native-load-windows-require-test--run (call)
  "Run CALL with a Windows target in a fresh Emacs; return the printed result."
  (let ((lisp-dir (file-name-directory (locate-library "nelisp-native-load"))))
    (with-temp-buffer
      (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                    "--batch" "-Q" "-L" lisp-dir
                    "-L" (expand-file-name "../src" lisp-dir)
                    "--eval"
                    (format "(progn (require 'nelisp-native-load)
  (when (featurep 'nelisp-native-windows) (princ \"RESULT preloaded\") (kill-emacs 0))
  (let ((nelisp-native-load--build-target 'windows-x86_64))
    (princ (format \"RESULT %%S\" (condition-case e %s (error e))))))"
                            call))
      (goto-char (point-min))
      (and (re-search-forward "^RESULT \\(.*\\)$" nil t) (match-string 1)))))

(ert-deftest nelisp-native-load-windows-require-test/unmap-and-protect-load-owner ()
  "Neither entry point fails on the Windows owner being unloaded."
  (skip-unless (locate-library "nelisp-native-load"))
  (dolist (call '("(nelisp-native-load--unmap 4096 4096)"
                  "(nelisp-native-load--protect 4096 4096 1)"))
    (let ((result (nelisp-native-load-windows-require-test--run call)))
      (should result)
      (should-not (equal result "preloaded"))
      (should-not (string-match-p "void-function nelisp-native-windows-" result)))))

(provide 'nelisp-native-load-windows-require-test)
;;; nelisp-native-load-windows-require-test.el ends here
