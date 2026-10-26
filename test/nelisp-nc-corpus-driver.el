;;; nelisp-nc-corpus-driver.el --- fx corpus on the native lowering  -*- lexical-binding: t; -*-
;; Loaded before test/nelisp-fx-adopt-corpus.el (FXC_FORCE=1) on a binary with
;; the native lowering.  The install hook converts a corpus function whose fxir
;; matches its harvested row as the corpus registers it; NC_MODE=vm gives every
;; such record back to the word VM instead.  `nc-corpus-report' prints how many
;; corpus functions ran native code; the check script loads a file calling it
;; last, since `kill-emacs-hook' does not run when the binary exits after --load.
(defvar nc-corpus-native nil)
(let ((orig (symbol-function 'nelisp--fx-register)))
  (fset 'nelisp--fx-register
        (lambda (name)
          (prog1 (funcall orig name)
            (when (and (boundp 'fxc-fns) (memq name fxc-fns) (memq name nelisp-nc--converted))
              (if (equal (getenv "NC_MODE") "vm") (nelisp-nc-select name 'vm)
                (push name nc-corpus-native)))))))
(defun nc-corpus-report ()
  (princ (format "NCCORPUS native=%d\n" (length nc-corpus-native))))
