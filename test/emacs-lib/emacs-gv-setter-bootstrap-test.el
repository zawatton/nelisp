;;; emacs-gv-setter-bootstrap-test.el --- GNU gv declaration bootstrap test -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((test-file (or load-file-name buffer-file-name))
       (root (expand-file-name "../.." (file-name-directory test-file))))
  (add-to-list 'load-path (expand-file-name "scripts" root))
  (load (expand-file-name "scripts/build-nelisp-bootstrap.el" root) nil t))

(ert-deftest emacs-gv-setter-bootstrap/bundler-orders-real-provider-first ()
  "The eager bundle puts genuine declaration providers before consumers."
  (let* ((src (nelisp-bootstrap--src-dir))
         (shim (expand-file-name "emacs-parity-eieio.el" src))
         (macroexp (nelisp-bootstrap--vendor-source-file
                    "emacs-lisp/emacs-lisp/macroexp.el"))
         (gv (nelisp-bootstrap--vendor-source-file
              "emacs-lisp/emacs-lisp/gv.el"))
         (lazy (nelisp-bootstrap--vendor-source-file
                "emacs-lisp-api/emacs-lisp/cl-macs.el"))
         (files (list shim gv macroexp macroexp gv lazy))
         (eager (nelisp-bootstrap--eager-runtime-files files)))
    (should (equal (cl-subseq eager 0 3) (list macroexp gv shim)))
    (should (= (cl-count macroexp eager :test #'equal) 1))
    (should (= (cl-count gv eager :test #'equal) 1))
    (should-not (member lazy eager))))

(ert-deftest emacs-gv-setter-bootstrap/bundler-rejects-missing-provider ()
  "A missing GNU provider must fail bundle preparation."
  (let ((shim (expand-file-name "emacs-parity-eieio.el"
                                (nelisp-bootstrap--src-dir))))
    (cl-letf (((symbol-function 'nelisp-bootstrap--vendor-source-file)
               (lambda (_) nil)))
      (should-error (nelisp-bootstrap--eager-runtime-files (list shim))
                    :type 'error))))

(ert-deftest emacs-gv-setter-bootstrap/declares-and-runs-setter ()
  "A GNU `gv-setter' declaration must install a working generalized place."
  (let* ((suffix (number-to-string (random 1000000)))
         (place (intern (concat "emacs-gv-setter-bootstrap-place-" suffix)))
         (setter (intern (concat "emacs-gv-setter-bootstrap-set-" suffix)))
         (cell (vector nil)))
    (should (assq 'gv-setter defun-declarations-alist))
    (fset setter (lambda (value) (aset cell 0 value)))
    (unwind-protect
        (progn
          (eval `(defun ,place ()
                   (declare (gv-setter ,setter))
                   (aref ',cell 0)))
          (let ((expanded (macroexpand (list 'setf (list place) 42))))
            (should (equal expanded (list setter 42)))
            (eval expanded))
          (should (= (aref cell 0) 42)))
      (fmakunbound setter)
      (when (fboundp place) (fmakunbound place))
      (put place 'gv-expander nil))))

(provide 'emacs-gv-setter-bootstrap-test)
;;; emacs-gv-setter-bootstrap-test.el ends here
