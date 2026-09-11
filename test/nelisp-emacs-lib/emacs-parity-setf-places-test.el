;;; emacs-parity-setf-places-test.el --- generalized place regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defconst emacs-parity-setf-places-test--source
  (expand-file-name "../src/emacs-parity-setf-places.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest emacs-parity-setf-places/standalone-if-places-and-host-guard ()
  "Install the shim only with the standalone marker and preserve host `setf'."
  (let ((host-setf (symbol-function 'setf))
        (old-eval (and (fboundp 'nelisp--eval-source-string)
                       (symbol-function 'nelisp--eval-source-string)))
        (old-write (and (fboundp 'nl-write-file)
                        (symbol-function 'nl-write-file)))
        (old-record (and (fboundp 'nelisp--record-set)
                         (symbol-function 'nelisp--record-set))))
    (unwind-protect
        (progn
          ;; An eval helper alone must not replace GNU's macro.
          (fset 'nelisp--eval-source-string (lambda (&rest _) nil))
          (when (fboundp 'nl-write-file) (fmakunbound 'nl-write-file))
          (load emacs-parity-setf-places-test--source nil nil)
          (should (eq host-setf (symbol-function 'setf)))
          ;; Add the standalone marker and exercise the actual shim.
          (fset 'nl-write-file (lambda (&rest _) nil))
          (load emacs-parity-setf-places-test--source nil nil)
          (let ((flag nil) (a 1) (b 2) (calls 0))
            (cl-labels ((cond-once () (setq calls (1+ calls)) flag))
              (setf (if (cond-once) a b) 7))
            (should (equal '(1 7) (list a b)))
            (should (= calls 1)))
          (let ((default-value 3))
            (set-default 'emacs-parity-setf-places-test--default 3)
            (setf (if t (default-value
                         'emacs-parity-setf-places-test--default)
                      default-value)
                  9)
            (should (= 9 (default-value
                          'emacs-parity-setf-places-test--default)))))
      (fset 'setf host-setf)
      (if old-eval (fset 'nelisp--eval-source-string old-eval)
        (fmakunbound 'nelisp--eval-source-string))
      (if old-write (fset 'nl-write-file old-write)
        (fmakunbound 'nl-write-file))
      (if old-record (fset 'nelisp--record-set old-record)
        (when (fboundp 'nelisp--record-set)
          (fmakunbound 'nelisp--record-set))))))

(provide 'emacs-parity-setf-places-test)
