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
          (when (fboundp 'nelisp--record-set)
            (fmakunbound 'nelisp--record-set))
          (load emacs-parity-setf-places-test--source nil nil)
          (should (eq host-setf (symbol-function 'setf)))
          ;; Add the standalone marker and exercise the actual shim.
          (fset 'nl-write-file (lambda (&rest _) nil))
          (load emacs-parity-setf-places-test--source nil nil)
          (should-not (eq host-setf (symbol-function 'setf)))
          ;; Quoted input is expanded after installation, even when this test
          ;; file itself was byte-compiled with the host macro installed.
          (should
           (equal '(1 7 1)
                  (eval '(let ((a 1) (b 2) (calls 0)
                               (nelisp-cl-macros--accessor-info nil))
                           (setf (if (progn (setq calls (1+ calls)) nil) a b) 7)
                           (list a b calls)) t)))
          (should
           (equal '(9 2)
                  (eval '(let ((a 1) (b 2)
                               (nelisp-cl-macros--accessor-info nil))
                           (setf (if t (if nil b a) b) 9)
                           (list a b)) t)))
          (should
           (equal '(9 3)
                  (eval '(let ((key (make-symbol "default-place"))
                               (local 3)
                               (nelisp-cl-macros--accessor-info nil))
                           (set key 1)
                           (setf (if t (default-value key) local) 9)
                           (list (default-value key) local)) t)))
          (should
           (equal '(8 (condition place index value))
                  (eval '(let ((v (vector 1)) (events nil)
                               (nelisp-cl-macros--accessor-info nil))
                           (setf (if (progn (push 'condition events) t)
                                     (aref (progn (push 'place events) v)
                                           (progn (push 'index events) 0))
                                   (error "unselected place"))
                                 (progn (push 'value events) 8))
                           (list (aref v 0) (nreverse events))) t))))
      (fset 'setf host-setf)
      (if old-eval (fset 'nelisp--eval-source-string old-eval)
        (fmakunbound 'nelisp--eval-source-string))
      (if old-write (fset 'nl-write-file old-write)
        (fmakunbound 'nl-write-file))
      (if old-record (fset 'nelisp--record-set old-record)
        (when (fboundp 'nelisp--record-set)
          (fmakunbound 'nelisp--record-set))))))

(provide 'emacs-parity-setf-places-test)
