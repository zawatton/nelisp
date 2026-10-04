;;; emacs-cc-bare-symbol-1-test.el --- bare-symbol-p contract -*- lexical-binding: t; -*-

(require 'ert)

(defconst emacs-cc-bare-symbol-1-test--root
  (expand-file-name "../.."
                    (file-name-directory (or load-file-name buffer-file-name))))

(add-to-list 'load-path
             (expand-file-name "packages/nelisp-emacs-foundation/src"
                               emacs-cc-bare-symbol-1-test--root))
(load "emacs-cc-bare-symbol-1" nil t)

(ert-deftest emacs-cc-bare-symbol-1/symbol-and-nonsymbol-cases ()
  (let ((objects (list 'plain nil t :keyword (make-symbol "uninterned")
                       17 1.5 "text" '(plain) [plain]
                       (make-hash-table :test 'eq)))
        (expected '(t t t t t nil nil nil nil nil nil)))
    (should (equal expected (mapcar #'bare-symbol-p objects)))))

(ert-deftest emacs-cc-bare-symbol-1/symbol-with-position-is-not-bare ()
  (when (and (fboundp 'position-symbol) (fboundp 'symbol-with-pos-p))
    (let ((object (position-symbol 'located 3)))
      (should (symbol-with-pos-p object))
      (should-not (bare-symbol-p object)))))

(ert-deftest emacs-cc-bare-symbol-1/arity-errors-name-the-primitive ()
  (should (equal '(wrong-number-of-arguments (bare-symbol-p 0))
                 (condition-case error-data (bare-symbol-p)
                   (error (list (car error-data) (cdr error-data))))))
  (should (equal '(wrong-number-of-arguments (bare-symbol-p 2))
                 (condition-case error-data (bare-symbol-p 'one 'two)
                   (error (list (car error-data) (cdr error-data)))))))

(ert-deftest emacs-cc-bare-symbol-1/fallback-matches-supported-symbol-values ()
  (let ((native-definition (symbol-function 'bare-symbol-p)))
    (unwind-protect
        (progn
          (fmakunbound 'bare-symbol-p)
          (load "emacs-cc-bare-symbol-1" nil t)
          (should (bare-symbol-p 'plain))
          (should (bare-symbol-p (make-symbol "uninterned")))
          (should (bare-symbol-p nil))
          (should-not (bare-symbol-p "text"))
          (should-not (bare-symbol-p (position-symbol 'located 3)))
          (should (equal '(wrong-number-of-arguments (bare-symbol-p 0))
                         (condition-case error-data (bare-symbol-p)
                           (error (list (car error-data)
                                        (cdr error-data)))))))
      (fset 'bare-symbol-p native-definition))))

(provide 'emacs-cc-bare-symbol-1-test)
;;; emacs-cc-bare-symbol-1-test.el ends here
