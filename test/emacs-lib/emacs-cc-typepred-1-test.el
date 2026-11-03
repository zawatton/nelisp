;;; emacs-cc-typepred-1-test.el --- type predicate coverage -*- lexical-binding: t; -*-

(require 'ert)

(defconst emacs-cc-typepred-1-test--root
  (expand-file-name "../.."
                    (file-name-directory (or load-file-name buffer-file-name))))

(add-to-list 'load-path
             (expand-file-name "packages/nelisp-emacs-buffer-core/src"
                               emacs-cc-typepred-1-test--root))
(load "emacs-cc-typepred-1" nil t)

(ert-deftest emacs-cc-typepred-1/integer-or-marker-p-cases ()
  (should (integer-or-marker-p 0))
  (should (integer-or-marker-p 1208925819614629174706176))
  (should (integer-or-marker-p ?x))
  (with-temp-buffer
    (should (integer-or-marker-p (copy-marker 1 t))))
  (let ((unset (make-marker)))
    (should (integer-or-marker-p unset))
    (should-not (integer-or-marker-p 1.0))
    (should-not (integer-or-marker-p nil))
    (should-not (integer-or-marker-p "1"))))

(ert-deftest emacs-cc-typepred-1/number-or-marker-p-cases ()
  (should (number-or-marker-p 0))
  (should (number-or-marker-p 1.25))
  (should (number-or-marker-p 1208925819614629174706176))
  (should (number-or-marker-p (make-marker)))
  (with-temp-buffer
    (should (number-or-marker-p (copy-marker 1 t))))
  (should-not (number-or-marker-p nil))
  (should-not (number-or-marker-p 'symbol)))

(ert-deftest emacs-cc-typepred-1/vector-or-char-table-p-cases ()
  (should (vector-or-char-table-p []))
  (should (vector-or-char-table-p [a 1]))
  (should (vector-or-char-table-p (make-char-table 'test-category)))
  (should-not (vector-or-char-table-p (make-bool-vector 3 nil)))
  (should-not (vector-or-char-table-p "text"))
  (should-not (vector-or-char-table-p '(a b)))
  (should-not (vector-or-char-table-p (make-hash-table))))

(provide 'emacs-cc-typepred-1-test)
;;; emacs-cc-typepred-1-test.el ends here
