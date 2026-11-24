;;; charprop-test.el --- Unicode property registry tests -*- lexical-binding: t; -*-

;;; Code:

(require 'nemacs-test-paths
         (expand-file-name "../../../test/emacs-lib/nemacs-test-paths"
                           (file-name-directory (or load-file-name buffer-file-name))))
(require 'ert)

(load (nemacs-test-paths-source "charprop.el") nil t)

(ert-deftest charprop-test/generated-registry ()
  (should (featurep 'charprop))
  (should (get 'bidi-class 'char-code-property-documentation))
  (should (equal (get-char-code-property ?é 'decomposition) '(101 769))))

(ert-deftest charprop-test/lazy-table ()
  (let ((char-code-property-alist nil))
    (define-char-code-property 'general-category "uni-category.el")
    (should (equal (cdr (assq 'general-category char-code-property-alist))
                   "uni-category.el"))
    (should (eq (get-char-code-property ?A 'general-category) 'Lu))
    (should (char-table-p (cdr (assq 'general-category char-code-property-alist))))))

(ert-deftest charprop-test/anonymous-property ()
  (unwind-protect
      (progn
        (should (eq (put-char-code-property ?x 'charprop-test-property 'example)
                    'example))
        (should (eq (get-char-code-property ?x 'charprop-test-property) 'example)))
    (put-char-code-property ?x 'charprop-test-property nil)))

(ert-deftest charprop-test/description ()
  (should (equal (char-code-property-description 'general-category 'Lu)
                 "Letter, Uppercase"))
  (should-not (char-code-property-description 'numeric-value 42)))

(provide 'charprop-test)
