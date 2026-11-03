;;; charprop-test.el --- tests for lightweight charprop facade  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;;; Code:

(require 'ert)

(load (expand-file-name
       "../src/charprop.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest charprop-test/feature-loads ()
  (should (featurep 'charprop))
  (should (fboundp 'charprop--define-char-code-property))
  (should (assq 'bidi-class charprop--registry))
  (should (assq 'lowercase charprop--registry)))

(ert-deftest charprop-test/lazy-tables-do-not-force-unicode-loads ()
  (should (stringp (nth 1 (assq 'name charprop--registry))))
  (should-not (charprop--unicode-property-table-internal 'name))
  (should-not (charprop--get-char-code-property ?A 'name)))

(ert-deftest charprop-test/define-and-get-vector-property ()
  (let ((table (make-vector 128 nil)))
    (aset table ?A 'letter-a)
    (charprop--define-char-code-property 'nelisp-test-vector table
                                         "test vector property")
    (should (eq (charprop--get-char-code-property ?A 'nelisp-test-vector)
                'letter-a))
    (should (eq (charprop--unicode-property-table-internal
                 'nelisp-test-vector)
                table))))

(ert-deftest charprop-test/put-overrides-lazy-property ()
  (charprop--define-char-code-property 'nelisp-test-lazy "uni-test.el"
                                       "test lazy property")
  (should-not (charprop--get-char-code-property ?x 'nelisp-test-lazy))
  (should (eq (charprop--put-char-code-property ?x 'nelisp-test-lazy
                                                'override)
              'override))
  (should (eq (charprop--get-char-code-property ?x 'nelisp-test-lazy)
              'override))
  (should-not (charprop--unicode-property-table-internal 'nelisp-test-lazy)))

(ert-deftest charprop-test/description-is-lightweight ()
  (should (equal (charprop--char-code-property-description
                  'general-category 'Lu)
                 "Lu"))
  (should (equal (charprop--char-code-property-description
                  'numeric-value 42)
                 "42"))
  (should-not (charprop--char-code-property-description
               'name nil)))

(provide 'charprop-test)

;;; charprop-test.el ends here
