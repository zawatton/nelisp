;;; standalone-source-normalize-test.el --- tests for standalone source rewrites  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'standalone-source-normalize)

(ert-deftest standalone-source-normalize-test/expands-setq-local ()
  (should
   (equal
    (standalone-source-normalize-form
     '(setq-local foo 1 bar (+ foo 2)))
    '(progn
       (set (make-local-variable 'foo) 1)
       (set (make-local-variable 'bar) (+ foo 2))))))

(ert-deftest standalone-source-normalize-test/preserves-quoted-data ()
  (should
   (equal
    (standalone-source-normalize-form
     '(quote (setq-local foo 1)))
    '(quote (setq-local foo 1)))))

(ert-deftest standalone-source-normalize-test/rewrites-inside-defun-body ()
  (should
   (equal
    (standalone-source-normalize-form
     '(defun demo ()
        (setq-local foo 1)
        foo))
    '(defun demo ()
       (set (make-local-variable 'foo) 1)
       foo))))

(ert-deftest standalone-source-normalize-test/leaves-malformed-setq-local ()
  (should
   (equal
    (standalone-source-normalize-form
     '(setq-local foo))
    '(setq-local foo))))

(ert-deftest standalone-source-normalize-test/caches-file-form-strings ()
  (let ((source (make-temp-file "standalone-source-normalize-" nil ".el"))
        (cache-dir (make-temp-file "standalone-source-normalize-cache-" t))
        standalone-source-normalize-cache-directory)
    (unwind-protect
        (progn
          (setq standalone-source-normalize-cache-directory cache-dir)
          (with-temp-file source
            (insert "(defvar cache-a 1)\n"))
          (should (equal (standalone-source-normalize-file-to-form-strings
                          source)
                         '("(defvar cache-a 1)")))
          (should (directory-files cache-dir nil "\\.elcache\\'"))
          (should (equal (standalone-source-normalize-file-to-form-strings
                          source)
                         '("(defvar cache-a 1)"))))
      (when (file-exists-p source)
        (delete-file source))
      (when (file-directory-p cache-dir)
        (delete-directory cache-dir t)))))

(ert-deftest standalone-source-normalize-test/cache-invalidates-on-change ()
  (let ((source (make-temp-file "standalone-source-normalize-" nil ".el"))
        (cache-dir (make-temp-file "standalone-source-normalize-cache-" t))
        standalone-source-normalize-cache-directory)
    (unwind-protect
        (progn
          (setq standalone-source-normalize-cache-directory cache-dir)
          (with-temp-file source
            (insert "(defvar cache-a 1)\n"))
          (standalone-source-normalize-file-to-form-strings source)
          (sleep-for 0.01)
          (with-temp-file source
            (insert "(defvar cache-b 2)\n"))
          (should (equal (standalone-source-normalize-file-to-form-strings
                          source)
                         '("(defvar cache-b 2)"))))
      (when (file-exists-p source)
        (delete-file source))
      (when (file-directory-p cache-dir)
        (delete-directory cache-dir t)))))

(ert-deftest standalone-source-normalize-test/corrupt-cache-is-miss ()
  (let ((source (make-temp-file "standalone-source-normalize-" nil ".el"))
        (cache-dir (make-temp-file "standalone-source-normalize-cache-" t))
        standalone-source-normalize-cache-directory)
    (unwind-protect
        (progn
          (setq standalone-source-normalize-cache-directory cache-dir)
          (with-temp-file source
            (insert "(defvar cache-a 1)\n"))
          (standalone-source-normalize-file-to-form-strings source)
          (with-temp-file (standalone-source-normalize--cache-file source)
            (insert "(:version"))
          (should (equal (standalone-source-normalize-file-to-form-strings
                          source)
                         '("(defvar cache-a 1)"))))
      (when (file-exists-p source)
        (delete-file source))
      (when (file-directory-p cache-dir)
        (delete-directory cache-dir t)))))

(ert-deftest standalone-source-normalize-test/splits-quoted-hash-table-defconst ()
  (let ((table (make-hash-table :test 'equal)))
    (puthash "a" 1 table)
    (puthash "b" '(2 3) table)
    (let ((forms (standalone-source-normalize-top-level-forms
                  (list 'defconst 'sample (list 'quote table)))))
      (should (= 3 (length forms)))
      (should (equal (car forms)
                     '(defconst sample
                        (make-hash-table :test 'equal))))
      (should (member '(puthash "a" 1 sample) (cdr forms)))
      (should (member '(puthash "b" '(2 3) sample) (cdr forms))))))

(provide 'standalone-source-normalize-test)

;;; standalone-source-normalize-test.el ends here
