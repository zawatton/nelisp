;;; nelisp-stdlib-hash-test.el --- Host ERT for pure-Elisp hash tables -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(unless (fboundp 'nelisp--make-record)
  (defun nelisp--make-record (tag &rest slots)
    (apply #'record tag slots)))

(unless (fboundp 'nelisp--record-length)
  (defun nelisp--record-length (obj)
    (1- (length obj))))

(unless (fboundp 'nelisp--record-type)
  (defun nelisp--record-type (obj)
    (aref obj 0)))

(unless (fboundp 'nelisp--record-ref)
  (defun nelisp--record-ref (obj index)
    (aref obj (1+ index))))

(unless (fboundp 'nelisp--record-set)
  (defun nelisp--record-set (obj index value)
    (aset obj (1+ index) value)))

(defvar nelisp-stdlib-hash-test--builtin-make-hash-table
  (symbol-function 'make-hash-table))
(defvar nelisp-stdlib-hash-test--builtin-hash-table-p
  (symbol-function 'hash-table-p))
(defvar nelisp-stdlib-hash-test--builtin-puthash
  (symbol-function 'puthash))
(defvar nelisp-stdlib-hash-test--builtin-gethash
  (symbol-function 'gethash))
(defvar nelisp-stdlib-hash-test--builtin-remhash
  (symbol-function 'remhash))
(defvar nelisp-stdlib-hash-test--builtin-clrhash
  (symbol-function 'clrhash))

(load-file (expand-file-name "../lisp/nelisp-stdlib-hash.el"
                             (file-name-directory
                              (or load-file-name buffer-file-name))))

(defalias 'nelisp-stdlib-hash-test--make-hash-table
  (symbol-function 'make-hash-table))
(defalias 'nelisp-stdlib-hash-test--hash-table-p
  (symbol-function 'hash-table-p))
(defalias 'nelisp-stdlib-hash-test--puthash
  (symbol-function 'puthash))
(defalias 'nelisp-stdlib-hash-test--gethash
  (symbol-function 'gethash))
(defalias 'nelisp-stdlib-hash-test--remhash
  (symbol-function 'remhash))
(defalias 'nelisp-stdlib-hash-test--clrhash
  (symbol-function 'clrhash))

(fset 'make-hash-table nelisp-stdlib-hash-test--builtin-make-hash-table)
(fset 'hash-table-p nelisp-stdlib-hash-test--builtin-hash-table-p)
(fset 'puthash nelisp-stdlib-hash-test--builtin-puthash)
(fset 'gethash nelisp-stdlib-hash-test--builtin-gethash)
(fset 'remhash nelisp-stdlib-hash-test--builtin-remhash)
(fset 'clrhash nelisp-stdlib-hash-test--builtin-clrhash)

(defun nelisp-stdlib-hash-test--count (table)
  "Return TABLE's live entry count."
  (nelisp--record-ref table 2))

(ert-deftest nelisp-stdlib-hash-test-equal-keys-coalesce ()
  (let ((table (nelisp-stdlib-hash-test--make-hash-table :test 'equal))
        (key-a (list 'x 'y))
        (key-b (list 'x 'y)))
    (nelisp-stdlib-hash-test--puthash key-a 10 table)
    (nelisp-stdlib-hash-test--puthash key-b 20 table)
    (should (= (nelisp-stdlib-hash-test--gethash key-a table) 20))
    (should (= (nelisp-stdlib-hash-test--gethash key-b table) 20))
    (should (= (nelisp-stdlib-hash-test--count table) 1))))

(ert-deftest nelisp-stdlib-hash-test-eql-floats ()
  (let ((table (nelisp-stdlib-hash-test--make-hash-table :test 'eql)))
    (nelisp-stdlib-hash-test--puthash 1.5 'value table)
    (should (eq (nelisp-stdlib-hash-test--gethash 1.5 table) 'value))
    (should (eq (nelisp-stdlib-hash-test--gethash (+ 1.0 0.5) table) 'value))))

(ert-deftest nelisp-stdlib-hash-test-string-equal-keys ()
  (let ((table (nelisp-stdlib-hash-test--make-hash-table :test 'string-equal)))
    (nelisp-stdlib-hash-test--puthash (concat "ab" "c") 7 table)
    (should (= (nelisp-stdlib-hash-test--gethash "abc" table) 7))
    (should (= (nelisp-stdlib-hash-test--count table) 1))))

(ert-deftest nelisp-stdlib-hash-test-remhash-preserves-colliding-siblings ()
  (let ((table (nelisp-stdlib-hash-test--make-hash-table :test 'eq :size 8))
        (keys nil))
    (dotimes (i 40)
      (let ((key (intern (format "nelisp-stdlib-hash-test-k%d" i))))
        (push key keys)
        (nelisp-stdlib-hash-test--puthash key i table)))
    (should (nelisp-stdlib-hash-test--remhash (nth 11 keys) table))
    (should-not (nelisp-stdlib-hash-test--gethash (nth 11 keys) table))
    (dolist (key keys)
      (unless (eq key (nth 11 keys))
        (should (integerp (nelisp-stdlib-hash-test--gethash key table nil)))))))

(ert-deftest nelisp-stdlib-hash-test-many-string-keys ()
  (let ((table (nelisp-stdlib-hash-test--make-hash-table :test 'equal :size 8)))
    (dotimes (i 200)
      (nelisp-stdlib-hash-test--puthash (format "key-%d" i) i table))
    (dotimes (i 200)
      (should (= (nelisp-stdlib-hash-test--gethash (format "key-%d" i) table) i)))
    (should (= (nelisp-stdlib-hash-test--count table) 200))))

(provide 'nelisp-stdlib-hash-test)

;;; nelisp-stdlib-hash-test.el ends here
