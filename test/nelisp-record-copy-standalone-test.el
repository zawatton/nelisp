;;; nelisp-record-copy-standalone-test.el --- record copy smoke  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; EIEIO stores classes and instances as standalone records.  `copy-sequence'
;; must preserve the record tag and payload slots because EIEIO's default
;; `make-instance' method copies a class default object before initialization.

;;; Code:

(require 'ert)

(defconst nelisp-record-copy-standalone-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root derived from this test file.")

(ert-deftest nelisp-record-copy-standalone/copies-record-payload ()
  "Copy a record without signalling unsupported-sequence or crashing."
  (let ((binary (expand-file-name "target/nelisp"
                                 nelisp-record-copy-standalone-test--root)))
    (unless (file-executable-p binary)
      (ert-skip "target/nelisp is not built; standalone-reader owns it"))
    (with-temp-buffer
      (let ((rc
             (call-process
              binary nil t nil "--eval"
              "(let* ((source (nelisp--make-record 'record-copy-regression 1 2)) (copy (copy-sequence source))) (if (and (recordp copy) (not (eq source copy)) (eq (nelisp--record-type copy) 'record-copy-regression) (= (nelisp--record-length copy) 3) (= (nelisp--record-ref copy 0) 1) (= (nelisp--record-ref copy 1) 2)) 'pass 'fail))")))
        (should (= rc 0))
        (should (equal (string-trim (buffer-string)) "pass"))))))

(provide 'nelisp-record-copy-standalone-test)

;;; nelisp-record-copy-standalone-test.el ends here
