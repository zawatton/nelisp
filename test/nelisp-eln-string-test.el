;;; nelisp-eln-string-test.el --- GNU Lisp_String view codec tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nelisp-eln-string)

(defvar nelisp-eln-string-test--memory nil)
(defvar nelisp-eln-string-test--next-base 65536)
(defvar nelisp-eln-string-test--read-count 0)
(defvar nelisp-eln-string-test--alloc-count 0)
(defvar nelisp-eln-string-test--release-count 0)
(defvar nelisp-eln-string-test--release-fails nil)
(defvar nelisp-eln-string-test--write-fails nil)
(defconst nelisp-eln-string-test--fake-owner-marker 'fake-mmap-owner)

(defun nelisp-eln-string-test--allocate (_size)
  (setq nelisp-eln-string-test--alloc-count
        (1+ nelisp-eln-string-test--alloc-count))
  (let ((owner (vector nelisp-eln-string-test--fake-owner-marker
                       nelisp-eln-string-test--next-base 0 t)))
    (setq nelisp-eln-string-test--next-base
          (+ nelisp-eln-string-test--next-base #x10000))
    owner))

(defun nelisp-eln-string-test--address (owner)
  (unless (and (vectorp owner) (eq (aref owner 0)
                                  nelisp-eln-string-test--fake-owner-marker)
               (aref owner 3))
    (error "fake mmap owner is closed"))
  (aref owner 1))

(defun nelisp-eln-string-test--release (owner)
  (setq nelisp-eln-string-test--release-count
        (1+ nelisp-eln-string-test--release-count))
  (when nelisp-eln-string-test--release-fails
    (error "injected munmap failure"))
  (aset owner 3 nil)
  t)

(defun nelisp-eln-string-test--ptr-write-u8 (address offset value)
  (puthash (+ address offset) value nelisp-eln-string-test--memory))

(defun nelisp-eln-string-test--ptr-read-u8 (address offset)
  (setq nelisp-eln-string-test--read-count
        (1+ nelisp-eln-string-test--read-count))
  (gethash (+ address offset) nelisp-eln-string-test--memory 0))

(defun nelisp-eln-string-test--ptr-write-u64 (address offset value)
  (when nelisp-eln-string-test--write-fails
    (error "injected external write failure"))
  (puthash (+ address offset) value nelisp-eln-string-test--memory))

(defun nelisp-eln-string-test--ptr-read-u64 (address offset)
  (gethash (+ address offset) nelisp-eln-string-test--memory 0))

(defun nelisp-eln-string-test--with-memory (function)
  (let ((symbols '(nl-ffi-memory-allocate nl-ffi-memory-address
                   nl-ffi-memory-release ptr-read-u8 ptr-write-u8
                   ptr-read-u64 ptr-write-u64))
        saved)
    (setq nelisp-eln-string-test--memory (make-hash-table :test 'eql)
          nelisp-eln-string-test--read-count 0
          nelisp-eln-string-test--alloc-count 0
          nelisp-eln-string-test--release-count 0
          nelisp-eln-string-test--release-fails nil
          nelisp-eln-string-test--write-fails nil)
    (dolist (symbol symbols)
      (push (cons symbol (and (fboundp symbol) (symbol-function symbol))) saved))
    (unwind-protect
        (progn
          (fset 'nl-ffi-memory-allocate #'nelisp-eln-string-test--allocate)
          (fset 'nl-ffi-memory-address #'nelisp-eln-string-test--address)
          (fset 'nl-ffi-memory-release #'nelisp-eln-string-test--release)
          (fset 'ptr-read-u8 #'nelisp-eln-string-test--ptr-read-u8)
          (fset 'ptr-write-u8 #'nelisp-eln-string-test--ptr-write-u8)
          (fset 'ptr-read-u64 #'nelisp-eln-string-test--ptr-read-u64)
          (fset 'ptr-write-u64 #'nelisp-eln-string-test--ptr-write-u64)
          (funcall function))
      (dolist (entry saved)
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))))

(defun nelisp-eln-string-test--bytes (base offset length)
  (let ((index 0) (bytes nil))
    (while (< index length)
      (push (ptr-read-u8 base (+ offset index)) bytes)
      (setq index (1+ index)))
    (nreverse bytes)))

(ert-deftest nelisp-eln-string/layout-and-byte-preservation ()
  (nelisp-eln-string-test--with-memory
   (lambda ()
     (dolist (string (list "ASCII" "雪 café" (string 65 0 66)
                           (unibyte-string 0 128 255)))
       (let* ((owner (nelisp-eln-string-allocate string))
              (base (nelisp-eln-string-address owner))
              (data (+ base 32))
              (unibyte (not (multibyte-string-p string)))
              (bytes (if unibyte string
                       (encode-coding-string string 'utf-8 t))))
         (unwind-protect
             (progn
               (should (= (ptr-read-u64 base 0) (length string)))
               (should (= (ptr-read-u64 base 8)
                          (if unibyte #xffffffffffffffff
                            (string-bytes bytes))))
               (should (= (ptr-read-u64 base 16) 0))
               (should (= (ptr-read-u64 base 24) data))
               (should (equal (nelisp-eln-string-test--bytes
                               data 0 (string-bytes bytes))
                              (append bytes nil)))
               (should (= (ptr-read-u8 data (string-bytes bytes)) 0))
               (should (equal (nelisp-eln-string-read owner) string)))
           (nelisp-eln-string-release owner)))))))

(ert-deftest nelisp-eln-string/sync-preserves-canonical-identity-after-gc ()
  (nelisp-eln-string-test--with-memory
   (lambda ()
     (let* ((original (copy-sequence "aあ"))
            (owner (nelisp-eln-string-allocate original))
            (base (nelisp-eln-string-address owner))
            (data (+ base 32)))
       (unwind-protect
           (progn
             (garbage-collect)
             (ptr-write-u8 data 0 ?x)
             (should (equal (nelisp-eln-string-read owner) "xあ"))
             (should (eq (nelisp-eln-string-sync owner) original))
             (should (equal original "xあ")))
         (nelisp-eln-string-release owner))))))

(ert-deftest nelisp-eln-string/sync-preserves-unibyte-values-and-identity ()
  (nelisp-eln-string-test--with-memory
   (lambda ()
     (let* ((original (copy-sequence (unibyte-string 0 128 255)))
            (owner (nelisp-eln-string-allocate original))
            (base (nl-ffi-memory-address (aref owner 2)))
            (data (+ base 32)))
       (unwind-protect
           (progn
             (ptr-write-u8 data 0 255)
             (ptr-write-u8 data 1 0)
             (ptr-write-u8 data 2 128)
             (should (eq (nelisp-eln-string-sync owner) original))
             (should (equal (append original nil) '(255 0 128)))
             (should-not (multibyte-string-p original)))
         (nelisp-eln-string-release owner))))))

(ert-deftest nelisp-eln-string/rejects-nonascii-sync-before-changing-original ()
  (nelisp-eln-string-test--with-memory
   (lambda ()
     (let* ((original (copy-sequence "雪"))
            (owner (nelisp-eln-string-allocate original))
            (base (nl-ffi-memory-address (aref owner 2)))
            (bytes (encode-coding-string "雲" 'utf-8 t))
            (index 0))
       (unwind-protect
           (progn
             (while (< index (length bytes))
               (ptr-write-u8 (+ base 32) index (aref bytes index))
               (setq index (1+ index)))
             (should-error (nelisp-eln-string-sync owner)
                           :type 'nelisp-eln-string-error)
             (should (equal original "雪")))
         (nelisp-eln-string-release owner))))))

(ert-deftest nelisp-eln-string/rejects-invalid-native-utf8-before-sync ()
  (nelisp-eln-string-test--with-memory
   (lambda ()
     (let* ((original (copy-sequence "éa"))
            (owner (nelisp-eln-string-allocate original))
            (base (nl-ffi-memory-address (aref owner 2))))
       (unwind-protect
           (progn
             (ptr-write-u8 (+ base 32) 0 #xff)
             (let ((condition
                    (condition-case data
                        (progn (nelisp-eln-string-sync owner) nil)
                      (nelisp-eln-string-error data))))
               (should condition)
               (should (eq (cadr condition)
                           'utf8-decoded-non-unicode-character)))
             (should (equal original "éa")))
         (nelisp-eln-string-release owner))))))

(ert-deftest nelisp-eln-string/sync-validates-owner-before-reading-fields ()
  (nelisp-eln-string-test--with-memory
   (lambda ()
     (should-error (nelisp-eln-string-sync nil)
                   :type 'nelisp-eln-string-error))))

(ert-deftest nelisp-eln-string/rejects-corrupt-pointer-before-data-read ()
  (nelisp-eln-string-test--with-memory
   (lambda ()
     (let* ((owner (nelisp-eln-string-allocate "safe"))
            (base (nl-ffi-memory-address (aref owner 2))))
       (unwind-protect
           (progn
             (ptr-write-u64 base 24 0)
             (let ((reads nelisp-eln-string-test--read-count))
               (should-error (nelisp-eln-string-read owner)
                             :type 'nelisp-eln-string-error)
               (should (= nelisp-eln-string-test--read-count reads))))
         (nelisp-eln-string-release owner))))))

(ert-deftest nelisp-eln-string/rejects-shape-change-before-sync-mutation ()
  (nelisp-eln-string-test--with-memory
   (lambda ()
     (let* ((original (copy-sequence "ab"))
            (owner (nelisp-eln-string-allocate original))
            (base (nl-ffi-memory-address (aref owner 2))))
       (unwind-protect
           (progn
             (ptr-write-u64 base 0 3)
             (should-error (nelisp-eln-string-sync owner)
                           :type 'nelisp-eln-string-error)
             (should (equal original "ab")))
         (nelisp-eln-string-release owner))))))

(ert-deftest nelisp-eln-string/preflight-rejects-properties-and-extended-characters ()
  (nelisp-eln-string-test--with-memory
   (lambda ()
     (should-error
      (nelisp-eln-string-allocate (propertize "x" 'face 'bold))
      :type 'nelisp-eln-string-error)
     (should-error (nelisp-eln-string-allocate (string #x110000))
                   :type 'nelisp-eln-string-error)
     (should (= nelisp-eln-string-test--alloc-count 0)))))

(ert-deftest nelisp-eln-string/allocation-write-failure-releases-mapping ()
  (nelisp-eln-string-test--with-memory
   (lambda ()
     (setq nelisp-eln-string-test--write-fails t)
     (should-error (nelisp-eln-string-allocate "x"))
     (should (= nelisp-eln-string-test--alloc-count 1))
     (should (= nelisp-eln-string-test--release-count 1)))))

(ert-deftest nelisp-eln-string/release-is-closed-on-success-and-retryable-on-error ()
  (nelisp-eln-string-test--with-memory
   (lambda ()
     (let ((owner (nelisp-eln-string-allocate "x")))
       (setq nelisp-eln-string-test--release-fails t)
       (should-error (nelisp-eln-string-release owner))
       (should (integerp (nelisp-eln-string-address owner)))
       (setq nelisp-eln-string-test--release-fails nil)
       (should (nelisp-eln-string-release owner))
       (should-error (nelisp-eln-string-address owner)
                     :type 'nelisp-eln-string-error)
       (should-error (nelisp-eln-string-release owner)
                     :type 'nelisp-eln-string-error)))))

(ert-deftest nelisp-eln-string/plans-preflight-without-write-and-stage-sync-directions ()
  (nelisp-eln-string-test--with-memory
   (lambda ()
     (let* ((original (copy-sequence "abc"))
            (input-plan (nelisp-eln-string-prepare original))
            (owner (nelisp-eln-string-allocate-prepared input-plan))
            (base (nelisp-eln-string-address owner))
            (data (+ base 32))
            (sync-to (progn (aset original 0 ?x)
                            (nelisp-eln-string-prepare-sync-to owner))))
       (unwind-protect
           (progn
             (should (= nelisp-eln-string-test--alloc-count 1))
             ;; Preparation stages bytes without mutating the external view.
             (should (= (ptr-read-u8 data 0) ?a))
             (nelisp-eln-string-validate-sync-plan sync-to)
             (nelisp-eln-string-commit-sync-plan sync-to)
             (should (= (ptr-read-u8 data 0) ?x))
             ;; Prepare native-to-canonical without changing the string yet.
             (ptr-write-u8 data 1 ?Y)
             (let ((sync-from (nelisp-eln-string-prepare-sync-from owner)))
               (should (equal original "xbc"))
               (nelisp-eln-string-validate-sync-plan sync-from)
               (nelisp-eln-string-commit-sync-plan sync-from)
               (should (eq (aref owner 1) original))
               (should (equal original "xYc"))))
         (nelisp-eln-string-release owner))))))

(ert-deftest nelisp-eln-string/sync-from-plan-rejects-properties-added-after-prepare ()
  (nelisp-eln-string-test--with-memory
   (lambda ()
     (let* ((original (copy-sequence "abc"))
            (owner (nelisp-eln-string-allocate original))
            (plan (nelisp-eln-string-prepare-sync-from owner)))
       (unwind-protect
           (progn
             (put-text-property 1 2 'face 'bold original)
             (should-error (nelisp-eln-string-validate-sync-plan plan)
                           :type 'nelisp-eln-string-error)
             (should (eq (get-text-property 1 'face original) 'bold)))
         (nelisp-eln-string-release owner))))))

(provide 'nelisp-eln-string-test)

;;; nelisp-eln-string-test.el ends here
