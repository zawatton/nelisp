;;; nelisp-eln-metadata-test.el --- tests for inert .eln metadata parsing -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nelisp-eln-metadata)

(defvar nelisp-eln-metadata-test--objects nil)
(defvar nelisp-eln-metadata-test--infos nil)
(defvar nelisp-eln-metadata-test--reads nil)
(defvar nelisp-eln-metadata-test--info-requests nil)

(defun nelisp-eln-metadata-test--pack-length (length)
  (apply #'unibyte-string
         (mapcar (lambda (shift) (logand 255 (ash length (- shift))))
                 '(0 8 16 24 32 40 48 56))))

(defun nelisp-eln-metadata-test--static-object (text)
  (let* ((payload (concat (encode-coding-string text 'utf-8-unix)
                          (unibyte-string 0)))
         (length (length payload)))
    (concat (nelisp-eln-metadata-test--pack-length length) payload)))

(defun nelisp-eln-metadata-test--set-blob (name text)
  (let ((bytes (nelisp-eln-metadata-test--static-object text)))
    (puthash name bytes nelisp-eln-metadata-test--objects)
    (puthash name (list :address #x1000 :size (length bytes)
                        :type 1 :binding 1 :source-path "fixture.eln"
                        :section-index 1)
             nelisp-eln-metadata-test--infos)))

(defun nelisp-eln-metadata-test--install-fixture ()
  (setq nelisp-eln-metadata-test--objects (make-hash-table :test 'equal)
        nelisp-eln-metadata-test--infos (make-hash-table :test 'equal)
        nelisp-eln-metadata-test--reads nil
        nelisp-eln-metadata-test--info-requests nil)
  (nelisp-eln-metadata-test--set-blob "freloc_hash_blob" "\"ba35c031\"")
  (nelisp-eln-metadata-test--set-blob "text_data_reloc_blob" "[nil 11]")
  (nelisp-eln-metadata-test--set-blob "text_data_reloc_eph_blob" "[22]")
  (nelisp-eln-metadata-test--set-blob "text_optim_qly_blob"
                                      "((native-comp-speed . 2))")
  (nelisp-eln-metadata-test--set-blob "text_data_fdoc_blob"
                                      "[\"Unicode: 雪 café\"]")
  (puthash "d_reloc" (list :address #x2000 :size 16 :type 1 :binding 1
                           :source-path "fixture.eln" :section-index 2)
           nelisp-eln-metadata-test--infos)
  (puthash "d_reloc_eph" (list :address #x3000 :size 8 :type 1 :binding 1
                               :source-path "fixture.eln" :section-index 3)
           nelisp-eln-metadata-test--infos))

(defun nelisp-eln-metadata-test--symbol-info (_handle name)
  (push name nelisp-eln-metadata-test--info-requests)
  (gethash name nelisp-eln-metadata-test--infos))

(defun nelisp-eln-metadata-test--read-bytes (_handle name offset length)
  (push (list name offset length) nelisp-eln-metadata-test--reads)
  (let ((bytes (gethash name nelisp-eln-metadata-test--objects)))
    (unless (and bytes (<= 0 offset) (<= 0 length)
                 (<= (+ offset length) (length bytes)))
      (error "test fixture refused invalid range"))
    (substring bytes offset (+ offset length))))

(defun nelisp-eln-metadata-test--with-api (function)
  (let ((old-info (and (fboundp 'nl-ffi-loader-symbol-info)
                       (symbol-function 'nl-ffi-loader-symbol-info)))
        (old-read (and (fboundp 'nl-ffi-loader-read-root-object-bytes)
                       (symbol-function 'nl-ffi-loader-read-root-object-bytes))))
    (nelisp-eln-metadata-test--install-fixture)
    (unwind-protect
        (progn
          (fset 'nl-ffi-loader-symbol-info
                #'nelisp-eln-metadata-test--symbol-info)
          (fset 'nl-ffi-loader-read-root-object-bytes
                #'nelisp-eln-metadata-test--read-bytes)
          (funcall function))
      (if old-info
          (fset 'nl-ffi-loader-symbol-info old-info)
        (fmakunbound 'nl-ffi-loader-symbol-info))
      (if old-read
          (fset 'nl-ffi-loader-read-root-object-bytes old-read)
        (fmakunbound 'nl-ffi-loader-read-root-object-bytes)))))

(ert-deftest nelisp-eln-metadata/valid-data-and-utf8-docs ()
  (nelisp-eln-metadata-test--with-api
   (lambda ()
     (let ((metadata (nelisp-eln-metadata-read '(:path "fixture.eln")))
           (reads (nreverse nelisp-eln-metadata-test--reads)))
       (should (equal (plist-get metadata :abi-hash) "ba35c031"))
       (should (equal (plist-get metadata :data-relocations) [nil 11]))
       (should (equal (plist-get metadata :ephemeral-data-relocations) [22]))
       (should (= (plist-get metadata :d-reloc-size) 16))
       (should (= (plist-get metadata :d-reloc-eph-size) 8))
       (should (string-match-p "雪 café"
                               (prin1-to-string
                                (plist-get metadata :function-docs))))
       (should (vectorp (plist-get metadata :function-docs)))
       (should (listp (plist-get metadata :optimization-qualities)))
       (should (equal (caar reads) "freloc_hash_blob"))
       (should (equal (car (nth 2 reads)) "text_data_reloc_blob"))))))

(ert-deftest nelisp-eln-metadata/rejects-bad-hash-before-other-payloads ()
  (nelisp-eln-metadata-test--with-api
   (lambda ()
     (nelisp-eln-metadata-test--set-blob "freloc_hash_blob" "\"deadbeef\"")
     (should-error (nelisp-eln-metadata-read '(:path "fixture.eln"))
                   :type 'nelisp-eln-metadata-error)
     (should (equal (delete-dups
                     (mapcar #'car (nreverse nelisp-eln-metadata-test--reads)))
                    '("freloc_hash_blob"))))))

(ert-deftest nelisp-eln-metadata/rejects-wrong-symbol-type-and-small-blob ()
  (nelisp-eln-metadata-test--with-api
   (lambda ()
     (puthash "freloc_hash_blob"
              (list :address #x1000 :size 16 :type 2)
              nelisp-eln-metadata-test--infos)
     (should-error (nelisp-eln-metadata-read '(:path "fixture.eln"))
                   :type 'nelisp-eln-metadata-error)
     (should-not nelisp-eln-metadata-test--reads)
     (nelisp-eln-metadata-test--install-fixture)
     (puthash "freloc_hash_blob"
              (list :address #x1000 :size 8 :type 1)
              nelisp-eln-metadata-test--infos)
     (should-error (nelisp-eln-metadata-read '(:path "fixture.eln"))
                   :type 'nelisp-eln-metadata-error)
     (should-not nelisp-eln-metadata-test--reads))))

(ert-deftest nelisp-eln-metadata/rejects-mismatched-length-and-missing-nul ()
  (nelisp-eln-metadata-test--with-api
   (lambda ()
     (let ((bytes (gethash "freloc_hash_blob"
                           nelisp-eln-metadata-test--objects)))
       (puthash "freloc_hash_blob"
                (concat (nelisp-eln-metadata-test--pack-length 1)
                        (substring bytes 8))
                nelisp-eln-metadata-test--objects))
     (should-error (nelisp-eln-metadata-read '(:path "fixture.eln"))
                   :type 'nelisp-eln-metadata-error)
     (nelisp-eln-metadata-test--install-fixture)
     (let* ((bytes (gethash "freloc_hash_blob"
                            nelisp-eln-metadata-test--objects))
            (bad (copy-sequence bytes)))
       (aset bad (1- (length bad)) 65)
       (puthash "freloc_hash_blob" bad nelisp-eln-metadata-test--objects))
     (should-error (nelisp-eln-metadata-read '(:path "fixture.eln"))
                   :type 'nelisp-eln-metadata-error))))

(ert-deftest nelisp-eln-metadata/rejects-trailing-form-and-small-reloc-storage ()
  (nelisp-eln-metadata-test--with-api
   (lambda ()
     (nelisp-eln-metadata-test--set-blob "text_data_reloc_blob" "[nil 11] [99]")
     (should-error (nelisp-eln-metadata-read '(:path "fixture.eln"))
                   :type 'nelisp-eln-metadata-error)
     (nelisp-eln-metadata-test--install-fixture)
     (puthash "d_reloc" (list :address #x2000 :size 8 :type 1)
              nelisp-eln-metadata-test--infos)
     (should-error (nelisp-eln-metadata-read '(:path "fixture.eln"))
                   :type 'nelisp-eln-metadata-error))))

(ert-deftest nelisp-eln-metadata/rejects-dependency-owned-storage-before-read ()
  (nelisp-eln-metadata-test--with-api
   (lambda ()
     (puthash "d_reloc"
              (list :address #x2000 :size 16 :type 1 :binding 1
                    :source-path "dependency.so" :section-index 2)
              nelisp-eln-metadata-test--infos)
     (should-error
      (nelisp-eln-metadata-read '(:path "fixture.eln"))
      :type 'nelisp-eln-metadata-error)
     (should-not (member "d_reloc"
                         (mapcar #'car (nreverse nelisp-eln-metadata-test--reads))))
     (should-not (member "d_reloc_eph" nelisp-eln-metadata-test--info-requests)))))

(ert-deftest nelisp-eln-metadata/preserves-hash-dollar-circle-and-gensym-identity ()
  (let* ((marker (nelisp-eln-metadata--make-hash-dollar-marker))
         (bytes (nelisp-eln-metadata-test--static-object
                 "[(#$ . 5) \"#$\"]"))
         (object (nelisp-eln-metadata--decode-and-read
                  (substring bytes 8) "synthetic" marker))
         (cycle (nelisp-eln-metadata--decode-and-read
                 (concat (encode-coding-string "#1=(a . #1#)" 'utf-8-unix)
                         (unibyte-string 0))
                 "circular" marker))
         (gensyms (nelisp-eln-metadata--decode-and-read
                   (concat (encode-coding-string "[#1=#:x #1#]" 'utf-8-unix)
                           (unibyte-string 0))
                   "gensym" marker)))
    (should (eq (car (aref object 0)) marker))
    (should (equal (aref object 1) "#$"))
    (should-not (eq (aref object 1) marker))
    (should (eq cycle (cdr cycle)))
    (should (eq (aref gensyms 0) (aref gensyms 1))))
  (nelisp-eln-metadata--check-hash-dollar-reader
   (nelisp-eln-metadata--make-hash-dollar-marker)))

(ert-deftest nelisp-eln-metadata/allows-nul-inside-printed-string ()
  (let* ((text (concat "[\"left" (string 0) "right\"]"))
         (bytes (nelisp-eln-metadata-test--static-object text))
         (object (nelisp-eln-metadata--decode-and-read
                  (substring bytes 8) "embedded-nul" nil)))
    (should (equal object (vector (concat "left" (string 0) "right"))))))

(ert-deftest nelisp-eln-metadata/rejects-reader-that-returns-hash-dollar-symbol ()
  (let ((old-reader (symbol-function 'read-from-string)))
    (unwind-protect
        (progn
          (fset 'read-from-string (lambda (_text) (cons (intern "#$") 2)))
          (let ((condition
                 (condition-case data
                     (progn
                       (nelisp-eln-metadata--check-hash-dollar-reader
                        (nelisp-eln-metadata--make-hash-dollar-marker))
                       nil)
                   (nelisp-eln-metadata-error data))))
            (should condition)
            (should (eq (caddr condition) 'reader-token-mismatch))))
      (fset 'read-from-string old-reader))))

(defmacro nelisp-eln-metadata-test--with-faking-reader (matches-text &rest body)
  "Run BODY with `read-from-string' faking the standalone's own
`#[...]' bug: any call whose TEXT contains MATCHES-TEXT raises
`(invalid-read-syntax \"#[\")' as the buggy ambient reader does; every
other call goes to the real reader, unchanged."
  (declare (indent 1))
  `(let ((old-reader (symbol-function 'read-from-string)))
     (unwind-protect
         (progn
           (fset 'read-from-string
                 (lambda (text &optional start end)
                   (if (string-search ,matches-text text)
                       (signal 'invalid-read-syntax (list "#["))
                     (funcall old-reader text start end))))
           ,@body)
       (fset 'read-from-string old-reader))))

(ert-deftest nelisp-eln-metadata/falls-back-when-ambient-reader-rejects-sharp-bracket ()
  ;; Simulates the standalone's own `#[...]' reader bug: a `#N#'
  ;; backreference occupying a byte-code literal's CODE field, to an
  ;; already fully-printed `#N=' label (GNU's native compiler
  ;; deduplicates `equal' instruction strings across otherwise unrelated
  ;; closures -- see nelisp-eln-metadata-bytecode-test.el).  The ambient
  ;; `read-from-string' raises `(invalid-read-syntax "#[")' on it; reading
  ;; must still succeed via the fallback reader.
  (nelisp-eln-metadata-test--with-api
   (lambda ()
     (nelisp-eln-metadata-test--set-blob
      "text_data_reloc_blob"
      "[#[257 #99=\"\\301\\300!\\207\" [V0 identity] 2] #[257 #99# [V0 car] 2]]")
     (nelisp-eln-metadata-test--with-faking-reader "#["
       (let* ((metadata (nelisp-eln-metadata-read '(:path "fixture.eln")))
              (data (plist-get metadata :data-relocations)))
         (should (byte-code-function-p (aref data 0)))
         (should (byte-code-function-p (aref data 1)))
         (should (equal (aref (aref data 0) 1) (aref (aref data 1) 1))))))))

(ert-deftest nelisp-eln-metadata/fallback-is-not-used-for-other-invalid-read-syntax ()
  ;; The fallback dispatch checks for the exact `"#["' tag the ambient
  ;; reader's own `#[...]' failures use; a differently-tagged
  ;; `invalid-read-syntax' (anything else the ambient reader might
  ;; legitimately reject) must still surface as an ordinary
  ;; `nelisp-eln-metadata-error', not silently retry.
  (nelisp-eln-metadata-test--with-api
   (lambda ()
     (let ((old-reader (symbol-function 'read-from-string)))
       (unwind-protect
           (progn
             (fset 'read-from-string
                   (lambda (text &optional start end)
                     (if (equal text "#$")
                         (funcall old-reader text start end)
                       (signal 'invalid-read-syntax '("#&")))))
             (let ((condition
                    (condition-case data
                        (progn (nelisp-eln-metadata-read '(:path "fixture.eln"))
                               nil)
                      (nelisp-eln-metadata-error data))))
               (should condition)
               (should (eq (caddr condition) 'invalid-printed-datum))))
         (fset 'read-from-string old-reader))))))

(provide 'nelisp-eln-metadata-test)

;;; nelisp-eln-metadata-test.el ends here
