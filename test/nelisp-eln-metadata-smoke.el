;;; nelisp-eln-metadata-smoke.el --- inspect genuine GNU .eln metadata -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(let* ((source-root (getenv "NELISP_ELN_METADATA_SOURCE_ROOT"))
       (loader-root (getenv "NELISP_ELN_METADATA_LOADER_SOURCE_ROOT"))
       (eln-path (getenv "NELISP_ELN_METADATA_ELN")))
  (unless (and source-root loader-root eln-path)
    (error "metadata smoke requires source roots and .eln path"))
  (load (concat source-root "/lisp/nelisp-eln-abi.el"))
  (load (concat source-root "/packages/nl-ffi/src/nl-ffi-memory.el"))
  (load (concat source-root "/packages/nl-ffi/src/nl-ffi.el"))
  (load (concat loader-root "/packages/nl-ffi/src/nl-ffi-loader.el"))
  (load (concat source-root "/lisp/nelisp-eln-metadata.el"))
  (let* ((handle (nl-ffi-loader-open eln-path))
         (metadata (nelisp-eln-metadata-read handle))
         (docs (prin1-to-string (plist-get metadata :function-docs)))
         (data (plist-get metadata :data-relocations))
         (i 0)
         (identity-ok nil))
    (unless (equal (plist-get metadata :abi-hash)
                   (plist-get nelisp-eln-abi-gnu-31-1-x86_64
                              :producer-abi-hash))
      (error "metadata hash did not match the pinned profile"))
    (unless (and (vectorp (plist-get metadata :data-relocations))
                 (vectorp (plist-get metadata :ephemeral-data-relocations)))
      (error "metadata relocation payloads are not vectors"))
    (unless (listp (plist-get metadata :optimization-qualities))
      (error "GNU optimization qualities are not a list"))
    (unless (or (hash-table-p (plist-get metadata :function-docs))
                (vectorp (plist-get metadata :function-docs)))
      (error "GNU function docs are not a hash table or vector"))
    (unless (string-match-p "雪 café" docs)
      (error "Unicode function doc was not decoded correctly: %s" docs))
    ;; The fixture stores two same-name uninterned symbols and one shared
    ;; read-label symbol in a real compiler data relocation vector.
    (while (and (< i (length data)) (not identity-ok))
      (let ((row (aref data i)))
        (when (and (vectorp row) (= (length row) 4)
                   (symbolp (aref row 0)) (symbolp (aref row 1))
                   (symbolp (aref row 2)) (symbolp (aref row 3))
                   (equal (symbol-name (aref row 0)) "eln-reader-x")
                   (equal (symbol-name (aref row 1)) "eln-reader-x")
                   (equal (symbol-name (aref row 2)) "eln-reader-y")
                   (equal (symbol-name (aref row 3)) "eln-reader-y"))
          (setq identity-ok
                (and (not (eq (aref row 0) (aref row 1)))
                     (eq (aref row 2) (aref row 3))
                     (null (intern-soft "eln-reader-x"))
                     (null (intern-soft "eln-reader-y")))))
        (setq i (1+ i))))
    (unless identity-ok
      (error "uninterned relocation identity did not survive metadata reading"))
    (princ "NELISP-ELN-METADATA-PASS\n")))

;;; nelisp-eln-metadata-smoke.el ends here
