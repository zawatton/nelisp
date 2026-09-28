;;; nelisp-eln-metadata.el --- validated GNU .eln serialized metadata -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Read-only metadata reader for one pinned GNU Emacs 31.1 x86_64 ELF
;; producer profile. It reads validated root-owned ELF objects through the
;; public FFI loader APIs, checks the ABI hash before touching other payloads,
;; and parses the printed Lisp data without evaluating it. GNU's printer uses
;; `print-circle', `print-gensym', and the `#$' load-file-name token; reading
;; binds a unique marker and enables circular-reader support. This does not
;; load GNU heap objects into NeLisp or claim native .eln runtime compatibility.
;; The current NeLisp reader discards text properties in #(...) literals, so
;; this reader does not claim those GNU printed-object properties are preserved.

;;; Code:

(require 'nelisp-eln-abi)
(require 'nl-ffi-loader)
(require 'nelisp-eln-metadata-bytecode)

(define-error 'nelisp-eln-metadata-error
  "Invalid GNU .eln metadata" 'nelisp-eln-abi-error)

(declare-function nl-ffi-loader-symbol-info "nl-ffi-loader"
                  (handle name))
(declare-function nl-ffi-loader-read-root-object-bytes "nl-ffi-loader"
                  (handle name offset length))

(defconst nelisp-eln-metadata--stt-object 1)
(defun nelisp-eln-metadata--fail (name reason &optional detail)
  (signal 'nelisp-eln-metadata-error (list name reason detail)))

(defun nelisp-eln-metadata--object-info (handle name symbol-info-function)
  (let ((info (funcall symbol-info-function handle name)))
    (unless (and (listp info)
                 (integerp (plist-get info :size))
                 (>= (plist-get info :size) 0)
                 (equal (plist-get info :type)
                        nelisp-eln-metadata--stt-object))
      (nelisp-eln-metadata--fail name 'not-root-object info))
    (unless (equal (plist-get info :source-path) (plist-get handle :path))
      (nelisp-eln-metadata--fail
       name 'not-root-object
       (list :source-path (plist-get info :source-path)
             :root-path (plist-get handle :path))))
    info))

(defun nelisp-eln-metadata--read-bytes
    (handle name offset length read-root-object-bytes-function)
  (let ((bytes (funcall read-root-object-bytes-function
                        handle name offset length)))
    (unless (and (stringp bytes)
                 (= (length bytes) length)
                 (= (string-bytes bytes) length))
      (nelisp-eln-metadata--fail name 'invalid-byte-count
                                 (list offset length bytes)))
    bytes))

(defun nelisp-eln-metadata--read-i64-le (bytes name)
  (unless (= (length bytes) 8)
    (nelisp-eln-metadata--fail name 'invalid-length-field bytes))
  (let ((value 0) (i 0))
    (while (< i 8)
      (setq value (+ value (ash (aref bytes i) (* i 8))))
      (setq i (1+ i)))
    (when (>= value (ash 1 63))
      (setq value (- value (ash 1 64))))
    value))

(defun nelisp-eln-metadata--trailing-whitespace-p (string start)
  (let ((i start) (n (length string)) (ok t))
    (while (and (< i n) ok)
      (unless (memq (aref string i) '(32 9 10 13))
        (setq ok nil))
      (setq i (1+ i)))
    ok))

(defun nelisp-eln-metadata--decode-and-read (bytes name hash-dollar-marker)
  (unless (and (> (length bytes) 0)
               (= (aref bytes (1- (length bytes))) 0))
    (nelisp-eln-metadata--fail name 'missing-terminating-nul))
  (let* ((body (substring bytes 0 (1- (length bytes))))
         (text (decode-coding-string body 'utf-8-unix))
         (roundtrip (encode-coding-string text 'utf-8-unix)))
    (unless (equal roundtrip body)
      (nelisp-eln-metadata--fail name 'invalid-utf8))
    (condition-case data
        (let* ((load-file-name hash-dollar-marker)
               (read-circle t)
               (read-result (read-from-string text))
               (object (car read-result))
               (end (cdr read-result)))
          (unless (nelisp-eln-metadata--trailing-whitespace-p text end)
            (nelisp-eln-metadata--fail name 'trailing-form))
          object)
      (nelisp-eln-metadata-error (signal (car data) (cdr data)))
      ;; The ambient reader's own `#[...]' handling can reject a
      ;; perfectly well-formed GNU byte-code literal when one of its
      ;; fields was printed as a `#N#' backreference to an already
      ;; fully-defined `#N=' label (see nelisp-eln-metadata-bytecode.el);
      ;; retry with the self-contained fallback reader before giving up.
      (invalid-read-syntax
       (if (equal (cadr data) "#[")
           (nelisp-eln-metadata--decode-and-read-bytecode-fallback
            text name hash-dollar-marker)
         (nelisp-eln-metadata--fail name 'invalid-printed-datum data)))
      (error (nelisp-eln-metadata--fail name 'invalid-printed-datum data)))))

(defun nelisp-eln-metadata--decode-and-read-bytecode-fallback
    (text name hash-dollar-marker)
  "Retry decoding TEXT with the `#[...]'-aware fallback reader.
Used only after the ambient reader raised `(invalid-read-syntax \"#[\")'."
  (let ((load-file-name hash-dollar-marker))
    (condition-case data
        (let* ((read-result (nelisp-eln-metadata-bytecode-read text))
               (object (car read-result))
               (end (cdr read-result)))
          (unless (nelisp-eln-metadata--trailing-whitespace-p text end)
            (nelisp-eln-metadata--fail name 'trailing-form))
          object)
      (nelisp-eln-metadata-error (signal (car data) (cdr data)))
      (error (nelisp-eln-metadata--fail name 'invalid-printed-datum data)))))

(defun nelisp-eln-metadata--make-hash-dollar-marker ()
  (let ((marker (make-string 2 0)))
    (aset marker 0 35)
    (aset marker 1 36)
    marker))

(defun nelisp-eln-metadata--check-hash-dollar-reader (marker)
  "Refuse readers that do not map GNU `#$' to bound `load-file-name'."
  (let ((value
         (condition-case data
             (let ((load-file-name marker) (read-circle t))
               (car (read-from-string "#$")))
           (error
            (nelisp-eln-metadata--fail "#$" 'reader-token-unsupported data)))))
    (unless (eq value marker)
      (nelisp-eln-metadata--fail "#$" 'reader-token-mismatch value))))

(defun nelisp-eln-metadata--read-static-object
    (handle name hash-dollar-marker symbol-info-function
     read-root-object-bytes-function)
  "Read one GNU `static_obj_t' blob named NAME from HANDLE."
  (let* ((info (nelisp-eln-metadata--object-info
                handle name symbol-info-function))
         (size (plist-get info :size)))
    (unless (>= size 9)
      (nelisp-eln-metadata--fail name 'static-object-too-small size))
    (let* ((header (nelisp-eln-metadata--read-bytes
                    handle name 0 8 read-root-object-bytes-function))
           (length (nelisp-eln-metadata--read-i64-le header name)))
      (unless (and (>= length 1) (= length (- size 8)))
        (nelisp-eln-metadata--fail name 'static-object-length
                                   (list length size)))
      (nelisp-eln-metadata--decode-and-read
       (nelisp-eln-metadata--read-bytes
        handle name 8 length read-root-object-bytes-function)
       name hash-dollar-marker))))

(defun nelisp-eln-metadata--check-storage
    (handle name vector symbol-info-function)
  (unless (vectorp vector)
    (nelisp-eln-metadata--fail name 'expected-vector vector))
  (let* ((info (nelisp-eln-metadata--object-info
                handle name symbol-info-function))
         (size (plist-get info :size))
         (required (* 8 (length vector))))
    (unless (>= size required)
      (nelisp-eln-metadata--fail name 'relocation-storage-too-small
                                 (list size required)))
    size))

(defun nelisp-eln-metadata-read (handle)
  "Return validated, inert metadata from GNU .eln HANDLE.
The exact pinned ABI hash is read and checked first, before any other blob
payload is requested. HANDLE uses `nl-ffi-loader' custom-loader semantics.
Returned payloads are ordinary Lisp data; no form is evaluated and no GNU
heap object is decoded."
  (nelisp-eln-metadata-read-with-backend
   handle #'nl-ffi-loader-symbol-info
   #'nl-ffi-loader-read-root-object-bytes))

(defun nelisp-eln-metadata-read-with-backend
    (handle symbol-info-function read-root-object-bytes-function)
  "Read inert GNU .eln metadata using explicit backend callbacks.
SYMBOL-INFO-FUNCTION takes HANDLE and NAME and returns metadata for a
root-defined STT_OBJECT. READ-ROOT-OBJECT-BYTES-FUNCTION takes HANDLE,
NAME, OFFSET, and LENGTH and returns those exact bytes. The caller owns
HANDLE lifetime. The default `nelisp-eln-metadata-read' entry point keeps
the existing custom-loader API."
  (unless (and (functionp symbol-info-function)
               (functionp read-root-object-bytes-function))
    (nelisp-eln-metadata--fail nil 'invalid-backend
                               (list symbol-info-function
                                     read-root-object-bytes-function)))
  (let* ((hash-dollar-marker
          (nelisp-eln-metadata--make-hash-dollar-marker))
         (hash (nelisp-eln-metadata--read-static-object
                handle "freloc_hash_blob" hash-dollar-marker
                symbol-info-function read-root-object-bytes-function))
         (expected (plist-get nelisp-eln-abi-gnu-31-1-x86_64
                              :producer-abi-hash)))
    (unless (and (stringp hash) (equal hash expected))
      (nelisp-eln-metadata--fail "freloc_hash_blob" 'abi-hash-mismatch
                                 (list hash expected)))
    (nelisp-eln-metadata--check-hash-dollar-reader hash-dollar-marker)
    (let* ((data (nelisp-eln-metadata--read-static-object
                  handle "text_data_reloc_blob" hash-dollar-marker
                  symbol-info-function read-root-object-bytes-function))
           (ephemeral (nelisp-eln-metadata--read-static-object
                       handle "text_data_reloc_eph_blob" hash-dollar-marker
                       symbol-info-function read-root-object-bytes-function))
           (qualities (nelisp-eln-metadata--read-static-object
                       handle "text_optim_qly_blob" hash-dollar-marker
                       symbol-info-function read-root-object-bytes-function))
           (docs (nelisp-eln-metadata--read-static-object
                  handle "text_data_fdoc_blob" hash-dollar-marker
                  symbol-info-function read-root-object-bytes-function))
           (data-size (nelisp-eln-metadata--check-storage
                       handle "d_reloc" data symbol-info-function))
           (ephemeral-size (nelisp-eln-metadata--check-storage
                            handle "d_reloc_eph" ephemeral
                            symbol-info-function)))
      (unless (listp qualities)
        (nelisp-eln-metadata--fail "text_optim_qly_blob"
                                   'expected-list qualities))
      (unless (or (hash-table-p docs) (vectorp docs))
        (nelisp-eln-metadata--fail "text_data_fdoc_blob"
                                   'expected-hash-table-or-vector docs))
      (list :abi-hash hash
            :profile nelisp-eln-abi-gnu-31-1-x86_64
            :hash-dollar-marker hash-dollar-marker
            :data-relocations data
            :ephemeral-data-relocations ephemeral
            :optimization-qualities qualities
            :function-docs docs
            :d-reloc-size data-size
            :d-reloc-eph-size ephemeral-size))))

(provide 'nelisp-eln-metadata)

;;; nelisp-eln-metadata.el ends here
