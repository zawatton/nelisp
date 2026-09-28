;;; nelisp-eln-symbol-info-smoke.el --- bounded GNU ELN metadata read -*- lexical-binding: t; -*-

(require 'nl-ffi)
(require 'nl-ffi-loader)

(let* ((path (getenv "ELN_FILE"))
       (handle (and path (nl-ffi-loader-open path)))
       (byte-path (getenv "BYTE_ELN_FILE"))
       (byte-handle (and byte-path (nl-ffi-loader-open byte-path)))
       (missing-path (concat byte-path ".must-not-exist"))
       (open-error (condition-case err
                       (progn (nl-ffi-loader-open missing-path) nil)
                     (nl-ffi-loader-open-failed err)))
       (info (and handle (nl-ffi-loader-symbol-info handle "freloc_hash_blob")))
       (header (and handle
                    (nl-ffi-loader-read-root-object-bytes
                     handle "freloc_hash_blob" 0 8)))
       (payload (and handle
                     (nl-ffi-loader-read-root-object-bytes
                      handle "freloc_hash_blob" 8 11)))
       (nul (and handle
                 (nl-ffi-loader-read-root-object-bytes
                  handle "freloc_hash_blob" 18 1)))
       (high-byte (and handle
                       (nl-ffi-loader-read-root-object-bytes
                        handle "text_data_reloc_eph_blob" 0 1)))
       (byte-info (and byte-handle
                       (nl-ffi-loader-symbol-info
                        byte-handle "text_data_fdoc_blob")))
       (ff-byte (and byte-handle
                     (nl-ffi-loader-read-root-object-bytes
                      byte-handle "text_data_fdoc_blob" 0 1)))
       (length (and header
                    (let ((n 0))
                      (dotimes (i 8 n)
                        (setq n (logior n (ash (aref header i) (* 8 i)))))))))
  (unless (and info
               (= (plist-get info :type) 1)
               (= (plist-get info :size) 19)
               (> (plist-get info :section-index) 0)
               (< (plist-get info :section-index) #xff00)
               (equal (plist-get info :source-path) path)
               (= length 11)
               (equal payload (string-as-unibyte (concat "\"ba35c031\"" (string 0))))
               (equal nul (unibyte-string 0))
               (equal high-byte (unibyte-string 128))
               (= (plist-get byte-info :size) 263)
               (equal (plist-get byte-info :source-path) byte-path)
               (equal ff-byte (unibyte-string 255))
               (nl-ffi-loader--path-openable-p path)
               (not (nl-ffi-loader--path-openable-p missing-path))
               (eq (car open-error) 'nl-ffi-loader-open-failed)
               (equal (nth 1 open-error) missing-path)
               (= (nth 2 open-error) -2))
    (error "ELN root-object metadata read failed: %S %S %S %S %S %S"
           info length payload nul high-byte byte-info))
  (let ((octets (concat nul high-byte ff-byte)))
    (unless (and (= (length octets) 3)
                 (equal octets (unibyte-string 0 128 255)))
      (error "ELN byte-copy output mismatch: %S" octets))
    (princ "ELN_ROOT_OBJECT_READ size=19 payload=ba35c031\n")
    (princ "ELN_RAW_BYTES count=3 bytes=00,80,ff\n")))

;;; nelisp-eln-symbol-info-smoke.el ends here
