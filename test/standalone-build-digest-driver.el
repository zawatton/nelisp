;;; standalone-build-digest-driver.el --- Reader identity and 1 MiB SHA vectors -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-load)
(let ((expected (getenv "NELISP_EXPECTED_BUILD_DIGEST")))
  (unless (equal (nelisp--build-digest) expected) (error "Build stamp mismatch"))
  (let ((digest (nelisp--build-digest)))
    (aset digest 0 ?z)
    (unless (equal (nelisp--build-digest) expected) (error "Mutable build stamp"))))
;; Linux retains the whole-file identity rather than silently switching its
;; cache protocol to the zero-field stamp.
(unless (equal (nelisp-native-load-running-binary-sha256)
               (getenv "NELISP_EXPECTED_FILE_DIGEST"))
  (error "Linux whole-file identity changed"))
(let ((bytes (make-string 1048576 97))
      (expected "9bc1b2a288b26af7257a36277ae3816a7d4f16e89c1e7e77d0a5c48bad62b360"))
  (unless (equal (nelisp--sha256 bytes) expected) (error "1 MiB string SHA mismatch"))
  (unless (equal (nelisp-native-load--digest bytes) expected) (error "1 MiB byte SHA mismatch")))
;; A decoded artifact's high bytes must be hashed as bytes, not UTF-8.
(require 'nelisp-native-raw-file)
(let ((bytes (nelisp-native-raw-file-read (getenv "NELISP_HIGH_BYTES") 0 1048576)))
  (unless (equal (nelisp-native-load--digest bytes)
                 "f5fb04aa5b882706b9309e885f19477261336ef76a150c3b4d3489dfac3953ec")
    (error "1 MiB binary SHA mismatch")))
;; Simulate the Windows OS branch on Linux; even an unavailable subprocess
;; helper must not affect a file-buffer SHA. Restore both owners afterwards.
(let ((os (symbol-function 'nelisp--target-os-code))
      (helper (symbol-function 'nelisp--secure-hash-helper)))
  (unwind-protect
      (progn
        (fset 'nelisp--target-os-code (lambda () 2))
        (fset 'nelisp--secure-hash-helper
              (lambda (&rest _) (error "Windows SHA helper must not run")))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert (make-string 1048576 97))
          (unless (equal (secure-hash 'sha256 (current-buffer))
                         "9bc1b2a288b26af7257a36277ae3816a7d4f16e89c1e7e77d0a5c48bad62b360")
            (error "Windows buffer SHA mismatch"))))
    (fset 'nelisp--target-os-code os)
    (fset 'nelisp--secure-hash-helper helper)))
(princ "BUILD-DIGEST-SHA-PASS\n")
