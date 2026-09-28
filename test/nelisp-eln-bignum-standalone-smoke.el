;;; nelisp-eln-bignum-standalone-smoke.el --- bignum view smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(let* ((directory (file-name-directory load-file-name))
       (root (or (getenv "NELISP_ROOT")
                 (expand-file-name ".." directory)))
       (module (or (getenv "NELISP_BIGNUM_VIEW")
                   (expand-file-name "../lisp/nelisp-eln-bignum.el"
                                     directory))))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (load module nil t))

(dolist (value (list (1- (ash 1 130)) (- (ash 1 130))))
  (let* ((owner (nelisp-eln-bignum-allocate value))
         (address (nelisp-eln-bignum-address owner))
         (signed-count (if (< value 0) -3 3)))
    (unwind-protect
        (progn
          (unless (= (nelisp-eln-abi-read-word address 0)
                     (nelisp-eln-bignum--header))
            (error "header mismatch"))
          (unless (= (ptr-read-u32 address 8) 3)
            (error "allocation count mismatch"))
          (unless (= (ptr-read-u32 address 12)
                     (logand signed-count #xffffffff))
            (error "signed size mismatch"))
          (unless (= (nelisp-eln-abi-read-word address 16) (+ address 24))
            (error "limb pointer mismatch"))
          (unless (= (nelisp-eln-abi-read-word address 24)
                     (if (< value 0) 0 18446744073709551615))
            (error "first limb mismatch"))
          (unless (eq (nelisp-eln-bignum-source owner) value)
            (error "source identity mismatch"))
          (garbage-collect)
          (unless (eq (nelisp-eln-bignum-decode
                       owner (nelisp-eln-bignum-word owner)) value)
            (error "decode identity mismatch"))
          (princ "bignum-view standalone-byte-gc-smoke PASS\n"))
      (nelisp-eln-bignum-release owner))))
