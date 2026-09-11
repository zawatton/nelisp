;;; nelisp-native-runtime-safety-test.el --- Native loader failure boundaries -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'nelisp-native-load)

(ert-deftest nelisp-native-runtime-safety/bulk-copy-keeps-high-bytes ()
  (let ((bytes (unibyte-string 0 127 128 255)) (written nil))
    (cl-letf (((symbol-function 'ptr-write-bytes)
               (lambda (address string)
                 (setq written (list address string)) (string-bytes string)))
              ((symbol-function 'ptr-write-u8)
               (lambda (&rest _) (error "Unexpected interpreted copy"))))
      (nelisp-native-load--poke-string 8192 17 bytes)
      (should (equal written (list 8209 bytes))))))

(ert-deftest nelisp-native-runtime-safety/digest-does-not-change-gc-policy ()
  (let ((freed nil) (copied nil))
    (cl-letf (((symbol-function 'nelisp--sha256-bytes)
               (lambda (address length)
                 (should (= address 8192)) (should (= length 4)) "digest"))
              ((symbol-function 'nelisp-native-load--mmap)
               (lambda (size executable)
                 (should (= size 4096)) (should-not executable) 8192))
              ((symbol-function 'ptr-write-bytes)
               (lambda (_ string) (setq copied string) (string-bytes string)))
              ((symbol-function 'nelisp--debug-switch)
               (lambda (&rest _) (error "Digest changed GC policy")))
              ((symbol-function 'syscall-direct)
               (lambda (&rest args) (setq freed args) 0)))
      (should (equal (nelisp-native-load--digest (unibyte-string 0 127 128 255))
                     "digest"))
      (should (equal copied (unibyte-string 0 127 128 255)))
      (should (equal freed '(11 8192 4096 0 0 0 0))))))

(ert-deftest nelisp-native-runtime-safety/incomplete-source-is-rejected ()
  (should-error
   (nelisp-native-load--raw-source-forms
    "unused.el" "(defun good (x) x)\n(defun incomplete (x)"))
  (should (equal (nelisp-native-load--raw-source-forms
                  "unused.el" "(defun good (x) x)\n; trailing comment")
                 '((defun good (x) x)))))

(ert-deftest nelisp-native-runtime-safety/contract-hash-ignores-print-settings ()
  (let ((expected (nelisp-native-load--raw-v2-contract-hash)))
    (let ((print-length 1) (print-level 1))
      (should (equal expected (nelisp-native-load--raw-v2-contract-hash))))))

(ert-deftest nelisp-native-runtime-safety/contract-mismatch-precedes-mmap ()
  (let ((binary (make-string 64 ?a)) (mapped nil))
    (cl-letf (((symbol-function 'nelisp-native-load-manifest)
               (lambda (_) (list :binary-sha256 binary)))
              ((symbol-function 'nelisp-native-load-raw-v2-check)
               (lambda (&rest _) nil))
              ((symbol-function 'nelisp-native-load--running-binary-sha256)
               (lambda () binary))
              ((symbol-function 'nelisp-runtime-reload-contract-matches-p)
               (lambda () nil))
              ((symbol-function 'nelisp-native-load--mmap)
               (lambda (&rest _) (setq mapped t) (error "Unexpected mmap"))))
      (let ((err (should-error
                  (nelisp-native-load-raw-v2-artifact "unused.nelr" nil binary))))
        (should (string-match-p "contract mismatch" (error-message-string err))))
      (should-not mapped))))

;;; nelisp-native-runtime-safety-test.el ends here
