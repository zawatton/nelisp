;;; nelisp-base64-test.el --- ERT tests for nelisp-base64  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-base64)

(defun nelisp-b64-test--all-bytes ()
  "Return a unibyte string containing bytes 0..255."
  (apply #'unibyte-string (number-sequence 0 255)))

(ert-deftest nelisp-b64-encode-rfc4648-vectors ()
  (should (equal (nelisp-b64-encode-string "" t) ""))
  (should (equal (nelisp-b64-encode-string "f" t) "Zg=="))
  (should (equal (nelisp-b64-encode-string "fo" t) "Zm8="))
  (should (equal (nelisp-b64-encode-string "foo" t) "Zm9v"))
  (should (equal (nelisp-b64-encode-string "foob" t) "Zm9vYg=="))
  (should (equal (nelisp-b64-encode-string "fooba" t) "Zm9vYmE="))
  (should (equal (nelisp-b64-encode-string "foobar" t) "Zm9vYmFy")))

(ert-deftest nelisp-b64-decode-rfc4648-vectors ()
  (should (equal (nelisp-b64-decode-string "") ""))
  (should (equal (nelisp-b64-decode-string "Zg==") "f"))
  (should (equal (nelisp-b64-decode-string "Zm8=") "fo"))
  (should (equal (nelisp-b64-decode-string "Zm9v") "foo"))
  (should (equal (nelisp-b64-decode-string "Zm9vYmFy") "foobar")))

(ert-deftest nelisp-b64-roundtrip-binary-all-bytes ()
  (let ((bytes (nelisp-b64-test--all-bytes)))
    (should (equal (nelisp-b64-decode-to-bytes
                    (nelisp-b64-encode-string bytes t))
                   bytes))))

(ert-deftest nelisp-b64-encode-bytes-list-input ()
  (should (equal (nelisp-b64-encode-bytes '(0 1 2 253 254 255))
                 "AAEC/f7/")))

(ert-deftest nelisp-b64-decode-to-bytes-unibyte-output ()
  (let ((decoded (nelisp-b64-decode-to-bytes "AAEC/f7/")))
    (should (stringp decoded))
    (should-not (multibyte-string-p decoded))
    (should (equal (append decoded nil) '(0 1 2 253 254 255)))))

(ert-deftest nelisp-b64-mime-line-break-at-76-columns ()
  (let* ((input (make-string 58 ?A))
         (encoded (nelisp-b64-encode-string input nil)))
    (should (equal encoded
                   "QUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFB\nQQ=="))))

(ert-deftest nelisp-b64-no-line-break-option ()
  (let ((input (make-string 58 ?A)))
    (should-not (string-match-p "\n" (nelisp-b64-encode-string input t)))))

(ert-deftest nelisp-b64-decode-ignores-ascii-whitespace ()
  (should (equal (nelisp-b64-decode-string " Zm9v\r\nYmFy\t ")
                 "foobar")))

(ert-deftest nelisp-b64-decode-rejects-invalid-char ()
  (should-error (nelisp-b64-decode-string "Zm9v*")
                :type 'error))

(ert-deftest nelisp-b64-decode-rejects-invalid-length ()
  (should-error (nelisp-b64-decode-string "abc")
                :type 'error))

(ert-deftest nelisp-b64-decode-rejects-padding-in-middle ()
  (should-error (nelisp-b64-decode-string "Zm=9")
                :type 'error)
  (should-error (nelisp-b64-decode-string "Zg==Zm8=")
                :type 'error))

(ert-deftest nelisp-b64-encode-string-rejects-multibyte ()
  (should-error (nelisp-b64-encode-string "あ" t)
                :type 'error))

(ert-deftest nelisp-b64-empty-roundtrip ()
  (should (equal (nelisp-b64-decode-string (nelisp-b64-encode-string "" t))
                 "")))

(provide 'nelisp-base64-test)

;;; nelisp-base64-test.el ends here
