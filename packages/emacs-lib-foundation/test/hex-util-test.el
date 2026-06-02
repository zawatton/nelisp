;;; hex-util-test.el --- tests for lightweight hex-util facade  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;;; Code:

(require 'ert)
(require 'hex-util)

(defun hex-util-test--octet-string (&rest octets)
  "Return an octet string containing OCTETS."
  (let ((string (make-string (length octets) 0))
        (idx 0))
    (dolist (octet octets)
      (aset string idx octet)
      (setq idx (1+ idx)))
    string))

(ert-deftest hex-util-test/feature-loads ()
  (should (featurep 'hex-util))
  (should (fboundp 'encode-hex-string))
  (should (fboundp 'decode-hex-string)))

(ert-deftest hex-util-test/encodes-octet-string ()
  (should (equal (encode-hex-string "") ""))
  (should (equal (encode-hex-string "ABC") "414243"))
  (should (equal (encode-hex-string
                  (hex-util-test--octet-string 0 1 15 16 127 128 255))
                 "00010f107f80ff")))

(ert-deftest hex-util-test/decodes-hex-string ()
  (should (equal (decode-hex-string "") ""))
  (should (equal (decode-hex-string "414243") "ABC"))
  (should (equal (decode-hex-string "00010f107f80ff")
                 (hex-util-test--octet-string 0 1 15 16 127 128 255))))

(ert-deftest hex-util-test/decodes-uppercase-and-mixed-case ()
  (should (equal (decode-hex-string "DEADBEEF")
                 (hex-util-test--octet-string #xde #xad #xbe #xef)))
  (should (equal (decode-hex-string "Cafe")
                 (hex-util-test--octet-string #xca #xfe))))

(ert-deftest hex-util-test/rejects-invalid-digit ()
  (should-error (decode-hex-string "0g")))

(ert-deftest hex-util-test/rejects-odd-length-input ()
  (should-error (decode-hex-string "abc")))

(provide 'hex-util-test)

;;; hex-util-test.el ends here
