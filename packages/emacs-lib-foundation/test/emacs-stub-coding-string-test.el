;;; emacs-stub-coding-string-test.el --- ERT for the UTF-8 coding-string stubs  -*- lexical-binding: t; -*-

;;; Commentary:

;; On the standalone runtime `emacs-stub.el' installs `decode-coding-string'
;; and `encode-coding-string' (its "Phase B5" block), replacing the reader's
;; own definitions.  Those stubs used to be the identity, so a decoded
;; string stayed unibyte.  SQLite text read through anvil-standalone then
;; reached MCP clients as double-encoded UTF-8 ("を" arrived as "ã\x82\x92"),
;; and Japanese written through `worklog-add' was stored as invalid UTF-8.
;;
;; No byte conversion is needed on the standalone runtime, but the result
;; kind is observable: decoding yields a multibyte string whose `length'
;; counts characters, and encoding yields a unibyte string whose `aref'
;; yields bytes.  These assertions hold identically under host Emacs (the
;; C primitives) and under standalone NeLisp (the stubs).

;;; Code:

(require 'ert)

(defconst emacs-stub-coding-string-test--wo-bytes (unibyte-string 227 130 146)
  "The UTF-8 bytes of HIRAGANA LETTER WO (U+3092).")

(ert-deftest emacs-stub-coding-string-test/decode-utf-8-is-multibyte ()
  (let ((s (decode-coding-string emacs-stub-coding-string-test--wo-bytes 'utf-8)))
    (should (multibyte-string-p s))
    (should (= (length s) 1))
    (should (= (aref s 0) #x3092))))

(ert-deftest emacs-stub-coding-string-test/encode-utf-8-is-unibyte ()
  (let ((s (encode-coding-string (string #x3092) 'utf-8)))
    (should-not (multibyte-string-p s))
    (should (= (length s) 3))
    (should (equal (append s nil) '(227 130 146)))))

(ert-deftest emacs-stub-coding-string-test/round-trip ()
  (let ((text (concat "PROBE " (string #x65e5 #x672c #x8a9e))))
    (should (equal (decode-coding-string (encode-coding-string text 'utf-8)
                                         'utf-8)
                   text))))

(ert-deftest emacs-stub-coding-string-test/ascii-unchanged ()
  (should (equal (decode-coding-string "{\"a\":1}" 'utf-8) "{\"a\":1}"))
  (should (equal (encode-coding-string "{\"a\":1}" 'utf-8) "{\"a\":1}")))

(provide 'emacs-stub-coding-string-test)

;;; emacs-stub-coding-string-test.el ends here
