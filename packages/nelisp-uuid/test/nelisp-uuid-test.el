;;; nelisp-uuid-test.el --- ERT tests for nelisp-uuid -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Coverage for `src/nelisp-uuid.el': v4 shape/version/variant bits, v5
;; determinism against reference vectors independently produced by
;; Python's standard-library `uuid' module (`uuid.uuid5'), canonical
;; text parsing, and malformed-input rejection.

;;; Code:

(require 'ert)
(require 'nelisp-uuid)

;;; v4 -----------------------------------------------------------------

(ert-deftest nelisp-uuid-v4-is-canonical-shape ()
  (should (nelisp-uuid-p (nelisp-uuid-v4))))

(ert-deftest nelisp-uuid-v4-has-version-nibble-4 ()
  (dotimes (_ 20)
    (let ((id (nelisp-uuid-v4)))
      (should (eq (aref id 14) ?4)))))

(ert-deftest nelisp-uuid-v4-has-rfc4122-variant ()
  (dotimes (_ 20)
    (let ((id (nelisp-uuid-v4)))
      (should (memq (aref id 19) '(?8 ?9 ?a ?b))))))

(ert-deftest nelisp-uuid-v4-calls-differ ()
  "Not a proof of randomness quality (see Commentary); a collision here
would mean the generator is broken, not merely imperfect."
  (should-not (equal (nelisp-uuid-v4) (nelisp-uuid-v4))))

;;; v5 -----------------------------------------------------------------

(ert-deftest nelisp-uuid-v5-matches-python-reference-dns ()
  "Reference value from Python's `uuid.uuid5(uuid.NAMESPACE_DNS, \"python.org\")'."
  (should (equal (nelisp-uuid-v5 nelisp-uuid-namespace-dns "python.org")
                 "886313e1-3b8a-5372-9b90-0c9aee199e5d")))

(ert-deftest nelisp-uuid-v5-matches-python-reference-url ()
  "Reference value from Python's
`uuid.uuid5(uuid.NAMESPACE_URL, \"https://example.com/\")'."
  (should (equal (nelisp-uuid-v5 nelisp-uuid-namespace-url "https://example.com/")
                 "dd2c1780-811a-5296-81c5-178a0ef488bc")))

(ert-deftest nelisp-uuid-v5-matches-python-reference-empty-name ()
  "Reference value from Python's `uuid.uuid5(uuid.NAMESPACE_DNS, \"\")'."
  (should (equal (nelisp-uuid-v5 nelisp-uuid-namespace-dns "")
                 "4ebd0208-8328-5d69-8c44-ec50939c0967")))

(ert-deftest nelisp-uuid-v5-is-deterministic ()
  (should (equal (nelisp-uuid-v5 nelisp-uuid-namespace-url "a")
                 (nelisp-uuid-v5 nelisp-uuid-namespace-url "a"))))

(ert-deftest nelisp-uuid-v5-differs-by-name ()
  (should-not (equal (nelisp-uuid-v5 nelisp-uuid-namespace-url "a")
                     (nelisp-uuid-v5 nelisp-uuid-namespace-url "b"))))

(ert-deftest nelisp-uuid-v5-differs-by-namespace ()
  (should-not (equal (nelisp-uuid-v5 nelisp-uuid-namespace-dns "a")
                     (nelisp-uuid-v5 nelisp-uuid-namespace-url "a"))))

(ert-deftest nelisp-uuid-v5-has-version-nibble-5 ()
  (should (eq (aref (nelisp-uuid-v5 nelisp-uuid-namespace-dns "a") 14) ?5)))

(ert-deftest nelisp-uuid-v5-has-rfc4122-variant ()
  (should (memq (aref (nelisp-uuid-v5 nelisp-uuid-namespace-dns "a") 19)
                '(?8 ?9 ?a ?b))))

(ert-deftest nelisp-uuid-v5-handles-multibyte-name ()
  "Name-based hashing must go through UTF-8 bytes, not raw codepoints."
  (should (nelisp-uuid-p (nelisp-uuid-v5 nelisp-uuid-namespace-dns "日本語"))))

(ert-deftest nelisp-uuid-v5-rejects-malformed-namespace ()
  (should-error (nelisp-uuid-v5 "not-a-uuid" "a")
                :type 'nelisp-uuid-parse-error))

;;; Parse / predicate ----------------------------------------------------

(ert-deftest nelisp-uuid-p-rejects-non-strings ()
  (should-not (nelisp-uuid-p nil))
  (should-not (nelisp-uuid-p 42))
  (should-not (nelisp-uuid-p "886313e1-3b8a-5372-9b90-0c9aee199e5")))

(ert-deftest nelisp-uuid-p-accepts-uppercase ()
  (should (nelisp-uuid-p "886313E1-3B8A-5372-9B90-0C9AEE199E5D")))

(ert-deftest nelisp-uuid-parse-lowercases ()
  (should (equal (nelisp-uuid-parse "886313E1-3B8A-5372-9B90-0C9AEE199E5D")
                 "886313e1-3b8a-5372-9b90-0c9aee199e5d")))

(ert-deftest nelisp-uuid-parse-signals-on-malformed ()
  (should-error (nelisp-uuid-parse "not-a-uuid")
                :type 'nelisp-uuid-parse-error)
  (should-error (nelisp-uuid-parse "886313e1-3b8a-5372-9b90-0c9aee199e5")
                :type 'nelisp-uuid-parse-error)
  (should-error (nelisp-uuid-parse "886313e1-3b8a-5372-9b90-0c9aee199e5dz")
                :type 'nelisp-uuid-parse-error)
  (should-error (nelisp-uuid-parse nil)
                :type 'nelisp-uuid-parse-error))

(ert-deftest nelisp-uuid-round-trips-through-v4 ()
  (let ((id (nelisp-uuid-v4)))
    (should (equal (nelisp-uuid-parse id) id))))

(ert-deftest nelisp-uuid-nil-p-recognises-the-nil-uuid ()
  (should (nelisp-uuid-nil-p "00000000-0000-0000-0000-000000000000"))
  (should-not (nelisp-uuid-nil-p (nelisp-uuid-v4))))

(provide 'nelisp-uuid-test)

;;; nelisp-uuid-test.el ends here
