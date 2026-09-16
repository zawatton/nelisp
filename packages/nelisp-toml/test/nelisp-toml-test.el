;;; nelisp-toml-test.el --- ERT tests for nelisp-toml -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Coverage for `src/nelisp-toml.el' against the actual shapes of this
;; repository's `nelisp.toml' / `nelisp.lock' (mirrored from
;; `tools/nelisp-project.py', `tools/nelisp_packages.py', and
;; `tools/nelisp_project_manifest.py'), plus one test per construct this
;; package deliberately refuses, and the line-number/error-shape
;; contract itself.

;;; Code:

(require 'ert)
(require 'nelisp-toml)

(defmacro nelisp-toml-test--should-unsupported (toml)
  "Assert that parsing TOML signals `nelisp-toml-unsupported-error'."
  `(should-error (nelisp-toml-parse-string ,toml)
                 :type 'nelisp-toml-unsupported-error))

(defmacro nelisp-toml-test--should-malformed (toml)
  "Assert that parsing TOML signals `nelisp-toml-parse-error'."
  `(should-error (nelisp-toml-parse-string ,toml)
                 :type 'nelisp-toml-parse-error))

;;; The manifest shape (`nelisp.toml') -------------------------------------

(ert-deftest nelisp-toml-parses-a-manifest-without-dependencies ()
  (let ((doc (nelisp-toml-parse-string
              "[package]\nname = \"hello\"\nversion = \"0.1.0\"\n\n[application]\nsource = \"src/main.nl\"\nentry = \"main\"\n")))
    (should (equal (nelisp-toml-get doc "package" "name") "hello"))
    (should (equal (nelisp-toml-get doc "package" "version") "0.1.0"))
    (should (equal (nelisp-toml-get doc "application" "source") "src/main.nl"))
    (should (equal (nelisp-toml-get doc "application" "entry") "main"))
    (should (null (nelisp-toml-get doc "dependencies")))))

(ert-deftest nelisp-toml-parses-a-manifest-with-dependencies-and-comments ()
  "Mirrors `test/nelisp-project-manifest-test.py's BASE plus a
[dependencies] table with a leading file comment, a table-header trailing
comment, a full-line comment, single- and double-quoted values, and a
per-entry trailing comment -- all present in that fixture verbatim."
  (let* ((text (concat
                "# project\n"
                "[package]\nname = \"hello\"\nversion = \"0.1.0\"\n\n"
                "[application]\nsource = \"src/main.nl\"\nentry = \"main\"\n\n"
                "[dependencies] # libraries\n"
                "# keep this note\n"
                "\"json\" = '1' # reason\n"
                "http = \"2\"\n"))
         (doc (nelisp-toml-parse-string text)))
    (should (equal (nelisp-toml-get doc "package" "name") "hello"))
    (should (equal (nelisp-toml-get doc "dependencies" "json") "1"))
    (should (equal (nelisp-toml-get doc "dependencies" "http") "2"))))

(ert-deftest nelisp-toml-parses-crlf-manifest ()
  "`tools/nelisp_project_manifest.py' round-trips CRLF-saved manifests
without normalizing them; the reader must accept that shape too."
  (let* ((lf "[package]\nname = \"hello\"\nversion = \"0.1.0\"\n\n[application]\nsource = \"src/main.nl\"\nentry = \"main\"\n")
         (crlf (replace-regexp-in-string "\n" "\r\n" lf))
         (doc (nelisp-toml-parse-string crlf)))
    (should (equal (nelisp-toml-get doc "package" "name") "hello"))
    (should (equal (nelisp-toml-get doc "application" "entry") "main"))))

;;; The lock shape (`nelisp.lock') -------------------------------------------

(ert-deftest nelisp-toml-parses-a-lock-with-packages ()
  "Mirrors the exact layout `tools/nelisp_packages.py's `lock_bytes'
emits: top-level `schema_version', a `[requirements]' table, and one or
more `[[packages]]' entries each with a nested `[packages.dependencies]'
table."
  (let* ((text (concat
                "schema_version = 1\n\n"
                "[requirements]\n"
                "\"greet\" = \"^1.0.0\"\n\n"
                "[[packages]]\n"
                "name = \"greet\"\n"
                "version = \"1.0.0\"\n"
                "sha256 = \"" (make-string 64 ?a) "\"\n"
                "url = \"https://example.com/greet-1.0.0.tar\"\n"
                "[packages.dependencies]\n"))
         (doc (nelisp-toml-parse-string text))
         (packages (nelisp-toml-get doc "packages")))
    (should (= (nelisp-toml-get doc "schema_version") 1))
    (should (equal (nelisp-toml-get doc "requirements" "greet") "^1.0.0"))
    (should (= (length packages) 1))
    (should (equal (gethash "name" (car packages)) "greet"))
    (should (equal (gethash "version" (car packages)) "1.0.0"))
    (should (hash-table-p (gethash "dependencies" (car packages))))
    (should (= (hash-table-count (gethash "dependencies" (car packages))) 0))))

(ert-deftest nelisp-toml-parses-a-lock-with-two-packages-and-transitive-deps ()
  (let* ((text (concat
                "schema_version = 1\n\n"
                "[requirements]\n"
                "\"greet\" = \"^1.0.0\"\n\n"
                "[[packages]]\n"
                "name = \"core\"\n"
                "version = \"1.0.0\"\n"
                "sha256 = \"" (make-string 64 ?a) "\"\n"
                "url = \"https://example.com/core-1.0.0.tar\"\n"
                "[packages.dependencies]\n\n"
                "[[packages]]\n"
                "name = \"greet\"\n"
                "version = \"1.0.0\"\n"
                "sha256 = \"" (make-string 64 ?b) "\"\n"
                "url = \"https://example.com/greet-1.0.0.tar\"\n"
                "[packages.dependencies]\n"
                "\"core\" = \"^1.0.0\"\n"))
         (doc (nelisp-toml-parse-string text))
         (packages (nelisp-toml-get doc "packages")))
    (should (= (length packages) 2))
    (should (equal (gethash "name" (nth 0 packages)) "core"))
    (should (equal (gethash "name" (nth 1 packages)) "greet"))
    (should (equal (gethash "core" (gethash "dependencies" (nth 1 packages)))
                   "^1.0.0"))))

(ert-deftest nelisp-toml-parses-empty-packages-array ()
  "`lock_bytes' writes a top-level `packages = []' when there are none."
  (let ((doc (nelisp-toml-parse-string
              "schema_version = 1\n\npackages = []\n\n[requirements]\n")))
    (should (= (nelisp-toml-get doc "schema_version") 1))
    (should (null (nelisp-toml-get doc "packages")))
    (should (hash-table-p (nelisp-toml-get doc "requirements")))))

;;; Duplicate / conflicting keys -------------------------------------------

(ert-deftest nelisp-toml-rejects-duplicate-key-in-same-table ()
  (nelisp-toml-test--should-malformed "[package]\nname = \"a\"\nname = \"b\"\n"))

(ert-deftest nelisp-toml-rejects-redefining-a-table-header ()
  (nelisp-toml-test--should-malformed "[package]\nname = \"a\"\n[package]\nname = \"b\"\n"))

(ert-deftest nelisp-toml-rejects-header-through-a-non-table-key ()
  (nelisp-toml-test--should-malformed "package = \"a\"\n[package]\nname = \"b\"\n"))

;;; Unsupported constructs (named `nelisp-toml-unsupported-error') ----------

(ert-deftest nelisp-toml-refuses-floats ()
  (nelisp-toml-test--should-unsupported "[a]\nx = 1.5\n"))

(ert-deftest nelisp-toml-refuses-exponent-floats ()
  (nelisp-toml-test--should-unsupported "[a]\nx = 1e10\n"))

(ert-deftest nelisp-toml-refuses-booleans ()
  (nelisp-toml-test--should-unsupported "[a]\nx = true\n"))

(ert-deftest nelisp-toml-refuses-dates ()
  (nelisp-toml-test--should-unsupported "[a]\nx = 1979-05-27\n"))

(ert-deftest nelisp-toml-refuses-times ()
  (nelisp-toml-test--should-unsupported "[a]\nx = 07:32:00\n"))

(ert-deftest nelisp-toml-refuses-hex-integers ()
  (nelisp-toml-test--should-unsupported "[a]\nx = 0xFF\n"))

(ert-deftest nelisp-toml-refuses-octal-integers ()
  (nelisp-toml-test--should-unsupported "[a]\nx = 0o17\n"))

(ert-deftest nelisp-toml-refuses-binary-integers ()
  (nelisp-toml-test--should-unsupported "[a]\nx = 0b101\n"))

(ert-deftest nelisp-toml-refuses-underscore-digit-separators ()
  (nelisp-toml-test--should-unsupported "[a]\nx = 1_000\n"))

(ert-deftest nelisp-toml-refuses-inline-tables ()
  (nelisp-toml-test--should-unsupported "dependencies = {json = \"1\"}\n[package]\nname = \"a\"\n"))

(ert-deftest nelisp-toml-refuses-multiline-basic-strings ()
  (nelisp-toml-test--should-unsupported "[a]\nx = \"\"\"hello\nworld\"\"\"\n"))

(ert-deftest nelisp-toml-refuses-multiline-literal-strings ()
  (nelisp-toml-test--should-unsupported "[a]\nx = '''hello\nworld'''\n"))

(ert-deftest nelisp-toml-refuses-dotted-keys-outside-headers ()
  (nelisp-toml-test--should-unsupported "[a]\nb.c = 1\n"))

;;; Malformed input (named `nelisp-toml-parse-error') ------------------------

(ert-deftest nelisp-toml-rejects-leading-zero-integers ()
  (nelisp-toml-test--should-malformed "[a]\nx = 01\n"))

(ert-deftest nelisp-toml-rejects-missing-equals ()
  (nelisp-toml-test--should-malformed "[a]\nx \"1\"\n"))

(ert-deftest nelisp-toml-rejects-unterminated-string ()
  (nelisp-toml-test--should-malformed "[a]\nx = \"unterminated\n"))

(ert-deftest nelisp-toml-rejects-newline-inside-basic-string ()
  (nelisp-toml-test--should-malformed "[a]\nx = \"line1\nline2\"\n"))

(ert-deftest nelisp-toml-rejects-trailing-garbage-after-value ()
  (nelisp-toml-test--should-malformed "[a]\nx = \"ok\" garbage\n"))

(ert-deftest nelisp-toml-rejects-empty-input-key ()
  (nelisp-toml-test--should-malformed "= \"a\"\n"))

(ert-deftest nelisp-toml-rejects-invalid-escape ()
  "The TOML text is `x = \"\\q\"' -- one backslash then `q', which is not
a recognised escape (contrast `nelisp-toml-parses-basic-string-escapes',
whose `\\\\' is a doubled, and therefore valid, backslash escape)."
  (nelisp-toml-test--should-malformed "[a]\nx = \"\\q\"\n"))

;;; Error shape: line number and reason ------------------------------------

(ert-deftest nelisp-toml-error-names-the-offending-line ()
  (condition-case err
      (progn (nelisp-toml-parse-string "[a]\nx = 1\ny = 1.5\n") (should nil))
    (nelisp-toml-unsupported-error
     (should (= (nth 1 (cdr err)) 3))
     (should (string-match-p "\\`line 3:" (nth 0 (cdr err)))))))

(ert-deftest nelisp-toml-error-on-first-line-reports-line-one ()
  (condition-case err
      (progn (nelisp-toml-parse-string "x = true\n") (should nil))
    (nelisp-toml-unsupported-error
     (should (= (nth 1 (cdr err)) 1)))))

(ert-deftest nelisp-toml-both-error-types-derive-from-nelisp-toml-error ()
  (should (memq 'nelisp-toml-error (get 'nelisp-toml-parse-error 'error-conditions)))
  (should (memq 'nelisp-toml-error (get 'nelisp-toml-unsupported-error 'error-conditions))))

;;; String escapes -------------------------------------------------------

(ert-deftest nelisp-toml-parses-basic-string-escapes ()
  (let ((doc (nelisp-toml-parse-string "[a]\nx = \"a\\\"b\\\\c\\nd\"\n")))
    (should (equal (nelisp-toml-get doc "a" "x") "a\"b\\c\nd"))))

(ert-deftest nelisp-toml-parses-unicode-escape ()
  (let ((doc (nelisp-toml-parse-string "[a]\nx = \"\\u65E5\\u672C\\u8A9E\"\n")))
    (should (equal (nelisp-toml-get doc "a" "x") "日本語"))))

(ert-deftest nelisp-toml-literal-string-has-no-escapes ()
  (let ((doc (nelisp-toml-parse-string "[a]\nx = 'a\\nb'\n")))
    (should (equal (nelisp-toml-get doc "a" "x") "a\\nb"))))

;;; Arrays -----------------------------------------------------------------

(ert-deftest nelisp-toml-parses-string-array ()
  (let ((doc (nelisp-toml-parse-string "[a]\nx = [\"one\", \"two\", \"three\"]\n")))
    (should (equal (nelisp-toml-get doc "a" "x") '("one" "two" "three")))))

(ert-deftest nelisp-toml-parses-integer-array-across-lines-with-comment ()
  (let ((doc (nelisp-toml-parse-string "[a]\nx = [\n  1, # one\n  2,\n  3\n]\n")))
    (should (equal (nelisp-toml-get doc "a" "x") '(1 2 3)))))

(ert-deftest nelisp-toml-parses-empty-array ()
  (let ((doc (nelisp-toml-parse-string "[a]\nx = []\n")))
    (should (null (nelisp-toml-get doc "a" "x")))))

;;; File reading -------------------------------------------------------------

(ert-deftest nelisp-toml-parse-file-reads-from-disk ()
  (let ((file (make-temp-file "nelisp-toml-test")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "[package]\nname = \"hello\"\nversion = \"0.1.0\"\n\n[application]\nsource = \"src/main.nl\"\nentry = \"main\"\n"))
          (should (equal (nelisp-toml-get (nelisp-toml-parse-file file) "package" "name")
                         "hello")))
      (delete-file file))))

;;; nelisp-toml-get ---------------------------------------------------------

(ert-deftest nelisp-toml-get-returns-nil-for-missing-path ()
  (let ((doc (nelisp-toml-parse-string "[a]\nx = 1\n")))
    (should (null (nelisp-toml-get doc "a" "missing")))
    (should (null (nelisp-toml-get doc "missing" "x")))
    (should (null (nelisp-toml-get doc "a" "x" "too-deep")))))

(provide 'nelisp-toml-test)

;;; nelisp-toml-test.el ends here
