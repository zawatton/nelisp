;;; nelisp-eln-metadata-bytecode-test.el --- tests for the #[...] fallback reader -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; These tests run under host GNU Emacs (see the Makefile's ERT target),
;; calling `nelisp-eln-metadata-bytecode-read' directly rather than
;; through `nelisp-eln-metadata-read''s ambient-reader dispatch, since
;; host's own `read-from-string' never raises the standalone-only
;; `(invalid-read-syntax "#[")' that triggers that dispatch (see
;; nelisp-eln-metadata-test.el for the dispatch-level coverage, which
;; simulates the standalone's failure via a temporary `read-from-string'
;; override).  Fixture text below is drawn from genuine GNU 31.1 .eln
;; `text_data_reloc_blob' payloads captured from real installed packages
;; (dictionary-server-plain.eln, and the shared-instruction-string shape
;; surveyed across anzu.eln / anvil-shell-filter.eln / bookmark.eln /
;; cl-macs.eln and hundreds of others -- see the eln-hashbracket S6
;; worklog), reduced to the minimum needed to exercise each shape.

;;; Code:

(require 'ert)
(require 'nelisp-eln-metadata-bytecode)

(defun nelisp-eln-metadata-bytecode-test--read (text)
  (car (nelisp-eln-metadata-bytecode-read text)))

(ert-deftest nelisp-eln-metadata-bytecode/plain-values-and-dotted-pair ()
  (should (eq (nelisp-eln-metadata-bytecode-test--read "nil") nil))
  (should (eq (nelisp-eln-metadata-bytecode-test--read "t") t))
  (should (= (nelisp-eln-metadata-bytecode-test--read "-42") -42))
  (should (equal (nelisp-eln-metadata-bytecode-test--read "\"a\\tb\"") "a\tb"))
  (should (equal (nelisp-eln-metadata-bytecode-test--read "(a . b)") '(a . b)))
  (should (equal (nelisp-eln-metadata-bytecode-test--read "[1 2 3]") [1 2 3])))

(ert-deftest nelisp-eln-metadata-bytecode/quote-and-function-sugar ()
  (should (equal (nelisp-eln-metadata-bytecode-test--read "'cconv") '(quote cconv)))
  (should (equal (nelisp-eln-metadata-bytecode-test--read "#'cconv-convert")
                 '(function cconv-convert))))

(ert-deftest nelisp-eln-metadata-bytecode/backslash-escaped-symbol-name ()
  ;; GNU prints the symbol whose name is "c-per-(-match" as
  ;; `c-per-\(-match' so the `(' does not read back as list syntax; a
  ;; naive atom scanner that stops at any unescaped-or-not `(' would
  ;; truncate the token and desynchronize the surrounding vector's
  ;; bracket count (see cc-engine.eln in the eln-hashbracket survey).
  (should (equal (symbol-name (nelisp-eln-metadata-bytecode-test--read
                                "c-per-\\(-match"))
                 "c-per-(-match")))

(ert-deftest nelisp-eln-metadata-bytecode/genuine-artifact-byte-code-literal ()
  ;; Captured verbatim from dictionary-server-plain.eln's
  ;; `text_data_reloc_blob' (31.1-ba35c031): a `make-closure' template
  ;; with an embedded raw tab and a raw NUL byte inside the instruction
  ;; string, neither escaped by GNU's printer.
  (let ((obj (nelisp-eln-metadata-bytecode-test--read
              "#[0 \"\\301\\300!\\205\t\x00\\302\\300!\\207\" [V0 buffer-name kill-buffer] 2]")))
    (should (byte-code-function-p obj))
    (should (equal (aref obj 0) 0))
    (should (equal (aref obj 2) [V0 buffer-name kill-buffer]))
    (should (equal (aref obj 3) 2))
    (should (equal (string-to-list (aref obj 1))
                   '(?\301 ?\300 ?! ?\205 ?\t 0 ?\302 ?\300 ?! ?\207)))))

(ert-deftest nelisp-eln-metadata-bytecode/backreference-as-code-field ()
  ;; The actual bug this module exists for: GNU's native compiler
  ;; deduplicates `equal' byte-code instruction strings across unrelated
  ;; closures in the same compilation unit, so the SAME string constant
  ;; is printed once via `#N=' and referenced again via `#N#' as another,
  ;; otherwise-different closure's own CODE field (observed verbatim in
  ;; anzu.eln, anvil-shell-filter.eln, bookmark.eln, and 387 other real
  ;; 31.1 .eln files surveyed).  A reader that hands the `#N#' occurrence
  ;; an unresolved placeholder instead of the label's already-known value
  ;; rejects this with `(invalid-read-syntax "#[")' even though it is
  ;; well-formed.
  (let* ((text "[#[257 #99=\"\\301\\300!\\207\" [V0 identity] 2] #[257 #99# [V0 car] 2]]")
         (vec (nelisp-eln-metadata-bytecode-test--read text))
         (first (aref vec 0))
         (second (aref vec 1)))
    (should (byte-code-function-p first))
    (should (byte-code-function-p second))
    (should (equal (aref first 1) (aref second 1)))
    (should (equal (aref first 2) [V0 identity]))
    (should (equal (aref second 2) [V0 car]))))

(ert-deftest nelisp-eln-metadata-bytecode/nested-byte-code-in-constants ()
  ;; `make-closure' templates commonly appear nested: the outer
  ;; function's CONSTANTS vector directly holds an inner `#[...]' (see
  ;; avl-tree.eln, dictionary-server-plain.eln).
  (let* ((text "#[257 \"\\300\\301\\302#\\207\" [V0 make-closure #[0 \"\\300\\207\" [V1] 1]] 4]")
         (outer (nelisp-eln-metadata-bytecode-test--read text)))
    (should (byte-code-function-p outer))
    (should (byte-code-function-p (aref (aref outer 2) 2)))))

(ert-deftest nelisp-eln-metadata-bytecode/hash-table-literal ()
  (let ((table (nelisp-eln-metadata-bytecode-test--read
                "#s(hash-table test eq data (a 1 b 2))")))
    (should (hash-table-p table))
    (should (eq (hash-table-test table) 'eq))
    (should (= (gethash 'a table) 1))
    (should (= (gethash 'b table) 2))))

(ert-deftest nelisp-eln-metadata-bytecode/propertized-string-and-bool-vector ()
  (should (equal (nelisp-eln-metadata-bytecode-test--read
                  "#(\"abc\" 0 1 (face bold))")
                 "abc"))
  (should (equal (nelisp-eln-metadata-bytecode-test--read "#&3\"\7\"")
                 (bool-vector t t t))))

(ert-deftest nelisp-eln-metadata-bytecode/empty-name-symbol ()
  (let ((sym (nelisp-eln-metadata-bytecode-test--read "##")))
    (should (symbolp sym))
    (should (equal (symbol-name sym) ""))))

(ert-deftest nelisp-eln-metadata-bytecode/declines-interpreted-closure ()
  ;; GNU can print an *interpreted* closure with the same `#[...]' bracket
  ;; syntax, ARGLIST a plain arg-list and CODE a cons of body forms
  ;; instead of a byte-code string (see cl-macs.eln's `fixnump'/`natnump'
  ;; helper).  This runtime has no interpreted-function object type, so
  ;; it declines explicitly rather than mis-representing it.
  (let ((condition
         (condition-case data
             (progn
               (nelisp-eln-metadata-bytecode-test--read
                "#[(x) ((ignore x) (natnump x)) nil]")
               nil)
           (unsupported-feature data))))
    (should condition)
    (should (equal (cdr condition) '(interpreted-function-byte-code)))))

(ert-deftest nelisp-eln-metadata-bytecode/rejects-garbled-byte-code-literal ()
  ;; Negative controls: a missing close bracket, and a well-typed-looking
  ;; but structurally invalid literal (CONSTANTS not a vector), must both
  ;; fail closed with a typed error rather than silently guessing.
  (should-error
   (nelisp-eln-metadata-bytecode-test--read "#[257 \"\\300\\207\" [V0]")
   :type 'nelisp-eln-metadata-bytecode-error)
  (should-error
   (nelisp-eln-metadata-bytecode-test--read "#[257 \"\\300\\207\" 99 2]")
   :type 'nelisp-eln-metadata-bytecode-error))

(ert-deftest nelisp-eln-metadata-bytecode/rejects-unsupported-and-circular-syntax ()
  (should-error (nelisp-eln-metadata-bytecode-test--read "#x10")
                :type 'nelisp-eln-metadata-bytecode-error)
  (should-error (nelisp-eln-metadata-bytecode-test--read "#1=(a . #1#)")
                :type 'nelisp-eln-metadata-bytecode-error)
  (should-error (nelisp-eln-metadata-bytecode-test--read "#7#")
                :type 'nelisp-eln-metadata-bytecode-error))

(provide 'nelisp-eln-metadata-bytecode-test)

;;; nelisp-eln-metadata-bytecode-test.el ends here
